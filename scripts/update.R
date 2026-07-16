#!/usr/bin/env Rscript
# scripts/update.R: copr-downloads producer.
#
# Every run fetches the iucar/cran Copr overview page, parses the per-chroot
# cumulative RPM-download counters and per-release .repo fetch counters, appends
# today's snapshot to the history pulled from the rolling "current" release, and
# re-exports the affected year shard plus the recent and summary shards. The
# cumulative counters are differenced into per-day deltas. When the overview is
# unreachable the run is a cheap heartbeat that leaves the prior release intact.
# run_update(io, out_dir) takes an injectable io for offline testing.

options(timeout = 300)

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(jsonlite)
})

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f))
  NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("parse_overview_html", mode = "function")) {
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "helpers.R"))
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Drop and rewrite the summary table inside an existing shard (the recent shard
# carries the summary so a single download answers most queries).
embed_summary <- function(recent_path, summary_df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS copr_downloads_summary")
  DBI::dbExecute(con, "
    CREATE TABLE copr_downloads_summary (
      chroot        TEXT PRIMARY KEY,
      release       TEXT,
      arch          TEXT,
      rpms_total    INTEGER,
      dl_7d         INTEGER,
      dl_30d        INTEGER,
      dl_90d        INTEGER,
      avg_daily_30d REAL,
      rank_30d      INTEGER,
      trend         REAL,
      first_date    TEXT,
      last_date     TEXT)")
  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "copr_downloads_summary", summary_df, append = TRUE)
}

run_update <- function(io, out_dir, force_full = FALSE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(out_dir, "manifest.json")
  recent_path   <- file.path(out_dir, "copr-downloads-recent.db")

  if (io$release_exists()) {
    mcode <- io$release_download("manifest.json", out_dir)
    rcode <- io$release_download("copr-downloads-recent.db", out_dir)
    # A release exists, so the prior history must be loaded before we rebuild and
    # clobber-upload. If either download fails, abort rather than silently treat
    # this as a cold start and overwrite the accumulated series with one day.
    if (!identical(as.integer(mcode), 0L) || !file.exists(manifest_path) ||
        !identical(as.integer(rcode), 0L) || !file.exists(recent_path)) {
      stop("release 'current' exists but its manifest/recent shard could not be ",
           "downloaded; aborting to protect accumulated history")
    }
  }
  prev <- if (file.exists(manifest_path))
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE) else list()
  prev_shards <- prev$shards %||% list()

  now       <- io$now()
  today     <- as.Date(format(now, "%Y-%m-%d", tz = "UTC"))
  today_str <- format(today, "%Y-%m-%d")

  parsed <- tryCatch(io$fetch_overview(), error = function(e) NULL)

  heartbeat <- function() {
    out <- if (length(prev) > 0) prev else list()
    out$last_checked   <- iso(now)
    out$source_kind    <- "frozen"
    out$changed_shards <- list()
    write_manifest(manifest_path, out)
    write_release_notes(file.path(out_dir, "release_notes.md"), out)
    list(changed_shards = character(0), manifest = out)
  }

  if (is.null(parsed) || nrow(parsed) == 0) {
    if (length(prev) == 0)
      stop("Copr overview unreachable and no prior release exists; cannot bootstrap")
    return(heartbeat())
  }

  # Seed history (cumulative totals only; deltas are always recomputed) from the
  # prior recent shard, plus every year shard when forcing a full rebuild.
  rpm_hist  <- data.frame(chroot = character(0), date = character(0),
                          rpms_total = integer(0), stringsAsFactors = FALSE)
  repo_hist <- data.frame(release = character(0), date = character(0),
                          repo_total = integer(0), stringsAsFactors = FALSE)
  load_prior <- function(path) {
    if (!file.exists(path)) return(invisible())
    c2 <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(c2), add = TRUE)
    tabs <- DBI::dbListTables(c2)
    if ("copr_downloads_daily" %in% tabs)
      rpm_hist <<- rbind(rpm_hist,
        DBI::dbGetQuery(c2, "SELECT chroot, date, rpms_total FROM copr_downloads_daily"))
    if ("copr_repo_downloads_daily" %in% tabs)
      repo_hist <<- rbind(repo_hist,
        DBI::dbGetQuery(c2, "SELECT release, date, repo_total FROM copr_repo_downloads_daily"))
  }
  load_prior(recent_path)
  if (isTRUE(force_full)) {
    for (nm in names(prev_shards)) {
      if (grepl("^copr-downloads-[0-9]{4}\\.db$", nm)) {
        io$release_download(nm, out_dir)
        load_prior(file.path(out_dir, nm))
      }
    }
  }

  # Append today's snapshot (idempotent: replace any existing rows for today).
  snap      <- snapshot_frames(parsed, today_str)
  rpm_hist  <- rpm_hist[rpm_hist$date != today_str, , drop = FALSE]
  repo_hist <- repo_hist[repo_hist$date != today_str, , drop = FALSE]
  rpm_all   <- rbind(rpm_hist,  snap$rpm)
  repo_all  <- rbind(repo_hist, snap$repo)
  rpm_all   <- rpm_all[!duplicated(rpm_all[c("chroot", "date")], fromLast = TRUE), , drop = FALSE]
  repo_all  <- repo_all[!duplicated(repo_all[c("release", "date")], fromLast = TRUE), , drop = FALSE]

  rpm_all  <- assign_deltas(rpm_all,  "chroot",  "rpms_total", "rpms_delta")
  repo_all <- assign_deltas(repo_all, "release", "repo_total", "repo_delta")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "copr_downloads_daily",
    rpm_all[c("chroot", "date", "rpms_total", "rpms_delta")])
  summary_df <- build_summary(con, today_str)

  years <- if (isTRUE(force_full)) sort(unique(substr(rpm_all$date, 1, 4))) else format(today, "%Y")
  changed_shards <- character(0); shard_updates <- list()
  for (yr in years) {
    shard <- sprintf("copr-downloads-%s.db", yr)
    ry <- rpm_all[substr(rpm_all$date, 1, 4) == yr, , drop = FALSE]
    py <- repo_all[substr(repo_all$date, 1, 4) == yr, , drop = FALSE]
    export_shard(file.path(out_dir, shard), ry, py)
    changed_shards <- c(changed_shards, shard)
    shard_updates[[shard]] <- coverage(ry)
  }

  win_cut <- format(today - RECENT_WINDOW_DAYS, "%Y-%m-%d")
  r_rpm   <- rpm_all[rpm_all$date >= win_cut, , drop = FALSE]
  r_repo  <- repo_all[repo_all$date >= win_cut, , drop = FALSE]
  export_shard(recent_path, r_rpm, r_repo)
  embed_summary(recent_path, summary_df)
  summary_path <- file.path(out_dir, "copr-downloads-summary.db")
  export_summary_shard(summary_path, summary_df)
  changed_shards <- c(changed_shards, "copr-downloads-recent.db", "copr-downloads-summary.db")
  shard_updates[["copr-downloads-recent.db"]] <- coverage(r_rpm)

  # Integrity / completeness core for the summary DB the downstream merge pulls.
  # Computed from the finalized on-disk copr-downloads-summary.db (written just
  # above) so db_bytes/db_sha256 describe the exact bytes uploaded to the release.
  # The summary is a full teardown-and-rebuild each run (build_summary) over the
  # always-loaded rolling recent window (RECENT_WINDOW_DAYS = 400); every summary
  # download metric spans at most 90 days, well inside that window, so it is a
  # complete snapshot: complete = TRUE.
  integrity_core <- summary_integrity_core(summary_path, complete = TRUE)

  out <- list(
    tag            = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at   = iso(now),
    last_checked   = iso(now),
    last_changed   = iso(now),
    source_kind    = "overview",
    source_url     = COPR_OVERVIEW_URL,
    granularities  = list("daily"),
    changed_shards = as.list(changed_shards),
    shards         = merge_shard_coverage(prev_shards, shard_updates),
    summary        = list(
      chroots     = nrow(summary_df),
      latest_date = today_str,
      rpms_total  = if (nrow(summary_df)) sum(summary_df$rpms_total, na.rm = TRUE) else 0))
  write_manifest(manifest_path, out, core = integrity_core)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  list(changed_shards = changed_shards, manifest = out)
}

with_retry <- function(expr, tries = 3L, wait = 3) {
  for (i in seq_len(tries)) {
    val <- tryCatch(force(expr), error = function(e) e)
    if (!inherits(val, "error")) return(val)
    if (i < tries) Sys.sleep(wait * i)
  }
  stop(val)
}

default_io <- function() {
  list(
    release_exists = function() {
      st <- suppressWarnings(system2("gh",
        c("release", "view", "current", "--repo", PUBLISH_REPO),
        stdout = FALSE, stderr = FALSE))
      identical(as.integer(st), 0L)
    },
    release_download = function(pattern, dir) {
      for (i in seq_len(3L)) {
        st <- suppressWarnings(system2("gh",
          c("release", "download", "current", "--repo", PUBLISH_REPO,
            "--pattern", pattern, "--dir", dir, "--clobber"),
          stdout = TRUE, stderr = TRUE))
        code <- as.integer(attr(st, "status") %||% 0L)
        if (identical(code, 0L)) return(0L)
        if (i < 3L) Sys.sleep(3 * i)
      }
      code
    },
    fetch_overview = function() {
      h <- curl::new_handle(useragent = COPR_USER_AGENT,
                            followlocation = TRUE, timeout = 120L)
      resp <- with_retry(curl::curl_fetch_memory(COPR_OVERVIEW_URL, handle = h))
      if (resp$status_code != 200L) {
        message("Copr overview HTTP ", resp$status_code)
        return(NULL)
      }
      html   <- rawToChar(resp$content)
      parsed <- parse_overview_html(html)
      if (nrow(parsed) == 0) {
        message("Copr overview returned no chroot rows ",
                "(possible anti-bot challenge); treating as unreachable")
        return(NULL)
      }
      parsed
    },
    now = function() Sys.time())
}

if (sys.nframe() == 0L) {
  args       <- commandArgs(trailingOnly = TRUE)
  out_dir    <- if (length(args) >= 1) args[1] else "out"
  force_full <- tolower(Sys.getenv("COPR_FORCE_REBUILD", "")) %in% c("true", "1", "yes")
  res <- run_update(default_io(), out_dir, force_full = force_full)
  cat("Changed shards:", if (length(res$changed_shards))
        paste(res$changed_shards, collapse = ", ") else "(none)", "\n")
}
