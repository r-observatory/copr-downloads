# scripts/helpers.R: pure functions used by update.R, unit-tested in tests/testthat/.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Empty parsed-overview frame with the canonical columns.
empty_overview <- function() data.frame(
  release = character(0), arch = character(0), chroot = character(0),
  rpms_total = integer(0), repo_total = integer(0), stringsAsFactors = FALSE)

# Parse the Copr project overview HTML into one row per chroot. Each chroot row
# in the page carries: a ".repo" download link (whose URL contains the release,
# e.g. "fedora-41"), an architecture cell with the cumulative RPM-download
# counter rendered as "(N)*", and a per-release ".repo" fetch counter rendered as
# "(N downloads)". Numbers are addressed structurally (by the "*"/"downloads"
# markers), not positionally, so a layout shuffle cannot silently misread them.
parse_overview_html <- function(html) {
  doc <- xml2::read_html(html)
  rows <- xml2::xml_find_all(
    doc, "//tr[.//a[contains(@href, '/repo/') and contains(@href, '.repo')]]")
  if (length(rows) == 0) return(empty_overview())

  num <- function(x) {
    d <- gsub("[^0-9]", "", x)
    if (!nzchar(d)) NA_integer_ else as.integer(d)
  }

  parts <- lapply(rows, function(r) {
    a    <- xml2::xml_find_first(r, ".//a[contains(@href, '/repo/')]")
    href <- xml2::xml_attr(a, "href")
    release <- sub(".*/repo/([^/]+)/.*", "\\1", href)
    if (is.na(release) || !nzchar(release)) return(NULL)

    smalls <- trimws(xml2::xml_text(xml2::xml_find_all(r, ".//small")))
    repo_small <- smalls[grepl("download", smalls, ignore.case = TRUE)]  # " (0 downloads) "
    repo_total <- if (length(repo_small)) num(repo_small[1]) else NA_integer_

    # The architectures cell holds one "<arch> (N)*" RPM counter per enabled arch
    # (e.g. "x86_64 (1386842)*", or "x86_64 (111)* aarch64 (222)*" if more than one
    # is built). Extract every pair so a multi-arch release yields one row each.
    arch_td  <- xml2::xml_find_first(r, ".//td[.//small[contains(., '*')]]")
    arch_txt <- if (length(arch_td)) gsub("\\s+", " ", trimws(xml2::xml_text(arch_td))) else ""
    pieces   <- regmatches(arch_txt,
      gregexpr("[A-Za-z0-9_]+\\s*\\([0-9,]+\\)\\*", arch_txt, perl = TRUE))[[1]]
    if (length(pieces) == 0) return(NULL)

    do.call(rbind, lapply(pieces, function(piece) {
      arch <- trimws(sub("\\s*\\([0-9,]+\\)\\*$", "", piece))
      if (!nzchar(arch)) arch <- "unknown"
      cnt  <- num(sub(".*\\(([0-9,]+)\\)\\*$", "\\1", piece))
      data.frame(release = release, arch = arch,
                 chroot = paste0(release, "-", arch),
                 rpms_total = cnt, repo_total = repo_total,
                 stringsAsFactors = FALSE)
    }))
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) return(empty_overview())
  out <- do.call(rbind, parts)
  out <- out[order(out$chroot), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Split a parsed overview snapshot taken on `date` into the two snapshot frames:
# per-chroot RPM totals and per-release .repo totals (deduplicated by release,
# since the .repo file is per release and repeats across that release's chroots).
snapshot_frames <- function(parsed, date) {
  if (nrow(parsed) == 0) {
    return(list(
      rpm  = data.frame(chroot = character(0), date = character(0),
                        rpms_total = integer(0), stringsAsFactors = FALSE),
      repo = data.frame(release = character(0), date = character(0),
                        repo_total = integer(0), stringsAsFactors = FALSE)))
  }
  rpm <- data.frame(chroot = parsed$chroot, date = date,
                    rpms_total = parsed$rpms_total, stringsAsFactors = FALSE)
  rpm <- rpm[!is.na(rpm$rpms_total), , drop = FALSE]

  rel <- parsed[!duplicated(parsed$release), c("release", "repo_total")]
  repo <- data.frame(release = rel$release, date = date,
                     repo_total = rel$repo_total, stringsAsFactors = FALSE)
  repo <- repo[!is.na(repo$repo_total), , drop = FALSE]
  rownames(rpm) <- NULL; rownames(repo) <- NULL
  list(rpm = rpm, repo = repo)
}

# Recompute the per-day delta column for a cumulative-counter frame: delta is the
# rise in `total_col` since the previous date within each `key_col` group. The
# first observation of a key has no delta (NA), and a decrease (a Copr counter
# reset or a chroot being rebuilt) is recorded as NA rather than a negative, so
# downstream sums never see a phantom drop. Operates on the full history so the
# column is always self-consistent regardless of which rows were just appended.
assign_deltas <- function(df, key_col, total_col, delta_col = "delta") {
  if (nrow(df) == 0) { df[[delta_col]] <- integer(0); return(df) }
  df <- df[order(df[[key_col]], df$date), , drop = FALSE]
  keys <- df[[key_col]]; tot <- as.numeric(df[[total_col]])
  delta <- rep(NA_integer_, nrow(df))
  for (i in seq_len(nrow(df))) {
    if (i > 1L && identical(keys[i], keys[i - 1L])) {
      d <- tot[i] - tot[i - 1L]
      if (!is.na(d) && d >= 0) delta[i] <- as.integer(d)
    }
  }
  df[[delta_col]] <- delta
  rownames(df) <- NULL
  df
}

# Write the two daily tables for one year shard. rpm_df has columns
# (chroot, date, rpms_total, rpms_delta); repo_df has (release, date, repo_total,
# repo_delta). Uses the published-shard PRAGMA (no WAL) and VACUUMs.
export_shard <- function(path, rpm_df, repo_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")

  DBI::dbExecute(con, "
    CREATE TABLE copr_downloads_daily (
      chroot     TEXT    NOT NULL,
      date       TEXT    NOT NULL,
      rpms_total INTEGER NOT NULL,
      rpms_delta INTEGER,
      PRIMARY KEY (chroot, date))")
  DBI::dbExecute(con, "CREATE INDEX idx_cdd_date ON copr_downloads_daily(date)")
  if (nrow(rpm_df) > 0) {
    DBI::dbWriteTable(con, "copr_downloads_daily",
      rpm_df[c("chroot", "date", "rpms_total", "rpms_delta")], append = TRUE)
  }

  DBI::dbExecute(con, "
    CREATE TABLE copr_repo_downloads_daily (
      release    TEXT    NOT NULL,
      date       TEXT    NOT NULL,
      repo_total INTEGER NOT NULL,
      repo_delta INTEGER,
      PRIMARY KEY (release, date))")
  DBI::dbExecute(con, "CREATE INDEX idx_crd_date ON copr_repo_downloads_daily(date)")
  if (nrow(repo_df) > 0) {
    DBI::dbWriteTable(con, "copr_repo_downloads_daily",
      repo_df[c("release", "date", "repo_total", "repo_delta")], append = TRUE)
  }

  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

# Write a minimal SQLite file containing only the summary table (for the merger).
export_summary_shard <- function(path, summary) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
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
  if (nrow(summary) > 0) DBI::dbWriteTable(con, "copr_downloads_summary", summary, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

# Build the per-chroot summary from copr_downloads_daily as of `today` (the latest
# snapshot date). rpms_total is the most recent cumulative counter; dl_7d/30d/90d
# are sums of the daily deltas in those trailing windows; trend compares the last
# 30 days of downloads to the prior 30; rank_30d ranks chroots by dl_30d. Release
# and arch are derived from the chroot id. Returns a data frame with the exact
# columns and order of the summary schema.
SUMMARY_COLS <- c("chroot", "release", "arch", "rpms_total", "dl_7d", "dl_30d",
                  "dl_90d", "avg_daily_30d", "rank_30d", "trend",
                  "first_date", "last_date")

build_summary <- function(con, today) {
  a <- format(as.Date(today), "%Y-%m-%d")
  df <- DBI::dbGetQuery(con, sprintf("
    WITH agg AS (
      SELECT chroot,
        MIN(date) AS first_date,
        MAX(date) AS last_date,
        COALESCE(SUM(CASE WHEN date > date('%1$s','-7 days')  THEN rpms_delta END), 0) AS dl_7d,
        COALESCE(SUM(CASE WHEN date > date('%1$s','-30 days') THEN rpms_delta END), 0) AS dl_30d,
        COALESCE(SUM(CASE WHEN date > date('%1$s','-90 days') THEN rpms_delta END), 0) AS dl_90d,
        COALESCE(SUM(CASE WHEN date > date('%1$s','-60 days')
                           AND date <= date('%1$s','-30 days') THEN rpms_delta END), 0) AS prev_30d
      FROM copr_downloads_daily
      GROUP BY chroot)
    SELECT a.chroot,
           a.first_date, a.last_date, a.dl_7d, a.dl_30d, a.dl_90d,
           ROUND(a.dl_30d / 30.0, 2) AS avg_daily_30d,
           CASE WHEN a.prev_30d > 0
                THEN ROUND((a.dl_30d * 1.0 / a.prev_30d - 1.0) * 100.0, 2)
                ELSE NULL END AS trend,
           l.rpms_total
      FROM agg a
      JOIN copr_downloads_daily l
        ON l.chroot = a.chroot AND l.date = a.last_date", a))
  if (nrow(df) == 0) {
    empty <- as.data.frame(setNames(
      lapply(SUMMARY_COLS, function(x) switch(x,
        chroot = , release = , arch = , first_date = , last_date = character(0),
        avg_daily_30d = , trend = numeric(0), integer(0))), SUMMARY_COLS),
      stringsAsFactors = FALSE)
    return(empty)
  }
  df$release  <- sub("-[^-]*$", "", df$chroot)
  df$arch     <- sub(".*-", "", df$chroot)
  df$rank_30d <- rank(-df$dl_30d, ties.method = "min")
  df[SUMMARY_COLS]
}

# Trailing-window rows for both daily tables, for copr-downloads-recent.db.
extract_recent <- function(con, today, window_days) {
  cutoff <- format(as.Date(today) - as.integer(window_days), "%Y-%m-%d")
  list(
    rpm = DBI::dbGetQuery(con, sprintf("
      SELECT chroot, date, rpms_total, rpms_delta FROM copr_downloads_daily
       WHERE date >= '%s' ORDER BY chroot, date", cutoff)),
    repo = DBI::dbGetQuery(con, sprintf("
      SELECT release, date, repo_total, repo_delta FROM copr_repo_downloads_daily
       WHERE date >= '%s' ORDER BY release, date", cutoff)))
}

# All rows for a calendar year across both daily tables.
extract_year <- function(con, year) {
  yp <- sprintf("%04d", as.integer(year))
  list(
    rpm = DBI::dbGetQuery(con, sprintf("
      SELECT chroot, date, rpms_total, rpms_delta FROM copr_downloads_daily
       WHERE substr(date,1,4) = '%s' ORDER BY chroot, date", yp)),
    repo = DBI::dbGetQuery(con, sprintf("
      SELECT release, date, repo_total, repo_delta FROM copr_repo_downloads_daily
       WHERE substr(date,1,4) = '%s' ORDER BY release, date", yp)))
}

# Per-shard coverage descriptor for the manifest.
coverage <- function(rpm_rows) {
  if (nrow(rpm_rows) == 0) return(list(rows = 0L, date_min = NA, date_max = NA))
  list(rows = nrow(rpm_rows), date_min = min(rpm_rows$date), date_max = max(rpm_rows$date))
}

# Carry forward the per-shard coverage map, overwriting rebuilt shards.
merge_shard_coverage <- function(prev, updates) {
  out <- prev %||% list()
  for (k in names(updates)) out[[k]] <- updates[[k]]
  out
}

#' Compute the lowercase hex SHA-256 of a file's exact on-disk bytes.
#'
#' Uses whatever the runner already provides, in preference order:
#'   1. digest  package        (if installed)
#'   2. openssl package        (if installed)
#'   3. sha256sum (coreutils)  - present on the ubuntu-latest CI runner
#'   4. shasum -a 256 (BSD)    - macOS/local fallback
#' No heavy dependency is declared: on CI (which installs only RSQLite,
#' jsonlite, testthat, DBI) the coreutils `sha256sum` path is used. If a
#' sibling pipeline already declares `digest`, that path wins automatically.
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(file = path, algo = "sha256")))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(tolower(as.character(openssl::sha256(con))))
  }
  sha_tool <- Sys.which("sha256sum")
  if (nzchar(sha_tool)) {
    out <- system2(sha_tool, shQuote(path), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  shasum_tool <- Sys.which("shasum")
  if (nzchar(shasum_tool)) {
    out <- system2(shasum_tool, c("-a", "256", shQuote(path)), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  stop("No SHA-256 backend found (need one of: digest, openssl, sha256sum, shasum)")
}

#' Build the integrity / completeness core describing a finalized SQLite file.
#'
#' Returns a named list of TOP-LEVEL manifest fields computed from the exact
#' on-disk bytes of `db_path` (call this only after the file is finalized):
#'   * db_filename - basename of the file
#'   * db_bytes    - byte size of the file as a double. Deliberately NOT cast
#'                   to integer: R's integer range is 32-bit and overflows to
#'                   NA (serialized as the string "NA") for files >= ~2 GiB.
#'   * db_sha256   - lowercase hex sha256 of the file's exact bytes
#'   * tables      - named list mapping each user table to its row count
#'   * complete    - passed through by the caller. complete = the DB holds the
#'                   full, non-partial dataset (a full rebuild each run);
#'                   freshness is tracked separately via generated_at and the
#'                   fingerprint. A pipeline with a genuine partial/bootstrap
#'                   state would derive this instead of hardcoding it.
#' Lets a downstream merge content-verify the asset it pulls and confirm the
#' expected tables/rows are present.
summary_integrity_core <- function(db_path, complete = TRUE) {
  stopifnot(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tables <- tryCatch({
    tbl_names <- DBI::dbGetQuery(con, "
      SELECT name FROM sqlite_master
       WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
       ORDER BY name")$name

    stats::setNames(
      lapply(tbl_names, function(t) {
        DBI::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
      }),
      tbl_names
    )
  }, finally = DBI::dbDisconnect(con))

  # db_bytes/db_sha256 read the raw on-disk file only after the connection
  # above is closed, so no open handle or journal file skews the hash/size.
  list(
    db_filename = basename(db_path),
    db_bytes    = file.size(db_path),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
}

# Write the manifest object as pretty JSON, preserving nulls and empty arrays.
# `core` (optional) is a named list of TOP-LEVEL fields to merge into the
# manifest - used to attach the integrity/completeness core built by
# summary_integrity_core() (db_filename, db_bytes, db_sha256, tables, complete).
write_manifest <- function(path, obj, core = NULL) {
  if (!is.null(core)) {
    obj <- c(obj, core)  # merge as top-level fields, not nested
  }
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null"), path)
}

# Render the GitHub release body (markdown) from a manifest object.
write_release_notes <- function(path, manifest) {
  ts  <- function(s) if (is.null(s) || is.na(s)) "n/a" else sub("Z$", " UTC", sub("T", " ", s))
  big <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "0" else
    formatC(as.numeric(x), format = "d", big.mark = ",")
  cs <- manifest$changed_shards
  changed <- if (length(cs) == 0) "none (source unreachable this run)" else
    paste(unlist(cs), collapse = ", ")

  lines <- c(
    "Per-chroot download statistics for the [iucar/cran](https://copr.fedorainfracloud.org/coprs/iucar/cran/) Fedora Copr project (CRAN packages built as RPMs by cran2copr). Copr exposes no per-package download counts; the finest available figure is the cumulative RPM-download counter per chroot (Fedora release plus architecture), which this pipeline snapshots daily and differences into per-day counts. See the [README](https://github.com/r-observatory/copr-downloads#readme) for the caveats.",
    "",
    "This is a single rolling release. Assets are SQLite shards: per-year archives (`copr-downloads-YYYY.db`), a rolling 400-day window (`copr-downloads-recent.db`), and a summary-only file (`copr-downloads-summary.db`), alongside `manifest.json`. Each run replaces only the shards that changed.",
    "",
    "| | |",
    "|---|---|",
    sprintf("| **Last checked** | %s |", ts(manifest$last_checked)),
    sprintf("| **Source this run** | %s |", manifest$source_kind %||% "n/a"),
    sprintf("| **Latest snapshot** | %s |", manifest$summary$latest_date %||% "n/a"),
    sprintf("| **Chroots tracked** | %s |", manifest$summary$chroots %||% "n/a"),
    sprintf("| **Changed this run** | %s |", changed),
    "",
    "## Shard coverage",
    "",
    "| Shard | Rows | From | To |",
    "|---|---:|---|---|")
  shards <- manifest$shards %||% list()
  for (nm in sort(names(shards))) {
    s <- shards[[nm]]
    lines <- c(lines, sprintf("| `%s` | %s | %s | %s |",
      nm, big(s$rows), s$date_min %||% "n/a", s$date_max %||% "n/a"))
  }
  lines <- c(lines, "",
    "_Fetch the rolling window:_",
    "```bash",
    "gh release download current --repo r-observatory/copr-downloads --pattern copr-downloads-recent.db",
    "```")
  writeLines(lines, path)
  invisible(NULL)
}
