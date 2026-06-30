test_that("assign_deltas computes per-key day-over-day rises, NA on first and reset", {
  df <- data.frame(
    chroot = c("a", "a", "a", "b", "b"),
    date   = c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-01", "2026-01-02"),
    rpms_total = c(100, 150, 140, 10, 30),
    stringsAsFactors = FALSE)
  out <- assign_deltas(df, "chroot", "rpms_total", "rpms_delta")
  out <- out[order(out$chroot, out$date), ]

  # a: first NA, +50, then a decrease (reset) -> NA
  expect_equal(out$rpms_delta[out$chroot == "a"], c(NA, 50L, NA))
  # b: first NA, +20
  expect_equal(out$rpms_delta[out$chroot == "b"], c(NA, 20L))
})

test_that("assign_deltas handles empty input and adds the delta column", {
  e <- data.frame(chroot = character(0), date = character(0), rpms_total = integer(0),
                  stringsAsFactors = FALSE)
  out <- assign_deltas(e, "chroot", "rpms_total", "rpms_delta")
  expect_equal(nrow(out), 0L)
  expect_true("rpms_delta" %in% names(out))
})

test_that("snapshot_frames dedups repo by release and drops NA totals", {
  parsed <- rbind(
    parsed_row("fedora-42", "x86_64",  1000, 5),
    parsed_row("fedora-42", "aarch64",  800, 5),  # same release; repo count repeats
    parsed_row("fedora-43", "x86_64",     NA, 7)) # NA rpm count -> dropped from rpm frame
  sf <- snapshot_frames(parsed, "2026-06-10")

  expect_equal(nrow(sf$rpm), 2L)
  expect_setequal(sf$rpm$chroot, c("fedora-42-x86_64", "fedora-42-aarch64"))
  expect_true(all(sf$rpm$date == "2026-06-10"))

  expect_equal(nrow(sf$repo), 2L)               # one row per release
  expect_setequal(sf$repo$release, c("fedora-42", "fedora-43"))
})

test_that("export_shard round-trips both daily tables", {
  rpm  <- data.frame(chroot = "fedora-42-x86_64", date = "2026-06-10",
                     rpms_total = 1000L, rpms_delta = NA_integer_, stringsAsFactors = FALSE)
  repo <- data.frame(release = "fedora-42", date = "2026-06-10",
                     repo_total = 5L, repo_delta = NA_integer_, stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".db")
  export_shard(p, rpm, repo)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM copr_downloads_daily")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM copr_repo_downloads_daily")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT rpms_total FROM copr_downloads_daily")$rpms_total, 1000L)
})

test_that("build_summary computes windows, totals, and ranks", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  rows <- data.frame(
    chroot = c(rep("fedora-42-x86_64", 3), rep("fedora-43-x86_64", 3)),
    date   = rep(c("2026-06-08", "2026-06-09", "2026-06-10"), 2),
    rpms_total = c(1000, 1100, 1250, 50, 60, 90),
    stringsAsFactors = FALSE)
  rows <- assign_deltas(rows, "chroot", "rpms_total", "rpms_delta")
  DBI::dbWriteTable(con, "copr_downloads_daily",
    rows[c("chroot", "date", "rpms_total", "rpms_delta")])

  s <- build_summary(con, "2026-06-10")
  expect_setequal(s$chroot, c("fedora-42-x86_64", "fedora-43-x86_64"))
  f42 <- s[s$chroot == "fedora-42-x86_64", ]
  expect_equal(f42$dl_7d, 250)        # deltas 100 + 150
  expect_equal(f42$rpms_total, 1250)
  expect_equal(f42$release, "fedora-42")
  expect_equal(f42$arch, "x86_64")
  expect_equal(f42$rank_30d, 1L)      # 250 downloads vs 40 for fedora-43
  expect_setequal(names(s), SUMMARY_COLS)
})
