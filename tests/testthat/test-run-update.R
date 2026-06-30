# Simulate the rolling "current" release with a local "published" directory.
publish <- function(out, pub) {
  for (f in list.files(out)) {
    if (grepl("\\.(db|json)$", f)) file.copy(file.path(out, f), file.path(pub, f), overwrite = TRUE)
  }
}

fake_io <- function(pub, parsed, now) {
  list(
    release_exists   = function() file.exists(file.path(pub, "manifest.json")),
    release_download = function(pattern, dir) {
      src <- file.path(pub, pattern)
      if (file.exists(src)) { file.copy(src, file.path(dir, pattern), overwrite = TRUE); 0L } else 1L
    },
    fetch_overview = function() parsed,
    now = function() now)
}

test_that("run_update bootstraps, then accrues per-day deltas across runs", {
  tmp <- withr::local_tempdir()
  pub <- file.path(tmp, "pub"); dir.create(pub)

  out1 <- file.path(tmp, "out1")
  p1 <- rbind(parsed_row("fedora-42", "x86_64", 1000, 5),
              parsed_row("fedora-43", "x86_64", 2000, 7))
  run_update(fake_io(pub, p1, as.POSIXct("2026-06-10 06:00:00", tz = "UTC")), out1)
  expect_true(file.exists(file.path(out1, "copr-downloads-2026.db")))
  expect_true(file.exists(file.path(out1, "copr-downloads-recent.db")))
  expect_true(file.exists(file.path(out1, "copr-downloads-summary.db")))
  publish(out1, pub)

  out2 <- file.path(tmp, "out2")
  p2 <- rbind(parsed_row("fedora-42", "x86_64", 1120, 5),
              parsed_row("fedora-43", "x86_64", 2050, 9))
  res2 <- run_update(fake_io(pub, p2, as.POSIXct("2026-06-11 06:00:00", tz = "UTC")), out2)
  expect_true("copr-downloads-2026.db" %in% res2$changed_shards)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "copr-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  d2 <- DBI::dbGetQuery(con, "SELECT * FROM copr_downloads_daily WHERE date = '2026-06-11' ORDER BY chroot")
  expect_equal(d2$rpms_delta[d2$chroot == "fedora-42-x86_64"], 120L)
  expect_equal(d2$rpms_delta[d2$chroot == "fedora-43-x86_64"], 50L)

  d1 <- DBI::dbGetQuery(con, "SELECT * FROM copr_downloads_daily WHERE date = '2026-06-10'")
  expect_true(all(is.na(d1$rpms_delta)))                # first snapshot has no delta

  d_repo <- DBI::dbGetQuery(con, "SELECT * FROM copr_repo_downloads_daily WHERE date = '2026-06-11' AND release='fedora-43'")
  expect_equal(d_repo$repo_delta, 2L)                   # 9 - 7

  s <- DBI::dbGetQuery(con, "SELECT * FROM copr_downloads_summary")
  expect_equal(nrow(s), 2L)
})

test_that("run_update heartbeats when the overview is unreachable", {
  tmp <- withr::local_tempdir()
  pub <- file.path(tmp, "pub"); dir.create(pub)
  out1 <- file.path(tmp, "out1")
  run_update(fake_io(pub, parsed_row("fedora-42", "x86_64", 1000, 5),
                     as.POSIXct("2026-06-10 06:00:00", tz = "UTC")), out1)
  publish(out1, pub)

  out2 <- file.path(tmp, "out2")
  res <- run_update(fake_io(pub, NULL, as.POSIXct("2026-06-11 06:00:00", tz = "UTC")), out2)
  expect_length(res$changed_shards, 0L)
  man <- jsonlite::fromJSON(file.path(out2, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$source_kind, "frozen")
})

test_that("run_update with no prior release and an unreachable source errors", {
  tmp <- withr::local_tempdir()
  pub <- file.path(tmp, "pub"); dir.create(pub)
  expect_error(
    run_update(fake_io(pub, NULL, as.POSIXct("2026-06-10 06:00:00", tz = "UTC")),
               file.path(tmp, "out")),
    "cannot bootstrap")
})

test_that("run_update aborts when the release exists but the recent shard cannot be downloaded", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  run_update(fake_io(pub, parsed_row("fedora-42", "x86_64", 1000, 5),
                     as.POSIXct("2026-06-10 06:00:00", tz = "UTC")), file.path(tmp, "out1"))
  publish(file.path(tmp, "out1"), pub)

  io <- fake_io(pub, parsed_row("fedora-42", "x86_64", 1100, 5),
                as.POSIXct("2026-06-11 06:00:00", tz = "UTC"))
  io$release_download <- function(pattern, dir) {           # recent download fails
    if (grepl("recent", pattern)) return(1L)
    src <- file.path(pub, pattern)
    if (file.exists(src)) { file.copy(src, file.path(dir, pattern), overwrite = TRUE); 0L } else 1L
  }
  expect_error(run_update(io, file.path(tmp, "out2")), "protect accumulated history")
})

test_that("run_update is idempotent on a same-day re-run (replaces, never duplicates)", {
  tmp <- withr::local_tempdir()
  pub <- file.path(tmp, "pub"); dir.create(pub)
  run_update(fake_io(pub, parsed_row("fedora-42", "x86_64", 1000, 5),
                     as.POSIXct("2026-06-10 06:00:00", tz = "UTC")), file.path(tmp, "o1"))
  publish(file.path(tmp, "o1"), pub)

  out2 <- file.path(tmp, "o2")
  run_update(fake_io(pub, parsed_row("fedora-42", "x86_64", 1010, 5),
                     as.POSIXct("2026-06-10 18:00:00", tz = "UTC")), out2)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "copr-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM copr_downloads_daily WHERE chroot='fedora-42-x86_64'")$n
  expect_equal(n, 1L)
  v <- DBI::dbGetQuery(con, "SELECT rpms_total FROM copr_downloads_daily WHERE date='2026-06-10'")$rpms_total
  expect_equal(v, 1010L)
})
