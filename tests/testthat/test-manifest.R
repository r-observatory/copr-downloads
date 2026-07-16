# Integrity / completeness core for the published manifest.json. Mirrors the
# proven cran-downloads pattern: summary_integrity_core() describes the exact
# on-disk bytes of the summary DB the downstream merge pulls, and write_manifest()
# merges that core into the manifest as top-level fields.

# Build a tiny, real summary DB on disk (canonical schema via export_summary_shard).
build_copr_summary_db <- function(n = 3L) {
  tmp <- tempfile(fileext = ".db")
  export_summary_shard(path = tmp, summary = data.frame(
    chroot        = paste0("fedora-4", seq_len(n), "-x86_64"),
    release       = paste0("fedora-4", seq_len(n)),
    arch          = rep("x86_64", n),
    rpms_total    = seq_len(n) * 1000L,
    dl_7d         = seq_len(n) * 7L,
    dl_30d        = seq_len(n) * 30L,
    dl_90d        = seq_len(n) * 90L,
    avg_daily_30d = seq_len(n) * 1.0,
    rank_30d      = seq_len(n),
    trend         = rep(NA_real_, n),
    first_date    = rep("2026-01-01", n),
    last_date     = rep("2026-06-10", n),
    stringsAsFactors = FALSE
  ))
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_copr_summary_db(3L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  expect_equal(core$db_bytes, as.integer(file.size(db)))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count
  expect_equal(core$tables, list(copr_downloads_summary = 3L))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  skip_if_not_installed("digest")
  db <- build_copr_summary_db(2L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db)
  independent <- tolower(digest::digest(file = db, algo = "sha256"))
  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest merges the integrity core as top-level fields", {
  db <- build_copr_summary_db(4L)
  on.exit(unlink(db), add = TRUE)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_manifest(
    path = tmp,
    obj  = list(
      tag            = "v20260714-000000",
      changed_shards = list("copr-downloads-summary.db"),
      summary        = list(chroots = 4L)),
    core = core)

  parsed <- jsonlite::fromJSON(tmp)
  # existing fields preserved
  expect_equal(parsed$tag, "v20260714-000000")
  expect_equal(parsed$summary$chroots, 4L)
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, as.integer(file.size(db)))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$copr_downloads_summary, 4L)
  expect_true(parsed$complete)
})
