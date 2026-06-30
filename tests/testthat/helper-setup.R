# Auto-sourced by testthat before tests run. Sources the pipeline code so tests
# can call the helpers and the orchestrator directly. During test_dir() the
# working directory is tests/testthat, so the repo root is two levels up.
.copr_root <- normalizePath(file.path(getwd(), "..", ".."))

source(file.path(.copr_root, "scripts", "config.R"))
source(file.path(.copr_root, "scripts", "helpers.R"))

.copr_update <- file.path(.copr_root, "scripts", "update.R")
if (file.exists(.copr_update)) source(.copr_update)

fixture_path <- function(...) {
  file.path(.copr_root, "tests", "testthat", "fixtures", ...)
}

# Build one parsed-overview row as parse_overview_html would emit it. Shared
# across test files (top-level defs in a test-*.R file are not visible elsewhere).
parsed_row <- function(release, arch, rpms_total, repo_total) {
  data.frame(release = release, arch = arch,
             chroot = paste0(release, "-", arch),
             rpms_total = as.integer(rpms_total),
             repo_total = as.integer(repo_total),
             stringsAsFactors = FALSE)
}
