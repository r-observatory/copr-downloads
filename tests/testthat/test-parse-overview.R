test_that("parse_overview_html extracts per-chroot counters from the live fixture", {
  html <- paste(readLines(fixture_path("overview-iucar-cran.html"), warn = FALSE),
                collapse = "\n")
  df <- parse_overview_html(html)

  expect_true(nrow(df) >= 5)
  expect_setequal(names(df), c("release", "arch", "chroot", "rpms_total", "repo_total"))
  expect_true(all(grepl("^fedora-", df$release)))
  expect_true(all(df$arch == "x86_64"))             # only x86_64 chroots are enabled
  expect_true(all(df$chroot == paste0(df$release, "-", df$arch)))
  expect_true(all(df$rpms_total > 0))

  # Exact values captured in the fixture.
  f42 <- df[df$chroot == "fedora-42-x86_64", ]
  expect_equal(nrow(f42), 1L)
  expect_equal(f42$rpms_total, 1818045L)

  f43 <- df[df$chroot == "fedora-43-x86_64", ]
  expect_equal(f43$rpms_total, 1281117L)
  expect_equal(f43$repo_total, 4304L)
})

test_that("parse_overview_html returns an empty frame for a challenge/blank page", {
  expect_equal(nrow(parse_overview_html("<html><body>Access denied</body></html>")), 0L)
})

test_that("parse_overview_html emits one row per arch when a release has several", {
  html <- '<table><tbody><tr>
    <td>Fedora 42</td>
    <td> x86_64 <small> (1818045)*</small> aarch64 <small> (50000)*</small> </td>
    <td class="rightmost">
      <a href="https://x/coprs/iucar/cran/repo/fedora-42/iucar-cran-fedora-42.repo">F42</a>
      <small> (4304 downloads) </small>
    </td>
  </tr></tbody></table>'
  df <- parse_overview_html(html)
  expect_equal(nrow(df), 2L)
  expect_setequal(df$chroot, c("fedora-42-x86_64", "fedora-42-aarch64"))
  expect_equal(df$rpms_total[df$arch == "x86_64"], 1818045L)
  expect_equal(df$rpms_total[df$arch == "aarch64"], 50000L)
  expect_true(all(df$repo_total == 4304L))   # .repo count is per release
})

test_that("parse_overview_html parses comma-formatted counters", {
  html <- '<table><tbody><tr>
    <td>Fedora 42</td>
    <td> x86_64 <small> (1,818,045)*</small> </td>
    <td class="rightmost">
      <a href="https://x/coprs/iucar/cran/repo/fedora-42/iucar-cran-fedora-42.repo">F42</a>
      <small> (4,304 downloads) </small>
    </td>
  </tr></tbody></table>'
  df <- parse_overview_html(html)
  expect_equal(df$rpms_total, 1818045L)
  expect_equal(df$repo_total, 4304L)
})
