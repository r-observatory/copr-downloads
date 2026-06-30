# scripts/config.R: pipeline constants (sourced by helpers.R consumers and update.R).

# The Fedora Copr project being tracked. cran2copr builds most of CRAN as RPMs
# into iucar/cran (owner "iucar", project "cran").
COPR_OWNER   <- "iucar"
COPR_PROJECT <- "cran"

# The project overview page is server-rendered HTML that carries the per-chroot
# cumulative RPM download counters (shown as "(N)*") and the per-release .repo
# fetch counters (shown as "(N downloads)"). Copr exposes no download-stats API,
# so this page is the lightest public source of the numbers. A plain non-browser
# User-Agent passes the Anubis anti-scraper gate that challenges browsers.
COPR_OVERVIEW_URL <- sprintf(
  "https://copr.fedorainfracloud.org/coprs/%s/%s/", COPR_OWNER, COPR_PROJECT)

# A non-browser UA: Anubis only challenges browser-like requests, so an honest
# tool UA is served the page directly without a proof-of-work.
COPR_USER_AGENT <- "r-observatory-copr-downloads/1.0 (+https://github.com/r-observatory/copr-downloads)"

PUBLISH_REPO <- "r-observatory/copr-downloads"

# Rolling window carried in copr-downloads-recent.db (days of daily snapshots).
RECENT_WINDOW_DAYS <- 400L
