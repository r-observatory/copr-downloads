# COPR Downloads

Daily download statistics for the [iucar/cran](https://copr.fedorainfracloud.org/coprs/iucar/cran/) Fedora Copr project, the [cran2copr](https://github.com/cran4linux/cran2copr) repository that builds most of CRAN as RPMs for Fedora. Fedora Copr exposes no per-package download API; the finest figure it publishes is the cumulative RPM-download counter per chroot (a Fedora release plus architecture, such as `fedora-42-x86_64`), shown on the project page. This pipeline snapshots those counters once per day, differences the cumulative totals into per-day download counts, and publishes the result as SQLite shard files attached to a single rolling GitHub release tag (`current`).

> [!IMPORTANT]
> **What these numbers mean, and what they do not.**
>
> - **Per chroot, not per package.** Copr's download hitcounter records a hit against the chroot, not the individual RPM, so there is no public per-package count for this project. Every row in `copr_downloads_daily` is a Fedora release plus architecture (`fedora-42-x86_64`), aggregated over all RPMs in that chroot. This is a platform limitation, not a pipeline choice.
> - **Cumulative counters, differenced into daily counts.** The source value (`rpms_total`) is an all-time cumulative count that Copr has maintained since long before this pipeline existed. `rpms_delta` is the rise since the previous daily snapshot, that is, downloads on that day. The per-day series therefore begins on the pipeline's first run, while `rpms_total` reflects the full history.
> - **Deltas are blank on the first snapshot and on resets.** The first day a chroot is seen has no `rpms_delta` (`NULL`). If a cumulative counter ever decreases (a chroot rebuild, an EOL purge, or a Copr-side recount), that day's delta is recorded as `NULL` rather than a negative, so window sums never see a phantom drop.
> - **Counts include mirror, CDN, and bot traffic.** Copr serves RPMs through a CDN; the counter increments on redirected requests, which include re-downloads by mirrors, CI systems, containers, and crawlers. Treat the figures as relative popularity and trend signals, not as a count of distinct human installs.
> - **`.repo` fetches are a separate, noisier metric.** `copr_repo_downloads_daily` tracks how often each release's `.repo` enablement file was fetched (the `(N downloads)` figure on the project page). It is a rough proxy for new repo enablements and is far smaller and noisier than the RPM counter.
> - **Not comparable to `cran-downloads`, `r2u-downloads`, or `bioconductor-downloads`.** CRAN cranlogs are daily per-package single-mirror counts; r2u counts are apt fetches of `.deb` files; Bioconductor counts are monthly distinct-IP figures. These are CDN RPM hits aggregated per chroot. Different populations, different methods: do not compare magnitudes.

## Data Access

All shards live as assets on the [`current` release](https://github.com/r-observatory/copr-downloads/releases/tag/current). Each daily run uploads only the shards that changed; the rest remain unchanged.

### Recent data (last 400 days)

For most use cases this is the only file you need. It holds the rolling 400-day window of both daily tables plus the full `copr_downloads_summary` table.

```bash
gh release download current \
  --repo r-observatory/copr-downloads \
  --pattern "copr-downloads-recent.db"
```

```r
url <- "https://github.com/r-observatory/copr-downloads/releases/download/current/copr-downloads-recent.db"
download.file(url, "copr-downloads-recent.db", mode = "wb")

library(RSQLite)
con <- dbConnect(SQLite(), "copr-downloads-recent.db")

# Daily RPM downloads for the Fedora 42 chroot over the last 30 days
dbGetQuery(con, "
  SELECT date, rpms_delta
  FROM copr_downloads_daily
  WHERE chroot = 'fedora-42-x86_64'
    AND rpms_delta IS NOT NULL
  ORDER BY date DESC LIMIT 30
")

# Current standing across chroots
dbGetQuery(con, "
  SELECT chroot, rpms_total, dl_30d, avg_daily_30d, trend
  FROM copr_downloads_summary
  ORDER BY rank_30d
")

dbDisconnect(con)
```

```python
import urllib.request, sqlite3
url = "https://github.com/r-observatory/copr-downloads/releases/download/current/copr-downloads-recent.db"
urllib.request.urlretrieve(url, "copr-downloads-recent.db")

con = sqlite3.connect("copr-downloads-recent.db")
for row in con.execute("""
    SELECT chroot, rpms_total, dl_30d
    FROM copr_downloads_summary
    ORDER BY rank_30d"""):
    print(row)
con.close()
```

### Per-year archives

Each calendar year of snapshots has its own shard (history begins the year this pipeline launched):

```bash
gh release download current \
  --repo r-observatory/copr-downloads \
  --pattern "copr-downloads-2026.db"
```

### Full history (all years)

```bash
gh release download current \
  --repo r-observatory/copr-downloads \
  --pattern "copr-downloads-*.db"
```

### Summary only

For the current per-chroot standing with the smallest download:

```bash
gh release download current \
  --repo r-observatory/copr-downloads \
  --pattern "copr-downloads-summary.db"
```

### Manifest

`manifest.json` lists which shards changed in the most recent run, the source kind (`overview` for a live read, `frozen` for a heartbeat when the page was unreachable), per-shard coverage, and freshness timestamps.

```bash
gh release download current \
  --pattern manifest.json \
  --repo r-observatory/copr-downloads
cat manifest.json
```

## Example Queries

### Daily downloads for a chroot

```sql
SELECT date, rpms_delta
  FROM copr_downloads_daily
 WHERE chroot = 'fedora-43-x86_64'
   AND rpms_delta IS NOT NULL
 ORDER BY date DESC
 LIMIT 30;
```

### Total downloads across all chroots, by day

```sql
SELECT date, SUM(rpms_delta) AS downloads
  FROM copr_downloads_daily
 WHERE rpms_delta IS NOT NULL
 GROUP BY date
 ORDER BY date DESC
 LIMIT 30;
```

### Repo-enablement fetches per release

```sql
SELECT release, date, repo_delta
  FROM copr_repo_downloads_daily
 WHERE repo_delta IS NOT NULL
 ORDER BY date DESC, release;
```

## Schema

### `copr_downloads_daily`

One row per chroot per daily snapshot. Present in `copr-downloads-recent.db` (last 400 days) and each `copr-downloads-YYYY.db` archive.

| Column | Type | Description |
|---|---|---|
| `chroot` | TEXT | Fedora release plus architecture, e.g. `fedora-42-x86_64` (PK part 1) |
| `date` | TEXT | Snapshot date `YYYY-MM-DD`, UTC (PK part 2) |
| `rpms_total` | INTEGER | Cumulative all-time RPM downloads reported that day |
| `rpms_delta` | INTEGER | Downloads since the previous snapshot; `NULL` on the first snapshot or a counter reset |

### `copr_repo_downloads_daily`

One row per Fedora release per daily snapshot, tracking `.repo` enablement-file fetches. Present in the same shards as `copr_downloads_daily`.

| Column | Type | Description |
|---|---|---|
| `release` | TEXT | Fedora release, e.g. `fedora-42` (PK part 1) |
| `date` | TEXT | Snapshot date `YYYY-MM-DD`, UTC (PK part 2) |
| `repo_total` | INTEGER | Cumulative `.repo` fetches reported that day |
| `repo_delta` | INTEGER | `.repo` fetches since the previous snapshot; `NULL` on the first snapshot or a reset |

### `copr_downloads_summary`

Per-chroot standing, rebuilt each run and anchored to the latest snapshot date. Present in `copr-downloads-recent.db` and `copr-downloads-summary.db`.

| Column | Type | Description |
|---|---|---|
| `chroot` | TEXT | Fedora release plus architecture (PK) |
| `release` | TEXT | Fedora release derived from the chroot |
| `arch` | TEXT | Architecture derived from the chroot |
| `rpms_total` | INTEGER | Latest cumulative RPM-download counter |
| `dl_7d` | INTEGER | Downloads (summed deltas) over the trailing 7 days |
| `dl_30d` | INTEGER | Downloads over the trailing 30 days |
| `dl_90d` | INTEGER | Downloads over the trailing 90 days |
| `avg_daily_30d` | REAL | Average daily downloads over the trailing 30 days |
| `rank_30d` | INTEGER | Rank of the chroot by `dl_30d` |
| `trend` | REAL | Percent change: last 30 days vs the prior 30; `NULL` when the prior window is empty |
| `first_date` | TEXT | First snapshot date for the chroot |
| `last_date` | TEXT | Latest snapshot date for the chroot |

## How it works

A daily GitHub Actions job (05:30 UTC) fetches the iucar/cran project overview page with a plain non-browser User-Agent (which the Copr anti-scraper gate serves directly, with no proof-of-work), parses the per-chroot `(N)*` RPM counters and per-release `(N downloads)` `.repo` counters out of the server-rendered HTML, and appends one daily snapshot to the history pulled from the `current` release. The cumulative counters are differenced per chroot into daily deltas, every affected year shard plus the rolling `copr-downloads-recent.db` and `copr-downloads-summary.db` are rebuilt, and only the changed shards are uploaded (with `manifest.json` last, so a crash leaves the prior state authoritative). When the overview page is unreachable, the run is a cheap heartbeat that refreshes `last_checked` and leaves the prior release intact.

## Attribution

Download figures are read from the public [iucar/cran](https://copr.fedorainfracloud.org/coprs/iucar/cran/) project page on Fedora Copr; the RPMs are built and maintained by the [cran2copr](https://github.com/cran4linux/cran2copr) project (Iñaki Úcar). This repository provides only the daily snapshotting and packaging into SQLite. Please respect Fedora Copr's infrastructure and terms.

## License

The pipeline code in this repository is released under the [MIT License](LICENSE). The underlying download figures originate from Fedora Copr.
