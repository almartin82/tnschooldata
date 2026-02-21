# Tennessee School Data Package (tnschooldata)

## CRITICAL DATA SOURCE RULES

**NEVER use Urban Institute API, NCES CCD, or ANY federal data source**
— the entire point of these packages is to provide STATE-LEVEL data
directly from state DOEs. Federal sources aggregate/transform data
differently and lose state-specific details. If a state DOE source is
broken, FIX IT or find an alternative STATE source — do not fall back to
federal data.

------------------------------------------------------------------------

## Available Years

- **Total Range**: 1999-2024
- **ASR Era** (1999-2011): Historical data from Annual Statistical
  Report ZIP files with Excel tables
  - District-level only (no school-level data)
  - Includes grade-level enrollment (K-12)
- **Modern Era** (2012-2024): Current TDOE data portal with
  school/district profiles
  - School-level and district-level data
  - Demographic percentages converted to counts
  - Includes special populations (economically disadvantaged, LEP,
    special ed)

## Data Sources

- **Modern data**: `school-profile-YYYY-YYYY.xlsx` and
  `district-profile-YYYY-YYYY.xlsx` from
  <https://www.tn.gov/content/dam/tn/education/data/>
- **Historical data**: ASR ZIP files from
  <https://www.tn.gov/content/dam/tn/education/documents/asr/>

## Test Coverage

The test suite verifies: - No Inf/NaN percentages in output (division by
zero protection) - Non-zero state-level enrollment totals - Expected
subgroups present (demographics, special populations) - Grade-level data
present for ASR era - District sums approximately match state totals -
Percentages in valid 0-1 range

## Data Fidelity Requirement

**tidy=TRUE MUST maintain fidelity to raw, unprocessed file data** -
Enrollment counts must match source data - Percentages calculated from
counts, not estimated - No synthetic/interpolated values - Missing data
represented as NA, not 0

## Known Data Format Differences

- **Modern era**: Uses percentage columns (e.g., `white_pct`), which are
  converted to counts using `round(pct/100 * total)`
- **ASR era**: Uses direct count columns for grades
- **State row**: Modern era includes state row in district data
  (district_no=0); ASR era requires aggregation

------------------------------------------------------------------------

# Claude Code Instructions

### GIT COMMIT POLICY

- Commits are allowed
- NO Claude Code attribution, NO Co-Authored-By trailers, NO emojis
- Write normal commit messages as if a human wrote them

------------------------------------------------------------------------

## Local Testing Before PRs (REQUIRED)

**PRs will not be merged until CI passes.** Run these checks locally
BEFORE opening a PR:

### CI Checks That Must Pass

| Check        | Local Command                                                                  | What It Tests                                  |
|--------------|--------------------------------------------------------------------------------|------------------------------------------------|
| R-CMD-check  | `devtools::check()`                                                            | Package builds, tests pass, no errors/warnings |
| Python tests | `pytest tests/test_pytnschooldata.py -v`                                       | Python wrapper works correctly                 |
| pkgdown      | [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html) | Documentation and vignettes render             |

### Quick Commands

``` r
# R package check (required)
devtools::check()

# Python tests (required)
system("pip install -e ./pytnschooldata && pytest tests/test_pytnschooldata.py -v")

# pkgdown build (required)
pkgdown::build_site()
```

### Pre-PR Checklist

Before opening a PR, verify: - \[ \] `devtools::check()` — 0 errors, 0
warnings - \[ \] `pytest tests/test_pytnschooldata.py` — all tests
pass - \[ \]
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
— builds without errors - \[ \] Vignettes render (no `eval=FALSE` hacks)

------------------------------------------------------------------------

## LIVE Pipeline Testing

This package includes `tests/testthat/test-pipeline-live.R` with LIVE
network tests.

### Test Categories:

1.  URL Availability - HTTP 200 checks
2.  File Download - Verify actual file (not HTML error)
3.  File Parsing - readxl/readr succeeds
4.  Column Structure - Expected columns exist
5.  get_raw_enr() - Raw data function works
6.  Data Quality - No Inf/NaN, non-negative counts
7.  Aggregation - State total \> 0
8.  Output Fidelity - tidy=TRUE matches raw

### Running Tests:

``` r
devtools::test(filter = "pipeline-live")
```

See `state-schooldata/CLAUDE.md` for complete testing framework
documentation.

------------------------------------------------------------------------

## Git Workflow (REQUIRED)

### Feature Branch + PR + Auto-Merge Policy

**NEVER push directly to main.** All changes must go through PRs with
auto-merge:

``` bash
# 1. Create feature branch
git checkout -b fix/description-of-change

# 2. Make changes, commit
git add -A
git commit -m "Fix: description of change"

# 3. Push and create PR with auto-merge
git push -u origin fix/description-of-change
gh pr create --title "Fix: description" --body "Description of changes"
gh pr merge --auto --squash

# 4. Clean up stale branches after PR merges
git checkout main && git pull && git fetch --prune origin
```

### Branch Cleanup (REQUIRED)

**Clean up stale branches every time you touch this package:**

``` bash
# Delete local branches merged to main
git branch --merged main | grep -v main | xargs -r git branch -d

# Prune remote tracking branches
git fetch --prune origin
```

### Auto-Merge Requirements

PRs auto-merge when ALL CI checks pass: - R-CMD-check (0 errors, 0
warnings) - Python tests (if py{st}schooldata exists) - pkgdown build
(vignettes must render)

If CI fails, fix the issue and push - auto-merge triggers when checks
pass.

------------------------------------------------------------------------

## Valid Filter Values (tidy enrollment via `fetch_enr(tidy = TRUE)`)

### subgroup

`total_enrollment`, `white`, `black`, `hispanic`, `asian`,
`native_american`, `pacific_islander`, `multiracial`, `male`, `female`,
`special_ed`, `lep`, `econ_disadv`

**NOT in tidy enrollment:** Gender subgroups (`male`, `female`) are
defined in the code but only present when the raw data includes those
columns. ASR-era data (1999-2011) has grade-level counts only, no
demographic or gender breakdowns.

### grade_level

`PK`, `K`, `01`-`12`, `TOTAL`

Grade aggregates from
[`enr_grade_aggs()`](https://almartin82.github.io/tnschooldata/reference/enr_grade_aggs.md):
`K8`, `HS`, `K12`

**Note:** Raw column names like `grade_k`, `grade_01` etc. are mapped to
`PK`, `K`, `01`-`12` in tidy output. Always filter on the mapped values.

### entity flags

`is_state`, `is_district`, `is_campus`

Determined by the `type` column: `"State"`, `"District"`, `"Campus"`.

------------------------------------------------------------------------

## README Images from Vignettes (REQUIRED)

**NEVER use `man/figures/` or `generate_readme_figs.R` for README
images.**

README images MUST come from pkgdown-generated vignette output so they
auto-update on merge:

``` markdown
![Chart name](https://almartin82.github.io/{package}/articles/{vignette}_files/figure-html/{chunk-name}-1.png)
```

**Why:** Vignette figures regenerate automatically when pkgdown builds.
Manual `man/figures/` requires running a separate script and is easy to
forget, causing stale/broken images.
