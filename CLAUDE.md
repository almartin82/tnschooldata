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

## Git Commits and PRs

- NEVER reference Claude, Claude Code, or AI assistance in commit
  messages
- NEVER reference Claude, Claude Code, or AI assistance in PR
  descriptions
- NEVER add Co-Authored-By lines mentioning Claude or Anthropic
- Keep commit messages focused on what changed, not how it was written

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
