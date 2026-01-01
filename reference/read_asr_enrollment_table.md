# Read enrollment table from extracted ASR files

Reads Table 7A (Average Daily Membership) or Table 8 (Net Enrollment)
from the extracted ASR ZIP directory.

## Usage

``` r
read_asr_enrollment_table(extract_dir, end_year)
```

## Arguments

- extract_dir:

  Directory containing extracted ASR files

- end_year:

  School year end

## Value

Data frame with district-level enrollment data
