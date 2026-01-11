# Tennessee Assessment Data Expansion Research

## Package Status

**Current Version:** 0.1.0 **R-CMD-check:** PASS (0 errors, 0 warnings,
0 notes) **Python Tests:** Not yet implemented **pkgdown:** Not yet
built

**Current Scope:** Enrollment data only (1999-2024)

------------------------------------------------------------------------

## Assessment Data Availability

### Data Source

**Tennessee Department of Education (TDOE)** - **Data Downloads
Portal:**
<https://www.tn.gov/education/districts/federal-programs-and-oversight/data/data-downloads.html> -
**Report Card:** <https://tdepublicschools.ondemand.sas.com/> - **File
Format:** Excel files (.xlsx)

### Assessment Files Available

**Assessment Data Files** provide: - Participation rates - Number of
students tested - Proficiency-level percentages - Broken down by grade
and student group

#### Years Available

- **State-level:** 2010-2024 (note: 2020 incomplete due to COVID)
- **District-level:** 2010-2024 (note: 2020 incomplete due to COVID)
- **School-level:** 2010-2021, 2023-2024 (missing 2022 at school level)

#### Coverage

- **Grades:** 3-8 (TCAP/TNReady) + High School EOC assessments
- **Subjects:**
  - English Language Arts (ELA)
  - Mathematics
  - Science
  - Social Studies
- **Student Groups:** All demographic subgroups (race/ethnicity,
  economic status, EL, special education)

### Accountability Files Available

**Accountability Data Files** provide: - Student counts and
proficiency-level breakdowns - Combined according to accountability
business rules - School, district, and state levels

#### Years Available

- **All levels:** 2013-2024 (excluding 2020)

------------------------------------------------------------------------

## Historical Assessment Landscape

### Tennessee Assessment Evolution Timeline

#### 1990s: TCAP Achievement Test Era

- **TCAP Achievement Test** established as primary statewide assessment
- **Grades:** 3-8
- **Subjects:** Reading/Language Arts, Mathematics, Science, Social
  Studies
- **Format:** Norm-referenced achievement tests

#### 2000s: Gateway Testing Era

**Early-Mid 2000s:** - **Gateway Examinations** introduced as high
school exit exams - **Subjects:** Algebra I, Biology I, English II -
**Graduation Requirement:** Students must pass all three Gateway tests
to receive diploma - Replaced older Tennessee Competency Test

**Late 2000s (2009-2010):** - **Gateway exit exam requirement
eliminated** for freshman class of 2009-10 - Part of Tennessee Diploma
Project (raised graduation requirements to 22 credits) - Gateway
replaced by expanded **End-of-Course (EOC) assessments** - EOC tests
integrated into course grades (15% of final grade)

#### 2010s: TNReady Transition

**Early 2010s:** - TCAP EOC assessments continued in high school
subjects - TCAP Achievement Tests continued in grades 3-8

**2015-2016:** - **TNReady** introduced as new TCAP assessment for ELA
and Math (grades 3-11) - Designed to align with new academic standards -
Replaced old TCAP tests with more rigorous questions

**2016-2017:** - First year of TNReady faced significant testing
platform difficulties - Second year showed improved scores across
subjects

**2018-2019:** - Science assessments transitioned from old TCAP format
to new standards - Planned transition to online administration (later
adjusted)

#### 2020s: Modern Era

- **TCAP** brand used for all statewide assessments
- **TNReady** for ELA and Math
- TCAP for Science and Social Studies
- EOC assessments for high school courses
- 2019-20 school year: No assessments due to COVID (federal waiver)
- 2020-21: Varying participation rates due to pandemic

------------------------------------------------------------------------

## Modern Assessment Structure

### Current Assessments (2024-25)

#### Grades 3-8 (TCAP Achievement Tests)

- **ELA (TNReady):** Grades 3-8
- **Math (TNReady):** Grades 3-8
- **Science:** Grades 3-8
- **Social Studies:** Grades 3-8

#### High School End-of-Course (EOC) Assessments

- **English I**
- **English II**
- **Algebra I**
- **Algebra II**
- **Geometry**
- **Biology** (Science)
- **U.S. History** (Social Studies)

### Historical Assessments

#### Gateway Tests (2000s - eliminated 2009-10)

- Algebra I
- Biology I
- English II

#### Additional High School Assessments

- **English III** (added 2012-13)
- Various other EOC subjects added over time

------------------------------------------------------------------------

## Schema Analysis

### File Structure (Based on TDOE Documentation)

**Assessment Files** (.xlsx format): - Multiple sheets/tabs for
different grade levels - Each sheet contains: - School/District/State
identifiers - Grade level - Subject area - Student group breakdowns -
**Participation rate** (percentage tested) - **Number of students
tested** - **Proficiency-level percentages**: - Below - Approaching - On
Track - Mastered (or similar 4-level scale)

**Accountability Files** (.xlsx format): - Similar structure to
assessment files - Combined metrics according to accountability rules -
Proficiency-level breakdowns - Suppression rules applied: - Single
asterisk (\*): Graduation cohort \< 10 - Double asterisk (\*\*):
Individual proficiency level \< 1% or \> 99% (state/district) or \< 5%
or \> 95% (school)

### Known Schema Issues

**Suppression:** - Small cell suppression (n \< 10) for privacy -
Percentage suppression at extremes (\< 1% or \> 99%) - Varies by
aggregation level

**Data Gaps:** - **2019-20:** No assessment data due to COVID waiver -
**2020-21:** Incomplete EOC data, varying participation rates -
**2022:** School-level assessment files appear missing from download
page

**Format Changes:** - Assessment formats and scales changed with TNReady
transition (2015-16) - Gateway test scores on different scale than EOC
assessments - Pre-2011 data may have different demographic categories

------------------------------------------------------------------------

## Time Series Heuristics

### Major Breakpoints

1.  **2009-2010:** Gateway elimination as graduation requirement
    - Gateway → EOC transition
    - Impact: High school assessment structure changed
2.  **2011:** Demographic categories may have changed
    - Addition of “Two or More Races” category
    - Asian/Pacific Islander split
3.  **2015-2016:** TNReady introduction
    - New test format, new proficiency scales
    - Major structural break in time series
    - Pre-2016 NOT directly comparable to post-2016
4.  **2019-2020:** COVID pandemic
    - No assessments administered
    - Gap in time series
5.  **2020-2021:** Variable participation
    - Lower participation rates
    - Data quality issues

### Continuous Segments

- **2010-2015:** Pre-TNReady EOC and TCAP Achievement Tests
- **2016-2019:** TNReady era (pre-COVID)
- **2021-2024:** Post-COVID recovery era

------------------------------------------------------------------------

## Implementation Complexity

### Complexity Level: **HIGH**

#### Challenges:

1.  **Multiple File Types:**
    - Assessment files (participation, proficiency percentages)
    - Accountability files (combined metrics)
    - Different file structures for different years
2.  **Schema Evolution:**
    - Gateway → EOC transition (2009-10)
    - TNReady introduction (2015-16) - major format change
    - Potential pre-2011 demographic category differences
3.  **Data Gaps:**
    - No 2019-20 data (COVID)
    - Incomplete 2020-21 data
    - Missing 2022 school-level files
4.  **Suppression Rules:**
    - Small cell suppression (n \< 10)
    - Percentage suppression at extremes
    - Varies by aggregation level
5.  **Aggregation Levels:**
    - State, District, School levels
    - Grade-level breakdowns
    - Student group breakdowns
    - Multiple subjects per grade level
6.  **Proficiency Scales:**
    - Different scales for Gateway vs EOC vs TNReady
    - 4-level proficiency scale (Below, Approaching, On Track, Mastered)
    - Scale may have changed over time
7.  **Participation Data:**
    - Participation rates
    - Number tested
    - Need to handle varying participation in pandemic years

#### Advantages:

1.  **Long Time Series:** 15 years of assessment data (2010-2024,
    excluding 2020)
2.  **Consistent File Format:** Excel files for all years
3.  **Official Data Source:** Direct from TDOE (no federal aggregation)
4.  **Multiple Aggregation Levels:** State, district, and school data
    available
5.  **Rich Disaggregations:** Student group breakdowns available

------------------------------------------------------------------------

## Implementation Recommendations

### Phase 1: Assessment Data Functions

**Proposed Function: `fetch_assessment(year, level, subject)`**

Parameters: - `year`: School year end (2010-2024, excluding 2020) -
`level`: “state”, “district”, or “school” - `subject`: “ela”, “math”,
“science”, “social_studies”, or “all” - `tidy`: Return tidy format
(TRUE) or wide format (FALSE)

**Data Sources:** - Assessment files from TDOE Data Downloads page -
Base URL pattern:
`https://www.tn.gov/content/dam/tn/education/data/{level}-assessment-files-{year}.xlsx`

### Phase 2: Accountability Data Functions

**Proposed Function: `fetch_accountability(year, level)`**

Parameters: - `year`: School year end (2013-2024, excluding 2020) -
`level`: “state”, “district”, or “school”

**Data Sources:** - Accountability files from TDOE Data Downloads page -
Base URL pattern:
`https://www.tn.gov/content/dam/tn/education/data/{level}-accountability-files-{year}.xlsx`

### Phase 3: Historical Assessment Data

**Challenges:** - Pre-2010 assessment data not in Data Downloads
portal - May need to locate in archived reports or databases - Gateway
test data (pre-2010) may require special handling

**Proposed Strategy:** - Start with 2010-present (available online) -
Research historical Gateway/TCAP data availability - Document data gaps
and format changes

### Phase 4: Time Series Handling

**Recommendations:** 1. Add `assessment_era` column to output: -
“pre_tnready” (2010-2015) - “tnready_transition” (2016-2018) -
“post_covid” (2021-2024)

2.  Add proficiency scale metadata:
    - Document scale changes
    - Include proficiency level definitions
3.  Add data quality flags:
    - Participation rate warnings (2020-21)
    - Suppression indicators
    - Missing data flags

------------------------------------------------------------------------

## Data Quality Considerations

### Known Issues

1.  **COVID-19 Impact (2019-21):**
    - 2019-20: No assessments (federal waiver)
    - 2020-21: Fall EOC data incomplete, varying participation
2.  **Small Cell Suppression:**
    - Results suppressed where n \< 10
    - Percentage suppression at extremes
    - Documented with asterisks in files
3.  **TNReady Transition (2015-16):**
    - Major format change
    - Not directly comparable to pre-2016
    - 2016 testing platform difficulties may affect data quality
4.  **File Availability:**
    - 2022 school-level assessment files missing from download page
    - Pre-2010 data not in standardized format

### Recommended Validation

1.  **Participation Rate Checks:**
    - Flag years with \< 95% participation
    - Document pandemic years
2.  **Suppression Documentation:**
    - Include suppression flags in output
    - Clear documentation of suppression rules
3.  **Cross-Level Validation:**
    - District sums should approximately equal state totals
    - School sums should approximately equal district totals
    - Allow for rounding differences
4.  **Time Series Consistency:**
    - Check for anomalous changes between years
    - Flag potential data quality issues

------------------------------------------------------------------------

## Testing Strategy

### Unit Tests Required

1.  **URL Accessibility:**
    - Verify assessment file URLs return 200 status
    - Test for available years (2010-2024, excluding 2020)
2.  **File Download:**
    - Verify files download successfully
    - Check file format (Excel .xlsx)
3.  **File Parsing:**
    - Verify readxl can parse files
    - Test multiple years to detect schema changes
4.  **Column Structure:**
    - Verify expected columns exist
    - Test for column name changes over time
5.  **Data Quality:**
    - No NA proficiency percentages where expected
    - Participation rates in valid range (0-1)
    - Student counts non-negative
6.  **Aggregation:**
    - State totals match sum of districts (approximately)
    - District totals match sum of schools (approximately)
7.  **Fidelity:**
    - Tidy format preserves raw data
    - No rounding errors in tidying

### Live Pipeline Tests

Similar to enrollment tests: 1. Download actual assessment files 2.
Parse and process 3. Validate output structure 4. Check data quality
constraints

------------------------------------------------------------------------

## Next Steps

### Research Needed

1.  **Download and analyze actual assessment files:**
    - Examine column structure for multiple years
    - Identify schema changes over time
    - Document proficiency level definitions
2.  **Investigate pre-2010 assessment data:**
    - Locate historical Gateway/TCAP data
    - Determine if data exists in machine-readable format
    - Assess feasibility of including earlier years
3.  **Test file access patterns:**
    - Verify URL patterns for all years
    - Check for missing files (e.g., 2022 school-level)
    - Document any access issues
4.  **Research TNReady format changes:**
    - Identify exact scale changes (2015-16)
    - Document proficiency level definitions
    - Determine comparability across eras

### Implementation Priority

1.  **High Priority:** 2016-2024 assessment data (TNReady era)
2.  **Medium Priority:** 2010-2015 assessment data (pre-TNReady)
3.  **Low Priority:** Pre-2010 Gateway/TCAP data (if available)

### Documentation Requirements

1.  **Proficiency Scale Definitions:**
    - Document 4-level scale (Below, Approaching, On Track, Mastered)
    - Note any changes in scale definitions over time
    - Include cut scores if available
2.  **Suppression Rules:**
    - Document all suppression rules clearly
    - Include suppression flags in output data
    - Provide guidance on handling suppressed values
3.  **COVID-19 Impact:**
    - Document 2019-20 data gap
    - Flag 2020-21 data quality issues
    - Provide guidance on interpreting pandemic-era data
4.  **Cross-Era Comparability:**
    - Clear warnings about pre/post-2016 comparability
    - Document Gateway vs EOC vs TNReady differences
    - Provide recommendations for time series analysis

------------------------------------------------------------------------

## References

### Official Sources

- [Tennessee Department of Education - Data Downloads &
  Requests](https://www.tn.gov/education/districts/federal-programs-and-oversight/data/data-downloads.html)
- [Tennessee School Report Cards \| 2025 TCAP State
  Results](https://tdepublicschools.ondemand.sas.com/state/assessment)

### Historical Documentation

- Tennessee Task Force on Student Testing and Assessment Report (2015)
- Tennessee Comptroller Reports (2002, 2004, 2009)
- Teaching, Testing, and Time Policy Report (SCORE, 2015)

### Assessment Resources

- TCAP Portal: <https://tn.mypearsonsupport.com/>
- TCAP Family Portal: <https://familyreport.tnedu.gov/>

### News and Analysis

- Chalkbeat Tennessee: TCAP coverage
- WPLN: History of TCAP testing
- Tennessee SCORE: Assessment policy analysis

------------------------------------------------------------------------

**Last Updated:** 2025-01-11 **Research Status:** Complete - Ready for
implementation planning **Recommended Next Phase:** Download and analyze
assessment files for schema documentation
