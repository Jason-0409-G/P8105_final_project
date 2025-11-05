Clean_NHANES-Data
================
Malcolm

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Load the data

``` r
nhanes <- read_csv("datasets/nhanes_oral.csv")
```

    ## Rows: 1160 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): Survey Years, Sex, Age Group, Race and Hispanic Origin, Measure, P...
    ## dbl  (4): Percent, Standard Error, Lower 95% CI Limit, Upper 95% CI Limit
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Delete useless cols

``` r
cols_to_drop <- c("Presentation Standard", "Note1", "Note2", "Notea", "Noteb")
nhanes <- nhanes %>%
  select(-any_of(cols_to_drop))
```

# Rename cols

``` r
nhanes <- nhanes %>%
  rename(
    survey_years = `Survey Years`,
    sex = Sex,
    age_group = `Age Group`,
    race_ethnicity = `Race and Hispanic Origin`,
    measure = Measure,
    percent = Percent,
    se = `Standard Error`,
    ci_lower = `Lower 95% CI Limit`,
    ci_upper = `Upper 95% CI Limit`
  )
```

# Clean percent

``` r
nhanes <- nhanes %>%
  mutate(
    percent = str_replace_all(as.character(percent), "%", ""),
    percent = as.numeric(percent)
  )
```

# Clean years

#### 在做趋势图或回归分析时，需要用一个“数值型时间”来表示每个点的位置。如果直接用 “1999–2000” 这种字符串，R 会认为它是一个 分类变量（factor），而不是连续的时间变量。

``` r
nhanes <- nhanes %>%
  mutate(
    survey_years = str_replace_all(survey_years, "–", "-"),
    year_mid = as.numeric(str_sub(survey_years, 1, 4)) + 0.5
  )
```

# Standardize the Measure Name

``` r
measure_map <- c(
  "Total Dental Caries in Primary Teeth" = "total_primary",
  "Total Dental Caries in Permanent Teeth" = "total_perm",
  "Untreated Dental Caries in Primary Teeth" = "untreated_primary",
  "Untreated Dental Caries in Permanent Teeth" = "untreated_perm",
  "Sealants on Permanent Teeth" = "sealant_perm",
  "Sealants on Permanent and Primary Teeth" = "sealant_all"
)

nhanes <- nhanes %>%
  mutate(measure_clean = recode(measure, !!!measure_map))
```

# 目前未对分组进行任何操作

# Delete the rows that missing percent

``` r
nhanes_clean <- nhanes %>%
  filter(!is.na(percent))
```

# Sort and output

``` r
nhanes_clean <- nhanes_clean %>%
  select(
    survey_years, year_mid, sex, age_group,
    race_ethnicity, measure, measure_clean,
    percent, se, ci_lower, ci_upper
  )
```

# Save

``` r
write_csv(nhanes_clean, "datasets/nhanes_oral_clean.csv")
```
