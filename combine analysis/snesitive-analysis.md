combine data analysis
================
Jian Gao-jg5037
2025-12-04

Loading the datasets

``` r
ca_data <- read.csv("~/Desktop/P8105_final_project/datasets/clean_ca.csv")
nhanes_data <- read.csv("~/Desktop/P8105_final_project/datasets/nhanes_oral_clean.csv")
```

Data processing for California dataset

``` r
ca_data <- ca_data %>%
  mutate(
    # Step 1: standardize age groups (replace en-dash with hyphen)
    age_group_std = str_replace_all(age_group, "–", "-"),

    # Step 2: baseline life-stage
    age_group_baseline = case_when(
      age_group_std %in% c("Age <1", "Age 1-2") ~ "Infant/Toddler",
      age_group_std %in% c("Age 3-5") ~ "Early Childhood",
      age_group_std %in% c("Age 6-9") ~ "Middle Childhood",
      age_group_std %in% c("Age 10-14", "Age 15-18") ~ "Adolescent",
      age_group_std %in% c("Age 19-20", "Age 21-34") ~ "Young Adult",
      age_group_std %in% c("Age 35-44", "Age 45-64") ~ "Middle Adult",
      age_group_std %in% c("Age 65-74", "Age 75+") ~ "Older Adult",
      TRUE ~ NA_character_
    ),

    # Step 3: alternative life-stage
    age_group_alt = case_when(
      age_group_std %in% c("Age <1", "Age 1-2") ~ "Infant/Toddler",
      age_group_std %in% c("Age 3-5") ~ "Early Childhood",
      age_group_std %in% c("Age 6-9", "Age 10-14") ~ "Middle Childhood",
      age_group_std %in% c("Age 15-18") ~ "Adolescent",
      age_group_std %in% c("Age 19-20", "Age 21-34") ~ "Young Adult",
      age_group_std %in% c("Age 35-44", "Age 45-64") ~ "Middle Adult",
      age_group_std %in% c("Age 65-74", "Age 75+") ~ "Older Adult",
      TRUE ~ NA_character_
    ),
    year_num = as.numeric(str_replace(year, "CY ", "")),
    util_rate_num = parse_number(utilization_rate) / 100
  )
```

Main trend plot for California data

``` r
ggplot(ca_data, aes(year_num, util_rate_num, color = age_group_baseline)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "CA Medicaid: Dental Utilization by Life-Stage",
    x = "Year", y = "Utilization Rate"
  ) +
  theme_bw()
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](snesitive-analysis_files/figure-gfm/ca_main_trend-1.png)<!-- -->

Sensitivity Analysis A: Baseline vs Alternative Grouping\*\*

``` r
sens_data <- bind_rows(
  ca_data %>% select(year_num, util_rate_num, age_group = age_group_baseline) %>% mutate(scenario = "Baseline"),
  ca_data %>% select(year_num, util_rate_num, age_group = age_group_alt) %>% mutate(scenario = "Alternative")
)

ggplot(sens_data, aes(year_num, util_rate_num, color = scenario)) +
  geom_line(size = 1) +
  facet_wrap(~ age_group) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Sensitivity Analysis A: Baseline vs Alternative Grouping",
    x = "Year", y = "Utilization Rate"
  ) +
  theme_bw()
```

![](snesitive-analysis_files/figure-gfm/ca_sensitivity_analysis-1.png)<!-- -->

Sensitivity Summary Table

``` r
sens_summary_A <- sens_data %>%
  group_by(age_group, scenario) %>%
  summarise(mean_rate = mean(util_rate_num), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = mean_rate) %>%
  mutate(diff_A = Alternative - Baseline)


list(
  Sensitivity_A = sens_summary_A
)
```

    ## $Sensitivity_A
    ## # A tibble: 7 × 4
    ##   age_group        Alternative Baseline  diff_A
    ##   <chr>                  <dbl>    <dbl>   <dbl>
    ## 1 Adolescent            0.329    0.347  -0.0176
    ## 2 Early Childhood       0.396    0.396   0     
    ## 3 Infant/Toddler        0.0947   0.0947  0     
    ## 4 Middle Adult          0.144    0.144   0     
    ## 5 Middle Childhood      0.395    0.427  -0.0321
    ## 6 Older Adult           0.135    0.135   0     
    ## 7 Young Adult           0.169    0.169   0

“Sensitivity analysis comparing the baseline and alternative age-group
definitions showed that five of the seven life-stage categories produced
identical utilization estimates. Only Adolescent and Middle Childhood
differed slightly (–1.8% and –3.2% respectively), reflecting the
intentional boundary shift of the 10–14 age segment. These differences
were small and did not change temporal trends or overall conclusions;
therefore, the baseline life-stage grouping was retained for the main
analysis.”

# NHANES sort data processing

``` r
nhanes_period <- nhanes_data %>%
  # only keep overall data
  filter(sex == "All", race_ethnicity == "All") %>%
  # standardize age and map to CA life-stage
  mutate(
    age_group_std = stringr::str_replace_all(age_group, "–", "-"),
    age_group_baseline = dplyr::case_when(
      age_group_std == "2-5"                     ~ "Early Childhood",
      age_group_std == "6-11"                    ~ "Middle Childhood",
      age_group_std == "12-19"                   ~ "Adolescent",
      age_group_std %in% c("20-29", "30-39")     ~ "Young Adult",
      age_group_std %in% c("40-49", "50-59")     ~ "Middle Adult",
      age_group_std %in% c("60-69", "70 and over") ~ "Older Adult",
      TRUE ~ NA_character_
    ),
    period   = survey_years,        
    prev_num = percent / 100        
  ) %>%
  # as ggregate by period × life-stage × measure
  group_by(period, age_group_baseline, measure_clean) %>%
  summarise(
    prev_num = mean(prev_num, na.rm = TRUE),
    .groups = "drop"
  )
```

``` r
ca_data <- ca_data %>%
  select(year_num, age_group = age_group_baseline, measure, util_rate_num)
```

``` r
write.xlsx(
  list(
    CA = ca_data,
    NHANES = nhanes_period
  ),
  file = "~/Desktop/P8105_final_project/final_clean_data.xlsx",
  overwrite = TRUE
)
```
