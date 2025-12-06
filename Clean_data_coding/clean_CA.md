clean_CA
================
Jason Gao
2025-11-05

# —- upload packages —-

``` r
library(tidyverse)
library(janitor)
library(knitr)
library(here)
setwd("..")
raw_data <- read_csv("datasets/ca_dental.csv")
```

# —- clean datasets —-

``` r
# ---- Full data cleaning pipeline ----
# 1 Define age group order
age_order <- c(
  "Age <1","Age 1–2","Age 3–5","Age 6–9",
  "Age 10–14","Age 15–18","Age 19–20","Age 21–34",
  "Age 35–44","Age 45–64","Age 65–74","Age 75+"
)

# 2 Clean dataset
clean_data_basic <- raw_data |>
  # (1) Standardize column names
  clean_names() |>
  
  # (2) Remove invalid annotation rows
  mutate(desc_clean = str_to_lower(str_squish(users_annotation_description))) |>
  filter(
    is.na(desc_clean) |
      !desc_clean %in% c(
        "no data available",
        "cell suppressed for small numbers",
        "cell suppressed for complementary cell"
      )
  ) |>
  select(-desc_clean) |>
  
  # (3) Clean "measure" text
  mutate(
    measure = measure |>
      str_replace_all("[–—−]", "-") |>
      str_remove_all("\\s*\\([^()]*\\)") |>
      str_squish()
  ) |>
  
  # (4) Rename important columns
  rename(
    year      = calendar_year,
    age_group = age_filter,
    denominator = denominator_3_months_continuous_eligibility
  ) |>
  
  # (5) Standardize and order age groups
  mutate(
    age_group = age_group |>
      str_replace_all("[–—−-]", "–") |>
      str_squish() |>
      str_replace_all("^Age\\s*<\\s*1$", "Age <1") |>
      str_replace_all("^Age 1–2$|^Age 1-2$", "Age 1–2") |>
      str_replace_all("^Age 3–5$|^Age 3-5$", "Age 3–5") |>
      str_replace_all("^Age 6–9$|^Age 6-9$", "Age 6–9") |>
      str_replace_all("^Age 10–14$|^Age 10-14$", "Age 10–14") |>
      str_replace_all("^Age 15–18$|^Age 15-18$", "Age 15–18") |>
      str_replace_all("^Age 19–20$|^Age 19-20$", "Age 19–20") |>
      str_replace_all("^Age 21–34$|^Age 21-34$", "Age 21–34") |>
      str_replace_all("^Age 35–44$|^Age 35-44$", "Age 35–44") |>
      str_replace_all("^Age 45–64$|^Age 45-64$", "Age 45–64") |>
      str_replace_all("^Age 65–74$|^Age 65-74$", "Age 65–74") |>
      str_replace_all("^Age 75\\+$", "Age 75+"),
    age_group = factor(age_group, levels = age_order)
  ) |>
  
  # (6) Sort dataset
  arrange(year, age_group, measure)

# Quick check
clean_data_basic |> distinct(measure) |> arrange(measure)
```

    ## # A tibble: 8 × 1
    ##   measure                                            
    ##   <chr>                                              
    ## 1 Annual Dental Visit                                
    ## 2 Exams/Oral Health Evaluations                      
    ## 3 Treatment for Caries or Caries-Preventive Procedure
    ## 4 Use of Dental Treatment Services                   
    ## 5 Use of Diagnostic Services                         
    ## 6 Use of Preventive Services                         
    ## 7 Use of Restorative Services                        
    ## 8 Use of Sealant

``` r
clean_data_basic |> count(year, age_group) |> arrange(year, age_group)
```

    ## # A tibble: 132 × 3
    ##    year    age_group     n
    ##    <chr>   <fct>     <int>
    ##  1 CY 2013 Age <1        5
    ##  2 CY 2013 Age 1–2       6
    ##  3 CY 2013 Age 3–5       6
    ##  4 CY 2013 Age 6–9       7
    ##  5 CY 2013 Age 10–14     7
    ##  6 CY 2013 Age 15–18     6
    ##  7 CY 2013 Age 19–20     6
    ##  8 CY 2013 Age 21–34     6
    ##  9 CY 2013 Age 35–44     6
    ## 10 CY 2013 Age 45–64     6
    ## # ℹ 122 more rows

``` r
head(clean_data_basic)
```

    ## # A tibble: 6 × 12
    ##   year    measure  age_group  users users_annotation_code users_annotation_des…¹
    ##   <chr>   <chr>    <fct>      <dbl>                 <dbl> <chr>                 
    ## 1 CY 2013 Annual … Age <1      3922                    NA <NA>                  
    ## 2 CY 2013 Exams/O… Age <1        42                    NA <NA>                  
    ## 3 CY 2013 Treatme… Age <1       714                    NA <NA>                  
    ## 4 CY 2013 Use of … Age <1       141                    NA <NA>                  
    ## 5 CY 2013 Use of … Age <1       756                    NA <NA>                  
    ## 6 CY 2013 Annual … Age 1–2   127182                    NA <NA>                  
    ## # ℹ abbreviated name: ¹​users_annotation_description
    ## # ℹ 6 more variables: denominator <dbl>, denominator_annotation_code <lgl>,
    ## #   denominator_annotation_description <lgl>, utilization_percent <chr>,
    ## #   utilization_annotation_code <dbl>, utilization_annotation_description <chr>

``` r
# 3 Simplify measure names
clean_data_simple <- clean_data_basic |>
  mutate(
    measure = case_when(
      measure == "Annual Dental Visit" ~ "Dental visit",
      measure == "Exams/Oral Health Evaluations" ~ "Exam/Evaluation",
      measure == "Treatment for Caries or Caries–Preventive Procedure" ~ "Caries treatment",
      measure == "Use of Dental Treatment Services" ~ "Treatment services",
      measure == "Use of Diagnostic Services" ~ "Diagnostic services",
      measure == "Use of Preventive Services" ~ "Preventive services",
      measure == "Use of Restorative Services" ~ "Restorative services",
      measure == "Use of Sealant" ~ "Sealant use",
      TRUE ~ measure
    )
  )

# 4 Keep only key variables & simplify names
clean_data_final <- clean_data_simple |>
  select(
    year,
    age_group,
    measure,
    users,
    denominator,
    utilization_percent   
  ) |>
  rename(
    year             = year,
    age_group        = age_group,
    measure          = measure,
    users            = users,
    denominator      = denominator,
    utilization_rate = utilization_percent   
  )


head(clean_data_final)
```

    ## # A tibble: 6 × 6
    ##   year    age_group measure                   users denominator utilization_rate
    ##   <chr>   <fct>     <chr>                     <dbl>       <dbl> <chr>           
    ## 1 CY 2013 Age <1    Dental visit               3922      251033 1.56%           
    ## 2 CY 2013 Age <1    Exam/Evaluation              42      251033 0.02%           
    ## 3 CY 2013 Age <1    Treatment for Caries or…    714      251033 0.28%           
    ## 4 CY 2013 Age <1    Treatment services          141      251033 0.06%           
    ## 5 CY 2013 Age <1    Preventive services         756      251033 0.30%           
    ## 6 CY 2013 Age 1–2   Dental visit             127182      577485 22.02%

``` r
write_csv(clean_data_final, here("datasets", "clean_ca.csv"))
```

## Data Cleaning and Processing

The original Medi-Cal dental utilization dataset contained multiple
annotation fields, coded measure names, and inconsistent formatting
across variables.  
To prepare the data for analysis, we conducted a structured cleaning
process in **R**, summarized as follows:

1.  **Column normalization:**  
    All column names were standardized using `janitor::clean_names()` to
    ensure consistent lower-case formatting with underscores.

2.  **Invalid record removal:**  
    Rows annotated as *“No data available”*, *“Cell suppressed for small
    numbers”*, or *“Cell suppressed for complementary cell”* were
    excluded.

3.  **Measure text cleaning:**  
    All dash symbols were unified, parentheses and code ranges (e.g.,
    CPT codes) were removed, and extra whitespace was trimmed.

4.  **Age group standardization:**  
    Age labels were harmonized (e.g., `Age1-2`, `Age 1 – 2` → `Age 1–2`)
    and ordered logically from `<1` to `75+`.

5.  **Annotation field removal:**  
    Redundant annotation columns were dropped, retaining only core
    analytical variables.

6.  **Variable renaming:**  
    Key variables were renamed for clarity and consistency (e.g.,  
    `Calendar.Year` → `year`,  
    `Age.Filter` → `age_group`,  
    `Denominator..3.Months.Continuous.Eligibility.` → `denominator`,  
    `Utilization` → `utilization_rate`).

7.  **Measure simplification:**  
    Long descriptive measure names were shortened to concise forms
    (e.g.,  
    *Annual Dental Visit* → *Dental visit*,  
    *Use of Preventive Services* → *Preventive services*).

The final cleaned dataset contains **six essential variables** —  
`year`, `age_group`, `measure`, `users`, `denominator`, and
`utilization_rate` —  
suitable for descriptive and longitudinal analysis.

``` r
rename_tbl <- tibble::tribble(
  ~original_name,        ~new_name,          ~meaning_en,
  "calendar_year",       "year",             "Reporting year (CY xxxx)",
  "age_filter",          "age_group",        "Standardized age group (ordered)",
  "measure",             "measure",          "Type of dental service (cleaned and simplified)",
  "users",               "users",            "Number of individuals using the service (numerator)",
  "denominator_3_months_continuous_eligibility", "denominator", "Population with ≥3 months continuous eligibility (denominator for utilization rate)",
  "utilization",         "utilization_rate", "Service utilization rate = users ÷ denominator"
)

knitr::kable(
  rename_tbl,
  align = "lll",
  caption = "Table 1: Final variable names and definitions"
)
```

| original_name | new_name | meaning_en |
|:---|:---|:---|
| calendar_year | year | Reporting year (CY xxxx) |
| age_filter | age_group | Standardized age group (ordered) |
| measure | measure | Type of dental service (cleaned and simplified) |
| users | users | Number of individuals using the service (numerator) |
| denominator_3_months_continuous_eligibility | denominator | Population with ≥3 months continuous eligibility (denominator for utilization rate) |
| utilization | utilization_rate | Service utilization rate = users ÷ denominator |

Table 1: Final variable names and definitions
