clean_CA
================
Jason Gao
2025-11-05

# —- upload packages —-

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(stringr)
library(readr)
library(dplyr)
raw_data<-read.csv("datasets/ca_dental.csv")
head(raw_data)
```

    ##   Calendar.Year
    ## 1       CY 2013
    ## 2       CY 2013
    ## 3       CY 2013
    ## 4       CY 2013
    ## 5       CY 2013
    ## 6       CY 2013
    ##                                                                   Measure
    ## 1 Annual Dental Visit (D0100 - D9999 or Safety Net Clinics 03 Encounters)
    ## 2 Annual Dental Visit (D0100 - D9999 or Safety Net Clinics 03 Encounters)
    ## 3 Annual Dental Visit (D0100 - D9999 or Safety Net Clinics 03 Encounters)
    ## 4 Annual Dental Visit (D0100 - D9999 or Safety Net Clinics 03 Encounters)
    ## 5 Annual Dental Visit (D0100 - D9999 or Safety Net Clinics 03 Encounters)
    ## 6 Annual Dental Visit (D0100 - D9999 or Safety Net Clinics 03 Encounters)
    ##   Age.Filter   Users Users.Annotation.code Users.Annotation.Description
    ## 1     Age <1   3,922                    NA                             
    ## 2  Age 10-14 602,513                    NA                             
    ## 3    Age 1-2 127,182                    NA                             
    ## 4  Age 15-18 370,954                    NA                             
    ## 5  Age 19-20  88,764                    NA                             
    ## 6  Age 21-34 111,494                    NA                             
    ##   Denominator..3.Months.Continuous.Eligibility. Denominator.Annotation.code
    ## 1                                       251,033                          NA
    ## 2                                     1,186,943                          NA
    ## 3                                       577,485                          NA
    ## 4                                       877,824                          NA
    ## 5                                       307,943                          NA
    ## 6                                       944,886                          NA
    ##   Denominator.Annotation.Description Utilization.. Utilization.Annotation.code
    ## 1                                 NA         1.56%                          NA
    ## 2                                 NA        50.76%                          NA
    ## 3                                 NA        22.02%                          NA
    ## 4                                 NA        42.26%                          NA
    ## 5                                 NA        28.82%                          NA
    ## 6                                 NA        11.80%                          NA
    ##   Utilization.Annotation.Description
    ## 1                                   
    ## 2                                   
    ## 3                                   
    ## 4                                   
    ## 5                                   
    ## 6

# —- clean datasets —-

``` r
clean_data <- raw_data %>%
  # 1️⃣ 清理 Measure 括号
  mutate(Measure = str_trim(str_remove_all(Measure, "\\s*\\([^\\)]+\\)"))) %>%
  
  # 2️⃣ 创建三类
  mutate(service_group_3 = case_when(
    str_detect(Measure, regex("Preventive|Sealant", ignore_case = TRUE)) ~ "Preventive care",
    str_detect(Measure, regex("Restorative|Caries|Treatment", ignore_case = TRUE)) ~ "Treatment care",
    str_detect(Measure, regex("Annual Dental Visit|Exam|Evaluation|Diagnostic", ignore_case = TRUE)) ~ "Dental access",
    TRUE ~ "Other"
  )) %>%
  
  # 3️⃣ 删除 "No data available" 等无效描述
  mutate(desc_clean = str_to_lower(str_squish(`Users.Annotation.Description`))) %>%
  filter(
    is.na(desc_clean) |
      !desc_clean %in% c(
        "no data available",
        "cell suppressed for small numbers",
        "cell suppressed for complementary cell"
      )
  ) %>%
  
  # 4️⃣ 删除所有 Annotation 类列
  select(
    -starts_with("Users.Annotation"),
    -starts_with("Denominator.Annotation"),
    -starts_with("Utilization.Annotation"),
    -starts_with("service_group_3"),
    -desc_clean
  ) |>
  janitor::clean_names() |>
  rename(
    year = calendar_year,
    measure = measure,
    age_group = age_filter,
    users = users,
    denom_3m = denominator_3_months_continuous_eligibility,
    )
 
# 查看清理后结果
head(clean_data)
```

    ##      year             measure age_group   users  denom_3m utilization
    ## 1 CY 2013 Annual Dental Visit    Age <1   3,922   251,033       1.56%
    ## 2 CY 2013 Annual Dental Visit Age 10-14 602,513 1,186,943      50.76%
    ## 3 CY 2013 Annual Dental Visit   Age 1-2 127,182   577,485      22.02%
    ## 4 CY 2013 Annual Dental Visit Age 15-18 370,954   877,824      42.26%
    ## 5 CY 2013 Annual Dental Visit Age 19-20  88,764   307,943      28.82%
    ## 6 CY 2013 Annual Dental Visit Age 21-34 111,494   944,886      11.80%

# —- save as new document —-

``` r
write_csv(clean_data, "datasets/clean_ca.csv")
```
