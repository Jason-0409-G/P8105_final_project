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
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
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
library(tibble)
library(forcats)
library(knitr)
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
clean_data <- raw_data |>
  # 1) 统一列名为小写+下划线
  clean_names() |>
  
  # 2) 删除无效描述（no data / cell suppressed）
  mutate(uad = str_to_lower(str_squish(users_annotation_description))) |>
  filter(is.na(uad) | !uad %in% c(
    "no data available",
    "cell suppressed for small numbers",
    "cell suppressed for complementary cell"
  )) |>
  select(-uad) |>
  
  # 3) 清理 measure：破折号统一，去括号内容，去多余空格
  mutate(
    measure = measure |>
      str_replace_all("[–—−]", "-") |>
      str_remove_all("\\s*\\([^()]*\\)") |>
      str_squish()
  ) |>
  
  # 4) 三大类分组
  mutate(
    service_group_3 = case_when(
      str_detect(measure, regex("Annual Dental Visit|Exams|Evaluation|Diagnostic", ignore_case = TRUE)) ~ "Dental access",
      str_detect(measure, regex("Preventive|Sealant", ignore_case = TRUE)) ~ "Preventive care",
      str_detect(measure, regex("Treatment for Caries|Dental Treatment Services|Restorative", ignore_case = TRUE)) ~ "Treatment care",
      TRUE ~ "Other"
    )
  ) |>
  
  # 5) 删除所有 Annotation 类列
  select(
    -starts_with("users_annotation"),
    -starts_with("denominator_annotation"),
    -starts_with("utilization_annotation")
  ) |>
  
  # 6) 简化列名
  rename(
    year = calendar_year,
    age_group = age_filter,
    denom_3m = denominator_3_months_continuous_eligibility,
    service_group = service_group_3
  ) |>
  select(-measure)
  


# 快速检查
clean_data |> count(service_group)
```

    ##     service_group   n
    ## 1   Dental access 336
    ## 2 Preventive care 286
    ## 3  Treatment care 254

``` r
head(clean_data)
```

    ##      year age_group   users  denom_3m utilization service_group
    ## 1 CY 2013    Age <1   3,922   251,033       1.56% Dental access
    ## 2 CY 2013 Age 10-14 602,513 1,186,943      50.76% Dental access
    ## 3 CY 2013   Age 1-2 127,182   577,485      22.02% Dental access
    ## 4 CY 2013 Age 15-18 370,954   877,824      42.26% Dental access
    ## 5 CY 2013 Age 19-20  88,764   307,943      28.82% Dental access
    ## 6 CY 2013 Age 21-34 111,494   944,886      11.80% Dental access

``` r
# 导出
```

``` r
# 确保数字列干净
grouped_three <- clean_data |>
  mutate(
    users = parse_number(users),
    denom_3m = parse_number(denom_3m)
  ) |>
  group_by(year, age_group, service_group) |>       # 按 年龄+年+三大类分组
  summarise(
    users_sum = sum(users, na.rm = TRUE),
    denom_ref = first(denom_3m),                     # 同龄组共用一个分母
    rate = users_sum / denom_ref,
    rate_pct = scales::percent(rate, accuracy = 0.01),
    .groups = "drop"
  ) |>
  arrange(year, age_group, service_group)

head(grouped_three, 12)
```

    ## # A tibble: 12 × 7
    ##    year    age_group service_group   users_sum denom_ref   rate rate_pct
    ##    <chr>   <chr>     <chr>               <dbl>     <dbl>  <dbl> <chr>   
    ##  1 CY 2013 Age 1-2   Dental access      128947    577485 0.223  22.33%  
    ##  2 CY 2013 Age 1-2   Preventive care    159817    577485 0.277  27.67%  
    ##  3 CY 2013 Age 1-2   Treatment care      33515    577485 0.0580 5.80%   
    ##  4 CY 2013 Age 10-14 Dental access     1114757   1186943 0.939  93.92%  
    ##  5 CY 2013 Age 10-14 Preventive care   1105451   1186943 0.931  93.13%  
    ##  6 CY 2013 Age 10-14 Treatment care     497988   1186943 0.420  41.96%  
    ##  7 CY 2013 Age 15-18 Dental access      680193    877824 0.775  77.49%  
    ##  8 CY 2013 Age 15-18 Preventive care    575350    877824 0.655  65.54%  
    ##  9 CY 2013 Age 15-18 Treatment care     333155    877824 0.380  37.95%  
    ## 10 CY 2013 Age 19-20 Dental access      161019    307943 0.523  52.29%  
    ## 11 CY 2013 Age 19-20 Preventive care    125399    307943 0.407  40.72%  
    ## 12 CY 2013 Age 19-20 Treatment care      82549    307943 0.268  26.81%

``` r
library(forcats)

age_order <- c(
  "Age <1","Age 1–2","Age 3–5","Age 6–9",
  "Age 10–14","Age 15–18","Age 19–20","Age 21–34",
  "Age 35–44","Age 45–64","Age 65–74","Age 75+"
)

grouped_three_sorted <- grouped_three |>
  # 先留底原值，便于排查
  mutate(age_group_raw = age_group) |>
  # 统一破折号、空格和写法
  mutate(
    age_group = age_group |>
      str_replace_all("[–—−-]", "–") |>   # 所有破折号统一成 EN DASH
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
      str_replace_all("^Age 75\\+$", "Age 75+")
  ) |>
  # 设定顺序并排序
  mutate(age_group = factor(age_group, levels = age_order)) |>
  arrange(year, age_group, service_group)
```

## Data Cleaning and Processing

Our raw dataset contained detailed Medi-Cal dental utilization records,
including numerous annotation fields and coded measure names.  
To ensure consistent structure and analytical readiness, we implemented
a multi-step cleaning pipeline in R, summarized as follows:

1.  **Column normalization:** Using `janitor::clean_names()` to convert
    all headers to lower case with underscores.
2.  **Invalid record removal:** Dropped entries annotated as *“No data
    available”* or *“Cell suppressed for small numbers”*.
3.  **Measure standardization:** Replaced multiple dash types, removed
    parenthetical CPT code ranges, and trimmed whitespace.
4.  **Service classification:** Mapped measures into three overarching
    service groups — *Dental access*, *Preventive care*, and *Treatment
    care*.
5.  **Annotation field removal:** Excluded redundant columns beginning
    with `Users.Annotation`, `Denominator.Annotation`, or
    `Utilization.Annotation`.
6.  **Variable renaming:** Unified variable names for clarity (e.g.,
    `Calendar.Year` → `year`,
    `Denominator..3.Months.Continuous.Eligibility.` → `denom_3m`).

``` r
measure_map <- tribble(
  ~measure_simplified,                                 ~service_group,     ~meaning_cn,
  "Annual Dental Visit",                               "Dental access",    "年度牙科就诊率/至少一次就诊",
  "Exams/Oral Health Evaluations",                     "Dental access",    "口腔检查与评估",
  "Use of Diagnostic Services",                        "Dental access",    "诊断类服务（含影像等）",
  "Use of Preventive Services",                        "Preventive care",  "预防性服务（洁治、氟化等）",
  "Use of Sealant",                                    "Preventive care",  "窝沟封闭（预防性）",
  "Treatment for Caries or Caries–Preventive Procedure","Treatment care",  "龋齿治疗/防龋操作",
  "Use of Dental Treatment Services",                  "Treatment care",   "治疗性服务总类（修复、拔牙等）",
  "Use of Restorative Services",                       "Treatment care",   "修复性服务（补牙、修复体）"
)

kable(measure_map, align = "lll", caption = "表1：简化后的 Measure 与三大类映射")
```

| measure_simplified | service_group | meaning_cn |
|:---|:---|:---|
| Annual Dental Visit | Dental access | 年度牙科就诊率/至少一次就诊 |
| Exams/Oral Health Evaluations | Dental access | 口腔检查与评估 |
| Use of Diagnostic Services | Dental access | 诊断类服务（含影像等） |
| Use of Preventive Services | Preventive care | 预防性服务（洁治、氟化等） |
| Use of Sealant | Preventive care | 窝沟封闭（预防性） |
| Treatment for Caries or Caries–Preventive Procedure | Treatment care | 龋齿治疗/防龋操作 |
| Use of Dental Treatment Services | Treatment care | 治疗性服务总类（修复、拔牙等） |
| Use of Restorative Services | Treatment care | 修复性服务（补牙、修复体） |

表1：简化后的 Measure 与三大类映射

``` r
rename_tbl <- tribble(
  ~original_name,                                  ~new_name,     ~meaning_cn,
  "calendar_year",                                 "year",        "统计年份（CY xxxx）",
  "age_filter",                                    "age_group",   "年龄分组（原始）",
  "users",                                         "users",       "使用服务人数（分子）",
  "denominator_3_months_continuous_eligibility",   "denom_3m",    "连续3个月资格的分母人数",
  "service_group_3",                               "service_group","三大类（access / preventive / treatment）"
)

kable(rename_tbl, align = "lll", caption = "表2：变量重命名对照表")
```

| original_name | new_name | meaning_cn |
|:---|:---|:---|
| calendar_year | year | 统计年份（CY xxxx） |
| age_filter | age_group | 年龄分组（原始） |
| users | users | 使用服务人数（分子） |
| denominator_3_months_continuous_eligibility | denom_3m | 连续3个月资格的分母人数 |
| service_group_3 | service_group | 三大类（access / preventive / treatment） |

表2：变量重命名对照表
