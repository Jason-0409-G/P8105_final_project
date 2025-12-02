CA_Horizontal
================
2025-12-02

# Age Group Comparison

### —- Weighted mean by age group —-

### We consider weighted averages more appropriate because the population size (denominator) differs substantially across service categories.。

### A simple mean can overrepresent service types with small denominators.

### Weighted utilization = total users / total eligible population in each age group

``` r
age_summary_weighted <- ca %>%
  group_by(age_group) %>%
  summarise(
    total_users = sum(users, na.rm = TRUE),
    total_denom = sum(denominator, na.rm = TRUE),
    weighted_util = total_users / total_denom
  ) %>%
  arrange(desc(weighted_util))

kable(age_summary_weighted)
```

| age_group | total_users | total_denom | weighted_util |
|:----------|------------:|------------:|--------------:|
| Age 6–9   |    40582322 |    95145614 |     0.4265286 |
| Age 3–5   |    23744880 |    60055501 |     0.3953823 |
| Age 10–14 |    42226240 |   116131801 |     0.3636062 |
| Age 15–18 |    25440852 |    76874410 |     0.3309405 |
| Age 19–20 |     7168455 |    33929864 |     0.2112727 |
| Age 1–2   |     6097515 |    36316061 |     0.1679013 |
| Age 45–64 |    25894840 |   163704702 |     0.1581802 |
| Age 65–74 |     7832363 |    51298149 |     0.1526832 |
| Age 35–44 |    12483435 |    86622304 |     0.1441134 |
| Age 21–34 |    25108100 |   186778161 |     0.1344274 |
| Age 75+   |     5130129 |    40423363 |     0.1269100 |
| Age \<1   |      208771 |    14611638 |     0.0142880 |

``` r
# Plot -------------------------------------------------------
library(scales)

age_summary_weighted %>%
  ggplot(aes(x = reorder(age_group, weighted_util), y = weighted_util)) +
  geom_col(fill = "#4C72B0", width = 0.7) +
  geom_text(aes(label = percent(weighted_util, accuracy = 0.1)),
            hjust = -0.05, size = 3.5, color = "gray20") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Weighted Average Dental Service Utilization by Age Group",
    subtitle = "Weighted by denominator (population size) across all service types, 2013–2022",
    x = "Age Group",
    y = "Weighted Utilization Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
```

![](CA_Horizontal_files/figure-gfm/unnamed-chunk-1-1.png)<!-- --> After
weighting each service type by its population denominator, the overall
age-patterns remain similar, but the weighted results better reflect
true population-level service use.

- Children aged 6–9 show the highest weighted utilization (~42.7%).
- Followed by 3–5 (~39.5%) and 10–14 (~36.4%).
- Adolescents (15–18) also have relatively high utilization (~33%).
- Adults over age 45 show substantially lower utilization (\<20%).
- Infants (\<1 year) have the lowest utilization (~1.4%).

Compared with the unweighted results, the weighted analysis corrects for
small denominators (e.g., younger infants), producing more reliable age
comparisons.

In summary: School-aged children (6–14) are the most active users of
dental services, while older adults show potential under-utilization.

# Service Type Comparison

``` r
# ---- Service Type Comparison ----
# Compare average weighted utilization across different service types

measure_summary_weighted <- ca %>%
  group_by(measure) %>%
  summarise(
    total_users = sum(users, na.rm = TRUE),
    total_denom = sum(denominator, na.rm = TRUE),
    weighted_util = total_users / total_denom
  ) %>%
  arrange(desc(weighted_util))

kable(measure_summary_weighted)
```

| measure | total_users | total_denom | weighted_util |
|:---|---:|---:|---:|
| Dental visit | 46365680 | 142528460 | 0.3253082 |
| Diagnostic services | 24741672 | 82470159 | 0.3000076 |
| Exam/Evaluation | 37916792 | 142528460 | 0.2660296 |
| Preventive services | 35484508 | 142528460 | 0.2489644 |
| Treatment for Caries or Caries-Preventive Procedure | 32943682 | 142528460 | 0.2311376 |
| Treatment services | 25414311 | 142528460 | 0.1783104 |
| Sealant use | 3553547 | 27992407 | 0.1269468 |
| Restorative services | 15497710 | 138786702 | 0.1116657 |

``` r
# Plot ------------------------------------------------------
measure_summary_weighted %>%
  ggplot(aes(x = reorder(measure, weighted_util), y = weighted_util)) +
  geom_col(fill = "#F4A261", width = 0.7) +
  geom_text(aes(label = scales::percent(weighted_util, accuracy = 0.1)),
            hjust = -0.05, size = 3.5, color = "gray20") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Weighted Average Utilization by Service Type",
    subtitle = "Weighted by denominator across all age groups and years (2013–2022)",
    x = "Service Type",
    y = "Weighted Utilization Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
```

![](CA_Horizontal_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Age x Service Type Interaction

``` r
# Explore differences in utilization across age groups and service types
# Weighted to account for variability in denominator sizes

cross_summary_weighted <- ca %>%
  group_by(age_group, measure) %>%
  summarise(
    total_users = sum(users, na.rm = TRUE),
    total_denom = sum(denominator, na.rm = TRUE),
    weighted_util = total_users / total_denom
  )
```

    ## `summarise()` has grouped output by 'age_group'. You can override using the
    ## `.groups` argument.

``` r
cross_summary_weighted <- cross_summary_weighted %>%
  mutate(age_group = factor(age_group, levels = age_order))

# Heatmap --------------------------------------------------------
cross_summary_weighted %>%
  ggplot(aes(x = age_group, y = measure, fill = weighted_util)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Dental Service Utilization by Age Group and Service Type",
    subtitle = "Weighted by denominator (population size)",
    x = "Age Group",
    y = "Service Type",
    fill = "Weighted Utilization"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(color = "gray20"),
    legend.position = "right"
  )
```

![](CA_Horizontal_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
