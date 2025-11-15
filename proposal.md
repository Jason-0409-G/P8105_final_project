Trends and Regional Differences in U.S. Dental Caries and Preventive
Care: NHANES + California Medi-Cal (1999–2023)
================
Team: Jian Gao (jg5037), Bruce Liu (tl3391), Malcolm Chen (zc2823), Keyu
Pan (kp3127)
Date: 2025-11-12

- [Team Members](#team-members)
- [Proposed Title](#proposed-title)
- [Motivation](#motivation)
- [Expected Final Product](#expected-final-product)
- [Data Sources](#data-sources)
- [Planned Analyses](#planned-analyses)
- [Timeline](#timeline)

## Team Members

- Jason Gao (jg5037)
- Bruce Liu (tl3391)
- Malcolm Chen (zc2823)
- Keyu Pan (kp3127)

## Proposed Title

**Trends and Regional Differences in U.S. Dental Caries Burden and
Preventive Service Utilization: A Combined Analysis of NHANES and
California Medi-Cal Data (1999–2023)**

## Motivation

Dental caries remains the most common chronic disease of childhood in
the U.S., and progress has been slower among low-income and minority
children ([National Institute of Dental and Craniofacial Research
2022](#ref-nidcr2022)).  
Sealants are highly effective for preventing pit-and-fissure caries,
with large reductions in molar cavities reported in the literature ([Ng
et al. 2023](#ref-ng2023)).  
We will integrate national oral health prevalence data (NHANES ([Centers
for Disease Control and Prevention (CDC) 2025](#ref-nhanes2025))) with
California program-level dental utilization data (Medi-Cal ([California
Department of Health Care Services 2023](#ref-medical2023))), and this
ecological analysis will assess whether state-level trends in preventive
dental service utilization (Medi-Cal) are temporally aligned with
national changes in caries burden (NHANES),

## Expected Final Product

- Reproducible R Markdown report with interactive visualizations (GitHub
  Pages).  
- Statistical and graphical summaries of national caries trends and
  California preventive-service utilization.  
- Evaluation of COVID-19 disruptions in dental service use.
- Exploratory analysis using Medi-Cal sealant utilization among children
  as a proxy for California’s preventive dental coverage, examining its
  temporal relationship with national caries burden trends.

## Data Sources

1.  **NHANES Oral Health Prevalence Estimates (1999–2020)** — Centers
    for Disease Control and Prevention (CDC) ([Centers for Disease
    Control and Prevention (CDC) 2025](#ref-nhanes2025)).  
2.  **California Medi-Cal Dental Utilization and Sealant Data
    (2013–2023)** — California Department of Health Care Services
    ([California Department of Health Care Services
    2023](#ref-medical2023)).  
3.  **Data dictionary** — official metadata file describing variable
    definitions.

All datasets are stored in the `datasets/` folder:  
`nhanes_oral.csv`, `ca_dental.csv`, `ca_dictionary.csv`.

## Planned Analyses

- Harmonize age stages across datasets:  
  Early Childhood (2–5), Childhood (6–9), Adolescence (10–19),  
  Young Adulthood (20–34), Adulthood (35–64), Older Age (65+).  
- **NHANES:** Describe time trends in total and untreated caries by age,
  sex, and race.<br> Assess inequality trends across demographic
  subgroups.
- **Medi-Cal:** Analyze preventive visit and sealant utilization trends
  (2013–2023) among children and adolescents, and evaluate
  COVID-19–related interruptions using segmented regression.  
- **Ecological analysis:** Merge harmonized datasets by age group × year
  to explore temporal associations between California preventive
  utilization (as a proxy for preventive coverage) and national caries
  burden using weighted least squares (1/SE²).  
- **Visualization:** Generate time-series plots, scatter-regression
  plots, inequality trend charts, and interrupted time-series (ITS)
  diagrams.

**Challenges:** Aligning variable definitions, matching age-year
windows, and addressing ecological inference limitations.

## Timeline

| Date Range | Main Task |
|----|----|
| **Nov 1 – Nov 10** | Data cleaning and harmonization; preliminary plots for NHANES and Medi-Cal. |
| **Nov 11 – Nov 17** | NHANES national trend and inequality analysis. |
| **Nov 18 – Nov 24** | Medi-Cal preventive and sealant utilization; COVID-19 interruption analysis. |
| **Nov 25 – Dec 1** | Ecological association modeling and sensitivity checks. |
| **Dec 2 – Dec 8** | Integrate results and finalize figures/tables. |
| **Dec 9 – Dec 12** | Final report writing, GitHub Pages deployment, and presentation preparation. |

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-medical2023" class="csl-entry">

California Department of Health Care Services. 2023. “Dental Utilization
Measures and Sealant Data by Age Groups, Calendar Year 2013–2023.”
data.gov.
<https://catalog.data.gov/dataset/dental-utilization-measures-and-sealant-data-by-age-groups-calendar-year-2013-to-2021-61ecf>.

</div>

<div id="ref-nhanes2025" class="csl-entry">

Centers for Disease Control and Prevention (CDC). 2025. “NHANES: Select
Oral Health Prevalence Estimates.” healthdata.gov.
<https://healthdata.gov/CDC/NHANES-Select-Oral-Health-Prevalence-Estimates/xif4-xkww/about_data>.

</div>

<div id="ref-nidcr2022" class="csl-entry">

National Institute of Dental and Craniofacial Research. 2022. “Oral
Health in America: Advances and Challenges — Section 2 Summary.” NIDCR,
NIH.
<https://www.nidcr.nih.gov/research/oralhealthinamerica/section-2-summary>.

</div>

<div id="ref-ng2023" class="csl-entry">

Ng, T. C. H. et al. 2023. “A Concise Review of Dental Sealants in Caries
Management.” *PMC Open Review*.
<https://pmc.ncbi.nlm.nih.gov/articles/PMC10149715/>.

</div>

</div>
