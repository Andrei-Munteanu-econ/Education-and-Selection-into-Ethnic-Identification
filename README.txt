# README for Replication Package

**"Education and Selection into Ethnic Identification: Evidence from Roma People in Romania"**

Andreea Mitrut (University of Gothenburg), Gabriel Kreindler (Harvard University), Margareta Matache (Harvard University), Andrei Munteanu (Université du Québec à Montréal), Cristian Pop-Eleches (Columbia University)

*American Economic Journal: Applied Economics*

---

## Overview

This package contains all code required to reproduce the tables and figures in the paper and online appendix. All raw data are confidential and cannot be included; instructions for obtaining access are provided below.

---

## Data Availability and Provenance Statements

### Romanian Population and Housing Census Microdata (1992, 2002, 2011)

Individual-level microdata from the Romanian Population and Housing Censuses of 1992, 2002, and 2011. These data are confidential and were obtained from the Romanian National Institute of Statistics (Institutul Național de Statistică, INS).

- **Provider:** Romanian National Institute of Statistics (INS), Bucharest, Romania
- **Access:** Researchers may request access by contacting INS directly. Access requires a formal data-sharing agreement and may be subject to fees.
  - Website: https://insse.ro
  - Contact: relatii.publice@insse.ro
- **Included in package:** No (confidential)

### Vital Statistics Natality Records (2003–2011, excluding 2004)

Individual-level birth records from Romania's civil registration system, used to link mothers in the 2011 census to their children and to obtain birth-certificate ethnicity.

- **Provider:** Romanian National Institute of Statistics (INS)
- **Access:** Same process as census microdata above
- **Included in package:** No (confidential)

### Survey Data

Original survey data collected by the authors via a Romanian survey firm. The survey measures beliefs about Roma ethnic identification, perceived markers of Roma ethnicity, and attitudes toward Roma people. Two experimental arms (priming and no-priming) with approximately 1,000 respondents each. Data are stored in SPSS format (`.sav` files).

- **IRB:** Columbia University, protocol number AAAU5373
- **Provider:** Authors
- **Included in package:** Yes (anonymized). The raw `.sav` files remain confidential; the provided files are the anonymized extracts `data/processed/survey/survey_no_priming.csv` and `survey_priming.csv` (used variables only, as numeric codes; identifiers, free text, and town/village names removed), plus the aggregate `siruta3_roma_survey.csv` locality Roma-share lookup.

### Administrative Reference Data

SIRUTA codes (Romania's standard locality classification system) and census nomenclators (school, occupation, and ethnicity codes) used to harmonize geographic and demographic variables across census years.

- **Provider:** Romanian National Institute of Statistics (INS)
- **Included in package:** Yes

---

## Dataset List

| Data File | Source | Provided | Description |
|-----------|--------|----------|-------------|
| 1992 census microdata | INS Romania | No | DBF files; individual-level records: demographics, education, ethnicity, occupation |
| 2002 census microdata | INS Romania | No | DBF files; individual-level records: demographics, education, ethnicity, occupation |
| 2011 census microdata | INS Romania | No | DBF files; individual-level records: demographics, education, ethnicity, occupation, ancestry |
| Birth records 2003–2011 | INS Romania | No | DTA files; birth certificates with mother's ethnicity and education |
| Census–birth linkage file | INS Romania | No | Crosswalk matching birth records to 2011 census individuals |
| SIRUTA locality codes | INS Romania | Yes | Standard locality classification system for Romania |
| Census nomenclators | INS Romania | Yes | Code-to-label mappings for census variables (school, ethnicity, occupation) |
| `baza_Link1.sav`, `baza_Link2.sav` | Authors | No (confidential) | Raw survey data on perceptions of Roma ethnicity, two experimental arms |
| `data/processed/survey/survey_no_priming.csv`, `survey_priming.csv` | Authors | Yes (anonymized) | Anonymized survey extracts (used variables only) built from the raw `.sav` by `00_anonymize_survey.R` |
| `data/processed/survey/siruta3_roma_survey.csv` | Authors | Yes (committed) | Aggregate locality Roma-share lookup; read by `00_anonymize_survey.R` |
| `data/processed/structural/results_*.rds` | Authors | Yes (provided) | Structural-model bootstrap estimates (normal/uniform/lognormal/triangle); let Table A.12, Figure 2, and Figure A.9 run without HPC re-estimation |

---

## Computational Requirements

### Software

- **R** version 4.3 or later. The results in the paper were produced with **R 4.4.2 (2024-10-31)** on Ubuntu 24.04.1 LTS (x86_64-pc-linux-gnu), run via RStudio on the Harvard FASRC (FAS Research Computing) cluster.

The exact package versions used to produce the results are listed at the end of this README (see "Package Versions").

The following packages are loaded by `code/main.R` via `pacman::p_load()`:

| Package | Purpose |
|---------|---------|
| `data.table` | Fast data manipulation and I/O |
| `fixest` | Fixed-effects and IV regressions |
| `modelsummary` | Regression table formatting |
| `haven` | Read SPSS/Stata files |
| `foreign` | Read DBF census files |
| `dplyr`, `tidyverse` | Data manipulation |
| `ggplot2`, `cowplot`, `patchwork` | Visualization |
| `ggrepel`, `ggh4x`, `gridExtra`, `scales` | Plot utilities |
| `kableExtra`, `xtable` | Table formatting |
| `sf`, `maps` | Spatial data and mapping |
| `MASS` | Logistic regression |
| `effects` | Plot logit model effects |
| `DescTools` | Winsorization |
| `corrplot`, `Hmisc` | Correlation matrices |
| `triangle`, `GA`, `quadprog` | Structural model estimation |
| `doParallel`, `foreach` | Parallel computing (structural model) |

### Controlled Randomness

Random seeds are set wherever results depend on simulation or bootstrap resampling, so all reported results are exactly reproducible:

- **Structural model** (`code/05 Structural model estimation/00_main_structural.R`): `set.seed(1)` (line 341) fixes the random draws used throughout estimation; `set.seed(j)` (line 367) re-seeds each bootstrap iteration `j`. The parallel estimation scripts (`01_estimate_model_het_parallel*.R`) likewise seed each bootstrap iteration by its index.
- **Bootstrapped scatterplots** re-seed each bootstrap iteration `i`:
  - `code/01 Figure 01 A04 A05 A06 scatterplots/Figure 01 and A04 A05 A06 scatterplots.R` (lines 133 and 390)
  - `code/AA Figure A02 gender-adjusted scatterplot/Figure A02.R` (lines 211 and 472)
  - `code/AA Figure A03 household head scatterplot/Figure A03 household heads.R` (lines 154 and 421)

### Hardware Requirements

The results in the paper were produced on the Harvard FASRC cluster using **20 cores and 150 GB RAM**.

- **Reduced-form analysis** (data reading, cleaning, all main and appendix tables/figures except structural): standard desktop or laptop; 16 GB RAM recommended given the size of census microdata.
- **Structural model estimation** (`code/05 Structural model estimation/`): requires a high-performance computing (HPC) cluster with parallel processing. Estimation scripts use `doParallel` with 20 parallel workers. Each distributional specification (normal, uniform, lognormal, triangle) runs bootstrap iterations across education levels. Analysis and figure scripts in Section 05 can use pre-computed `.rds` results files if available in `data/processed/structural/`.

**Note on thread settings:** `code/functions.R` sets `setDTthreads(threads=36)`. Adjust this to match available hardware before running.

### Estimated Runtime

Timings below are for the FASRC environment (20 cores, 150 GB RAM). Most individual scripts run in under one minute; the exceptions are data cleaning, structural model estimation, and the bootstrapped scatter plots.

| Component | Approximate Time | Hardware |
|-----------|-----------------|---------|
| Read raw census data (`00 read raw/`) | 30–60 min | FASRC (20 cores, 150 GB RAM) |
| Clean and link data (`00 clean/`) | ~1 hour | FASRC (20 cores, 150 GB RAM) |
| Scatter plots (`01 Figure 01 …`) | Several hours (bootstrapped standard errors) | FASRC (20 cores, 150 GB RAM) |
| Reduced-form analysis (most other scripts) | Under 1 min each | FASRC (20 cores, 150 GB RAM) |
| Structural model estimation (`05 Structural model estimation/`) | Several hours | FASRC (20 cores, 150 GB RAM) |

---

## Description of Programs

### Directory Structure

```
replication_final/
├── code/
│   ├── main.R                                        Environment setup and package loading
│   ├── functions.R                                   Shared utility functions
│   ├── 00 read raw/                                  Read raw census microdata (7 scripts)
│   ├── 00 clean/                                     Clean and link data across census years (8 scripts)
│   ├── 01 Figure 01 A04 A05 A06 scatterplots/       Figures 1, A.4, A.5, A.6 (1 script)
│   ├── 02 Table 01 main iv results/                  Table 1 (1 script)
│   ├── 03 Table 02 A08 heterogeneity and town size/  Tables 2, A.8 (1 script)
│   ├── 04 Table 03 marriage/                         Table 3 (1 script)
│   ├── 05 Structural model estimation/               Structural model data prep, estimation, analysis (11 scripts)
│   ├── 06 Figure 02 structural estimates/            Figure 2 (1 script)
│   ├── 07 Table 04 survey/                           Table 4; survey anonymization + locality shares (3 scripts)
│   ├── AA Figure A01 match rates by town pop/        Figure A.1 (1 script)
│   ├── AA Figure A02 gender-adjusted scatterplot/    Figure A.2 (2 scripts)
│   ├── AA Figure A03 household head scatterplot/     Figure A.3 (1 script)
│   ├── AA Figure A07 A08 sample representativeness/  Figures A.7, A.8 (1 script)
│   ├── AA Figure A09 structural model fit/           Figure A.9 (1 script)
│   ├── AA Table A01 mismatch rates/                  Table A.1 (1 script)
│   ├── AA Table A02 mismatch Roma by education/      Table A.2 (1 script)
│   ├── AA Table A03 conditional independence mismatch/ Table A.3 (1 script)
│   ├── AA Table A04 IV mothers sample/               Table A.4 (1 script)
│   ├── AA Table A05 no fixed effects/                Table A.5 (1 script)
│   ├── AA Table A06 2002-2011/                       Table A.6 (1 script)
│   ├── AA Table A07 mother tongue/                   Table A.7 (1 script)
│   ├── AA Table A09 IV household enumerator and non declaration/ Table A.9 (1 script)
│   ├── AA Table A10 IV siblings/                     Table A.10 (1 script)
│   ├── AA Table A11 marriage/                        Table A.11 (1 script)
│   ├── AA Table A12 structural parameters distribution/ Table A.12 (1 script)
│   ├── AA Table A13 survey ethnic identification and education/ Table A.13 (1 script)
│   ├── AA Table A14 survey Roma markers/             Table A.14 (1 script)
│   └── AA text/                                      In-text statistics (5 scripts)
├── data/
│   ├── raw/                                          Raw data (not provided; see Data Availability)
│   │   ├── 1992/                                     1992 census DBF files
│   │   ├── 2002/                                     2002 census DBF files
│   │   ├── 2011/                                     2011 census DBF files
│   │   ├── Birth/                                    Birth records DTA files
│   │   ├── other/{1992,2002,2011}/                   Census nomenclators and SIRUTA codes
│   │   └── survey/                                   Survey SPSS files
│   └── processed/                                    Intermediate datasets (generated by code; except the committed survey lookup)
│       ├── linked/                                   Linked individual records across census years
│       ├── structural/                               Structural-model bootstrap results (.rds; provided)
│       ├── results/                                  Figure bootstrap result caches (generated)
│       └── survey/                                   Anonymized survey CSVs + provided siruta3_roma_survey.csv lookup
└── output/                                           Figures (.pdf) and tables (.tex) produced by code
```

### Execution Model

`code/main.R` loads all required packages, defines all working directory path variables, sources `functions.R` (which also sets thread options), and then **sources every analysis script in order** via a `run()` helper. Scripts execute in folder-numbering order — `00 read raw` → `00 clean` → `01` → `02` → … → `07` → `AA` — with `run()` using `source(..., chdir = TRUE)` so each script's relative paths and `setwd()` calls resolve correctly. Within-folder order follows filename prefixes.

Two flags near the top of `main.R` gate the optional steps that require the confidential census or an HPC cluster (both default to `FALSE`):

- `estimate_structural` — when `FALSE`, the heavy bootstrap estimation scripts (`05 Structural model estimation/01_estimate_model_het_parallel*.R`) are skipped and `02_analysis_main.R` uses the precomputed `.rds` results in `data/processed/structural/`. Set `TRUE` to re-estimate from scratch (HPC).
- `regenerate_siruta_share` — when `FALSE`, the survey pipeline uses the provided, committed `data/processed/survey/siruta3_roma_survey.csv`. Set `TRUE` to regenerate that locality Roma-share lookup from the census via `07 Table 04 survey/00_roma_share_by_locality.R` (HPC).

### Structural Model

`code/05 Structural model estimation/` contains:
- `00_main_structural.R` — prepares estimation input data from cleaned linked census data
- `01_estimate_model_het_parallel.R` (and variants: `_lognormal`, `_triangle`, `_uniform`) — bootstrap estimation under four distributional assumptions; saves results to `data/processed/structural/`
- `02_analysis_main.R` — post-estimation analysis using saved results; feeds Figure 2

Estimation scripts require an HPC environment and are gated behind the `estimate_structural` flag in `main.R` (default `FALSE`). With the flag `FALSE`, the precomputed `.rds` results in `data/processed/structural/` are used and the analysis and figure scripts run directly without re-estimating.

---

## Instructions to Replicators

1. **Obtain data access** from the Romanian National Institute of Statistics (INS). See the Data Availability section for contact details.

2. **Place raw data files** in the corresponding subdirectories under `data/raw/`:
   - Census microdata: `data/raw/1992/`, `data/raw/2002/`, `data/raw/2011/`
   - Birth records: `data/raw/Birth/`
   - Census nomenclators: `data/raw/other/1992/`, `data/raw/other/2002/`, `data/raw/other/2011/`
   - Survey data: `data/raw/survey/`

3. **Update working directory paths** in `code/main.R`. Replace the `wd` and `wd_data_raw` variables (lines 30 and 31) with the local path to the `replication_final/` folder and the raw data directory, respectively. All other paths are constructed from `wd` and do not need modification.

4. **Adjust thread count** in `code/functions.R`: set `setDTthreads(threads=N)` to match available cores on the local machine.

5. **Set the run-control flags** near the top of `code/main.R`: `estimate_structural` (re-estimate the structural model vs. use precomputed results) and `regenerate_siruta_share` (rebuild the survey locality Roma-share lookup from the census vs. use the provided file). Both default to `FALSE`; leave them `FALSE` unless you have the census and an HPC cluster.

6. **Run `code/main.R`.** It installs any missing packages via `pacman::p_load()` and then sources every analysis script in the correct order (`00 read raw` → `00 clean` → `01`…`07` → `AA`). No manual per-folder execution is needed.

7. **Structural model:** the bootstrap estimation scripts in `code/05 Structural model estimation/` require an HPC cluster (see Computational Requirements). With `estimate_structural <- FALSE` they are skipped and the precomputed `.rds` results in `data/processed/structural/` are used; set `estimate_structural <- TRUE` to re-estimate.

8. **Survey data:** place the raw SPSS files in `data/raw/survey/`. `00_anonymize_survey.R` builds the anonymized `data/processed/survey/survey_*.csv` from the `.sav` files plus the provided `siruta3_roma_survey.csv` lookup. Set `regenerate_siruta_share <- TRUE` only to rebuild that lookup from the census (HPC); otherwise the committed lookup is used.

9. **Verify outputs** by comparing files in `output/` with those listed below.

---

## List of Tables and Programs

### Main Text

| Table | Title | Program | Output File |
|-------|-------|---------|-------------|
| Table 1 | Changes in Ethnic Identification by Education: Instrumental Variables Approach | `code/02 Table 01 main iv results/Table 01 main iv results.R` | `output/Table 01.tex` |
| Table 2 | Changes in Ethnic Identification by Education — Heterogeneity | `code/03 Table 02 A08 heterogeneity and town size/Table 02 A08 heterogeneity and town size.R` | `output/Table 02.tex` |
| Table 3 | Inter-marriage and Ethnic Identification | `code/04 Table 03 marriage/04 Table 03 marriage.R` | `output/Table 03.tex` |
| Table 4 | Beliefs Regarding Roma Changes in Ethnic Identification and Education | `code/07 Table 04 survey/Table 04 survey.R` | `output/Table 04.tex` |

### Online Appendix

| Table | Title | Program | Output File |
|-------|-------|---------|-------------|
| Table A.1 | Census Linkages | `code/AA Table A01 mismatch rates/Table A01 mismatch rates.R` | `output/Table A01.tex` |
| Table A.2 | Census Linkages for Baseline Roma | `code/AA Table A02 mismatch Roma by education/Table A02 mismatch Roma by education.R` | `output/Table A02.tex` |
| Table A.3 | Conditional Independence of Reported Education and Ethnicity for Mismatched Records | `code/AA Table A03 conditional independence mismatch/Table A03 conditional independence mismatch.R` | `output/Table A03.tex` |
| Table A.4 | Changes in Ethnic Identification by Education (Mothers' Sample) | `code/AA Table A04 IV mothers sample/Table A04 IV mothers sample.R` | `output/Table A04.tex` |
| Table A.5 | Changes in Ethnic Identification by Education: Instrumental Variables Approach (no fixed effects) | `code/AA Table A05 no fixed effects/Table A05 no fixed effects.R` | `output/Table A05.tex` |
| Table A.6 | Changes in Ethnic Identification by Education (2002–2011) | `code/AA Table A06 2002-2011/Table A06 2002-2011.R` | `output/Table A06.tex` |
| Table A.7 | Changes in Ethnic Identification by Education — Ethnicity Defined using Romani as Mother Tongue | `code/AA Table A07 mother tongue/Table A07 mother tongue.R` | `output/Table A07.tex` |
| Table A.8 | Changes in Ethnic Identification by Education and Town Size | `code/03 Table 02 A08 heterogeneity and town size/Table 02 A08 heterogeneity and town size.R` | `output/Table A08.tex` |
| Table A.9 | Changes in Ethnic Identification by Education — Different Specifications (1992–2011) | `code/AA Table A09 IV household enumerator and non declaration/AA Table A09 household enumerator and non declaration.R` | `output/Table A09.tex` |
| Table A.10 | Between-Sibling Changes in Ethnic Identification | `code/AA Table A10 IV siblings/Table A10 IV siblings.R` | `output/Table A10.tex` |
| Table A.11 | Intermarriage and Ethnic Identification (2002–2011) | `code/AA Table A11 marriage/Table A11 marriage.R` | `output/Table A11.tex` |
| Table A.12 | Fraction of Roma-background Individuals with a Null Probability of Roma Self-Reporting | `code/AA Table A12 structural parameters distribution/Table A12 structural parameters distribution.R` | `output/Table A12.tex` |
| Table A.13 | Respondent Views Internally Consistent but Divergent | `code/AA Table A13 survey ethnic identification and education/Table A13.R` | `output/Table A13.tex` |
| Table A.14 | Perceived Markers of Roma Ethnicity | `code/AA Table A14 survey Roma markers/Table A14 survey Roma markers.R` | `output/Table A14.tex` |

### In-Text Statistics

| Content | Program |
|---------|---------|
| Survey: ease of identifying Roma | `code/AA text/text survey easy to identify Roma.R` |
| Match rates and sample sizes | `code/AA text/text match rates and sample sizes for use in text.R` |
| Share with no reported ethnicity (2011) | `code/AA text/text percent with no reported ethnicity in 2011.R` |
| Share of male household heads | `code/AA text/text share of male hh heads.R` |

*Only the first script writes a file (`output/text easy to identify Roma.tex`); the other three print their statistics to the console.*

---

## List of Figures and Programs

### Main Text

| Figure | Title | Program | Output File |
|--------|-------|---------|-------------|
| Figure 1 | Changes in Ethnic Identification by Education and Occupation, 1992–2011 | `code/01 Figure 01 A04 A05 A06 scatterplots/Figure 01 and A04 A05 A06 scatterplots.R` | `output/Figure 01.pdf` |
| Figure 2 | Estimated Roma Population Accounting for Heterogeneity | `code/06 Figure 02 structural estimates/Figure 02 structural estimates.R` | `output/Figure 02.pdf` |

### Online Appendix

| Figure | Title | Program | Output File |
|--------|-------|---------|-------------|
| Figure A.1 | Singleton Cells and Match Rates by Town Size | `code/AA Figure A01 match rates by town pop/AA Figure A01 match rates by town pop.R` | `output/Figure A01.pdf` |
| Figure A.2 | Changes in Ethnic Identification by Education and Occupation, 1992–2011 (locality gender-ratio adjusted) | `code/AA Figure A02 gender-adjusted scatterplot/Figure A02.R` | `output/Figure A02.pdf` |
| Figure A.3 | Changes in Ethnic Identification by Education and Occupation: Household Heads Only (1992–2011) | `code/AA Figure A03 household head scatterplot/Figure A03 household heads.R` | `output/Figure A03.pdf` |
| Figure A.4 | Changes in Ethnic Identification by Education: Other Ethnicities (1992–2011) | `code/01 Figure 01 A04 A05 A06 scatterplots/Figure 01 and A04 A05 A06 scatterplots.R` | `output/Figure A04.pdf` |
| Figure A.5 | Changes in Ethnic Identification by Education (2002–2011) | `code/01 Figure 01 A04 A05 A06 scatterplots/Figure 01 and A04 A05 A06 scatterplots.R` | `output/Figure A05.pdf` |
| Figure A.6 | Changes in Ethnic Identification by Education: Other Ethnicities (2002–2011) | `code/01 Figure 01 A04 A05 A06 scatterplots/Figure 01 and A04 A05 A06 scatterplots.R` | `output/Figure A06.pdf` |
| Figure A.7 | Sample Representativeness: Self-Reported Roma Individuals | `code/AA Figure A07 A08 sample representativeness/Figures A07 A08 sample representativeness.R` | `output/Figure A07.pdf` |
| Figure A.8 | Sample Representativeness: All Individuals | `code/AA Figure A07 A08 sample representativeness/Figures A07 A08 sample representativeness.R` | `output/Figure A08.pdf` |
| Figure A.9 | Structural Model Fit | `code/AA Figure A09 structural model fit/Figure A09 structural model fit.R` | `output/Figure A09.pdf` |

---

## References

Mitrut, Andreea, Gabriel Kreindler, Margareta Matache, Andrei Munteanu, and Cristian Pop-Eleches. "Education and Selection into Ethnic Identification: Evidence from Roma People in Romania." *American Economic Journal: Applied Economics* (forthcoming).

---

## Package Versions

The results in the paper were produced with **R 4.4.2** on the Harvard FASRC cluster (20 cores, 150 GB RAM) via RStudio. The following package versions were loaded (from `sessionInfo()`):

| Package | Version |
|---------|---------|
| data.table | 1.18.4 |
| fixest | 0.12.1 |
| xtable | 1.8-4 |
| patchwork | 1.3.0 |
| modelsummary | 2.6.0 |
| MASS | 7.3-64 |
| haven | 2.5.4 |
| foreign | 0.8-88 |
| dplyr | 1.1.4 |
| kableExtra | 1.4.0 |
| ggplot2 | 3.5.1 |
| ggh4x | 0.3.0 |
| gridExtra | 2.3 |
| scales | 1.3.0 |
| cowplot | 1.1.3 |
| effects | 4.2-2 |
| carData | 3.0-5 |
| sf | 1.0-19 |
| maps | 3.4.2.1 |
| ggrepel | 0.9.6 |
| tidyverse | 2.0.0 |
| tibble | 3.2.1 |
| tidyr | 1.3.1 |
| readr | 2.1.5 |
| purrr | 1.0.4 |
| stringr | 1.5.1 |
| forcats | 1.0.0 |
| lubridate | 1.9.4 |
| DescTools | 0.99.59 |
| corrplot | 0.95 |
| triangle | 1.0 |
| GA | 3.2.4 |
| foreach | 1.5.2 |
| iterators | 1.0.14 |
| quadprog | 1.5-8 |
| Hmisc | 5.2-2 |

