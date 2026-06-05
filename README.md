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

Individual-level microdata from the Romanian Population and Housing Censuses of 1992, 2002, and 2011. These data are confidential and were obtained under a data-sharing agreement with the Romanian National Institute of Statistics (Institutul Național de Statistică, INS).

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
- **Included in package:** No (confidential)

### Administrative Reference Data

SIRUTA codes (Romania's standard locality classification system) and census nomenclators (school, occupation, and ethnicity codes) used to harmonize geographic and demographic variables across census years.

- **Provider:** Romanian National Institute of Statistics (INS)
- **Included in package:** No

---

## Dataset List

| Data File | Source | Provided | Description |
|-----------|--------|----------|-------------|
| 1992 census microdata | INS Romania | No | DBF files; individual-level records: demographics, education, ethnicity, occupation |
| 2002 census microdata | INS Romania | No | DBF files; individual-level records: demographics, education, ethnicity, occupation |
| 2011 census microdata | INS Romania | No | DBF files; individual-level records: demographics, education, ethnicity, occupation, ancestry |
| Birth records 2003–2011 | INS Romania | No | DTA files; birth certificates with mother's ethnicity and education |
| Census–birth linkage file | INS Romania | No | Crosswalk matching birth records to 2011 census individuals |
| SIRUTA locality codes | INS Romania | No | Standard locality classification system for Romania |
| Census nomenclators | INS Romania | No | Code-to-label mappings for census variables (school, ethnicity, occupation) |
| Shapefiles (Romania localities) | INS Romania | No | Geographic boundaries for mapping |
| `baza_Link1.sav`, `baza_Link2.sav` | Authors | No | Survey data on perceptions of Roma ethnicity, two experimental arms |

---

## Computational Requirements

### Software

- **R** version 4.3 or later (tested on R 4.x)

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

### Hardware Requirements

- **Reduced-form analysis** (data reading, cleaning, all main and appendix tables/figures except structural): standard desktop or laptop; 16 GB RAM recommended given the size of census microdata.
- **Structural model estimation** (`code/05 Structural model estimation/`): requires a high-performance computing (HPC) cluster with parallel processing. Estimation scripts use `doParallel` with 20 parallel workers. Each distributional specification (normal, uniform, lognormal, triangle) runs bootstrap iterations across education levels. Analysis and figure scripts in Section 05 can use pre-computed `.rds` results files if available in `data/processed/results/`.

**Note on thread settings:** `code/functions.R` sets `setDTthreads(threads=36)`. Adjust this to match available hardware before running.

### Estimated Runtime

| Component | Approximate Time | Hardware |
|-----------|-----------------|---------|
| Read raw census data (`00 read raw/`) | 30–60 min | Desktop, 16 GB RAM |
| Clean and link data (`00 clean/`) | 30–60 min | Desktop, 16 GB RAM |
| Reduced-form analysis (all other scripts) | 1–2 hours total | Desktop |
| Structural model estimation (`05 Structural model estimation/`) | Several hours | HPC, 20+ cores |

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
│   ├── 05 Structural model estimation/               Structural model data prep, estimation, analysis (17 scripts)
│   ├── 06 Figure 02 structural estimates/            Figure 2 (1 script)
│   ├── 07 Table 04 survey/                           Table 4 and locality shares (2 scripts)
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
│   └── AA text/                                      In-text statistics (4 scripts)
├── data/
│   ├── raw/                                          Raw data (not provided; see Data Availability)
│   │   ├── 1992/                                     1992 census DBF files
│   │   ├── 2002/                                     2002 census DBF files
│   │   ├── 2011/                                     2011 census DBF files
│   │   ├── Birth/                                    Birth records DTA files
│   │   ├── other/{1992,2002,2011}/                   Census nomenclators and SIRUTA codes
│   │   ├── survey/                                   Survey SPSS files
│   │   └── shapefiles/                               Geographic boundary files
│   └── processed/                                    Intermediate datasets (generated by code)
│       ├── linked/                                   Linked individual records across census years
│       ├── structural/                               Intermediate structural model data
│       └── results/                                  Model estimation results (.rds files)
└── output/                                           Figures (.pdf) and tables (.tex) produced by code
```

### Execution Model

`code/main.R` loads all required packages and defines all working directory path variables. It does **not** source the analysis scripts. Each analysis script reads the paths defined in `main.R` (via `source('functions.R')` which also sets thread options). The correct execution order is determined by the folder numbering: `00 read raw` → `00 clean` → `01` → `02` → … → `07` → `AA` scripts. Within-folder script order follows filename prefixes where multiple scripts are present.

### Structural Model

`code/05 Structural model estimation/` contains:
- `00_main_structural.R` — prepares estimation input data from cleaned linked census data
- `01_estimate_model_het_parallel.R` (and variants: `_lognormal`, `_triangle`, `_uniform`) — bootstrap estimation under four distributional assumptions; saves results to `data/processed/results/`
- `02_analysis_main.R` — post-estimation analysis using saved results; feeds Figure 2

Estimation scripts require an HPC environment. If pre-computed results (`.rds` files) are available in `data/processed/results/`, the analysis and figure scripts can be run directly without re-estimating.

---

## Instructions to Replicators

1. **Obtain data access** from the Romanian National Institute of Statistics (INS). See the Data Availability section for contact details.

2. **Place raw data files** in the corresponding subdirectories under `data/raw/`:
   - Census microdata: `data/raw/1992/`, `data/raw/2002/`, `data/raw/2011/`
   - Birth records: `data/raw/Birth/`
   - Census nomenclators: `data/raw/other/1992/`, `data/raw/other/2002/`, `data/raw/other/2011/`
   - Survey data: `data/raw/survey/`
   - Shapefiles: `data/raw/shapefiles/`

3. **Update working directory paths** in `code/main.R`. Replace the `wd` and `wd_data_raw` variables (lines 30–31) with the local path to the `replication_final/` folder and the raw data directory, respectively. All other paths are constructed from `wd` and do not need modification.

4. **Adjust thread count** in `code/functions.R`: set `setDTthreads(threads=N)` to match available cores on the local machine.

5. **Install R packages** by running `code/main.R`. Missing packages will be installed automatically via `pacman::p_load()`.

6. **Run scripts** in the order indicated by folder numbering (see Directory Structure above). Start with `00 read raw/`, then `00 clean/`, then folders `01` through `07`, then `AA` folders.

7. **Structural model:** The estimation scripts in `code/05 Structural model estimation/` require an HPC cluster (see Computational Requirements). If pre-computed estimation results are available, skip the `01_estimate_model_het_parallel*.R` scripts and run `02_analysis_main.R` and `code/06 Figure 02 structural estimates/Figure 02 structural estimates.R` directly.

8. **Verify outputs** by comparing files in `output/` with those listed below.

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
| Table A.9 | Changes in Ethnic Identification by Education — Different Specifications (1992–2011) | `code/AA Table A09 IV household enumerator and non declaration/AA Table 09 household enumerator and non declaration.R` | `output/Table A09.tex` |
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
| Figure A.9 | Structural Model Fit | `code/AA Figure A09 structural model fit/Figure 09 structural model fit.R` | `output/Figure A09.pdf` |

---

## References

Mitrut, Andreea, Gabriel Kreindler, Margareta Matache, Andrei Munteanu, and Cristian Pop-Eleches. "Education and Selection into Ethnic Identification: Evidence from Roma People in Romania." *American Economic Journal: Applied Economics* (forthcoming).
