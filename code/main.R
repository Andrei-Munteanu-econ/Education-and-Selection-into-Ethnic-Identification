# install.packages("dplyr","tidyverse","data.table","fixest","xtable","modelsummary",
#                  "rddensity","rdd","rdrobust","haven","foreign","dplyr","eeptools","svglite","lubridate","kableExtra",
#                  "ggh4x","pacman","gridExtra","scales")
pacman::p_load(data.table,
               fixest,
               xtable,
               patchwork,
               modelsummary,
               MASS,#logistic regression
               haven,foreign,dplyr,kableExtra,
               ggh4x,gridExtra,scales,
               cowplot,  #for better ggplot axes and grids
               effects, #plotr logit output
               sf,#shapefile
               maps,#maps
               # rgdal,#open shapefile
               ggrepel,#label points in ggplot
               tidyverse,
               DescTools,#winsorize
               corrplot,
               triangle,
               GA,
               quadprog,
               Hmisc#correlation matrix
               ) 




wd<-'/n/holystore01/LABS/kreindler_lab/Lab/romapassing/replication_final/'
wd_data_raw<-'/n/holystore01/LABS/kreindler_lab/Lab/romapassing/rawdata'
wd_code<-paste0(wd,'/code/')
wd_output<-paste0(wd,'/output/')

#raw data

wd_data_92<-paste0(wd,'/data/raw/1992')
wd_data_02<-paste0(wd,'/data/raw/2002')
wd_data_11<-paste0(wd,'/data/raw/2011')
wd_data_births<-paste0(wd,'/data/raw/Birth/')
#folders with census-specific codebooks
wd_data_92_other<-paste0(wd,'/data/raw/other/1992')
wd_data_02_other<-paste0(wd,'/data/raw/other/2002')
wd_data_11_other<-paste0(wd,'/data/raw/other/2011')
wd_data_survey<-paste0(wd,'/data/raw/survey/')
wd_data_shapefiles<-paste0(wd,'/data/raw/shapefiles/')
wd_data_elections_raw<-paste0(wd,'/data/raw/elections/')


#final data
wd_data_linked<-paste0(wd,'/data/processed/linked/')
wd_data_structural<-paste0(wd,'/data/processed/structural/')
wd_data_results<-paste0(wd,'/data/processed/results/')
wd_data_survey_processed<-paste0(wd,'/data/processed/survey/')

setwd(wd_code)
source('functions.R')


# =====================================================================
# Run the full pipeline
# =====================================================================
# Scripts are sourced in the folder-numbering order documented in the README
# (00 read raw -> 00 clean -> 01..07 -> AA). `run()` sources each script with
# chdir = TRUE so that (a) scripts using a relative source() resolve correctly
# and (b) each script's own setwd(wd_data_*) / setwd(wd_output) still applies.
run <- function(rel) {
  cat("\n=== Running:", rel, "===\n")
  source(file.path(wd_code, rel), chdir = TRUE)
}

# Structural estimation (05/01_estimate_*) is computationally heavy and requires
# an HPC cluster. Leave FALSE to use the precomputed results in
# data/processed/results/; set TRUE to re-estimate from scratch (see README,
# "Instructions to Replicators", step 7).
estimate_structural <- FALSE

# The survey locality Roma-share lookup (07/00_roma_share_by_locality.R) needs the
# confidential 2011 census. Leave FALSE to use the provided, committed
# data/processed/survey/siruta3_roma_survey.csv; set TRUE to regenerate it (HPC).
regenerate_siruta_share <- FALSE


# ---- 00 read raw: read raw census DBF/birth files -> intermediate CSVs ----
run("00 read raw/read_92_full.R")
run("00 read raw/read_92_common.R")
run("00 read raw/read_02_full.R")
run("00 read raw/read_02_common.R")
run("00 read raw/read_11_full.R")
run("00 read raw/read_11_common.R")
run("00 read raw/read_11_migrants.R")

# ---- 00 clean: harmonize, match births, build linked panels ----
run("00 clean/00_01clean.R")
run("00 clean/00_02births_match_v3.R")
run("00 clean/00_03add_births_2011_v3.R")
run("00 clean/00_04roma.R")
run("00 clean/00_05_unique.R")
run("00 clean/00_05_non_unique.R")
run("00 clean/00_05_unique_genderless.R")
run("00 clean/00_05_unique_genderless_subsample.R")

# ---- 01-04: main-text figures and tables ----
run("01 Figure 01 A04 A05 A06 scatterplots/Figure 01 and A04 A05 A06 scatterplots.R")
run("02 Table 01 main iv results/Table 01 main iv results.R")
run("03 Table 02 A08 heterogeneity and town size/Table 02 A08 heterogeneity and town size.R")
run("04 Table 03 marriage/04 Table 03 marriage.R")

# ---- 05 structural model: definitions, (optional) estimation, analysis ----
run("05 Structural model estimation/00_main_structural.R")
if (estimate_structural) {
  run("05 Structural model estimation/01_estimate_model_het_parallel.R")
  run("05 Structural model estimation/01_estimate_model_het_parallel_uniform.R")
  run("05 Structural model estimation/01_estimate_model_het_parallel_lognormal.R")
  run("05 Structural model estimation/01_estimate_model_het_parallel_triangle.R")
}
run("05 Structural model estimation/02_analysis_main.R")
run("05 Structural model estimation/02_graph_compare_uniform_normal.R")

# ---- 06-07: structural figure and survey ----
run("06 Figure 02 structural estimates/Figure 02 structural estimates.R")
if (regenerate_siruta_share) run("07 Table 04 survey/00_roma_share_by_locality.R")
run("07 Table 04 survey/00_anonymize_survey.R")   # builds anonymized survey CSVs
run("07 Table 04 survey/Table 04 survey.R")

# ---- AA: appendix tables, figures, and in-text statistics ----
run("AA Table A01 mismatch rates/Table A01 mismatch rates.R")
run("AA Table A02 mismatch Roma by education/Table A02 mismatch Roma by education.R")
run("AA Table A03 conditional independence mismatch/Table A03 conditional independence mismatch.R")
run("AA Table A04 IV mothers sample/Table A04 IV mothers sample.R")
run("AA Table A05 no fixed effects/Table A05 no fixed effects.R")
run("AA Table A06 2002-2011/Table A06 2002-2011.R")
run("AA Table A07 mother tongue/Table A07 mother tongue.R")
run("AA Table A09 IV household enumerator and non declaration/AA Table A09 household enumerator and non declaration.R")
run("AA Table A10 IV siblings/Table A10 IV siblings.R")
run("AA Table A11 marriage/Table A11 marriage.R")
run("AA Table A12 structural parameters distribution/Table A12 structural parameters distribution.R")
run("AA Table A13 survey ethnic identification and education/Table A13.R")
run("AA Table A14 survey Roma markers/Table A14 survey Roma markers.R")
run("AA Figure A01 match rates by town pop/AA Figure A01 match rates by town pop.R")
run("AA Figure A02 gender-adjusted scatterplot/Figure A02.R")   # sources locality_gender_ratios.R
run("AA Figure A03 household head scatterplot/Figure A03 household heads.R")
run("AA Figure A07 A08 sample representativeness/Figures A07 A08 sample representativeness.R")
run("AA Figure A09 structural model fit/Figure A09 structural model fit.R")
run("AA text/text match rates and sample sizes for use in text.R")
run("AA text/text percent with no reported ethnicity in 2011.R")
run("AA text/text share of male hh heads.R")
run("AA text/referee response triple match-vsn sample size.R")
run("AA text/text survey easy to identify Roma.R")
