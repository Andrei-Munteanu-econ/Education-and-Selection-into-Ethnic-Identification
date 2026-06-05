# --- Package Installation (commented out; run once manually if needed) ---
# install.packages("dplyr","tidyverse","data.table","fixest","xtable","modelsummary",
#                  "rddensity","rdd","rdrobust","haven","foreign","dplyr","eeptools","svglite","lubridate","kableExtra",
#                  "ggh4x","pacman","gridExtra","scales")

# --- Library Loading ---
# pacman::p_load installs missing packages then loads all listed packages
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
               Hmisc,#correlation matrix
               doParallel,#parallel backend for structural model bootstrap
               foreach,   #parallel foreach loops in structural model estimation
               rmarkdown, #render() calls in 02_analysis_main.R
               dtplyr,    #data.table backend for dplyr (structural model)
               EnvStats   #statistical utilities used in structural model
               )




# --- Path Configuration ---
# wd: root of the replication package on the cluster (Harvard FASRC / Holystore)
wd<-'/n/holystore01/LABS/kreindler_lab/Lab/romapassing/replication_final/'
# wd_data_raw: external raw census microdata, stored outside the replication root
wd_data_raw<-'/n/holystore01/LABS/kreindler_lab/Lab/romapassing/rawdata'
wd_code<-paste0(wd,'/code/')
wd_output<-paste0(wd,'/output/')

#raw data
# Census microdata directories, one per census year (1992, 2002, 2011)
wd_data_92<-paste0(wd,'/data/raw/1992')
wd_data_02<-paste0(wd,'/data/raw/2002')
wd_data_11<-paste0(wd,'/data/raw/2011')
# Birth registry microdata (used for demographic controls and cohort checks)
wd_data_births<-paste0(wd,'/data/raw/Birth/')
#folders with census-specific codebooks
wd_data_92_other<-paste0(wd,'/data/raw/other/1992')
wd_data_02_other<-paste0(wd,'/data/raw/other/2002')
wd_data_11_other<-paste0(wd,'/data/raw/other/2011')
# Survey data directory (supplementary ethnicity survey used in robustness checks)
wd_data_survey<-paste0(wd,'/data/raw/survey/')
# Shapefiles for Romania LAU2/commune boundaries (used in geographic figures)
wd_data_shapefiles<-paste0(wd,'/data/raw/shapefiles/')
wd_data_elections_raw<-paste0(wd,'/data/raw/elections/')


#final data
# wd_data_linked: cross-census matched panel (1992-2002 and 1992-2011 linkages)
wd_data_linked<-paste0(wd,'/data/processed/linked/')
# wd_data_structural: inputs and intermediate objects for the structural mixture model
wd_data_structural<-paste0(wd,'/data/processed/structural/')
# wd_data_results: .rds outputs from regression, IV, and structural estimation routines
wd_data_results<-paste0(wd,'/data/processed/results/')

# Set working directory to code folder so all source() calls resolve relative to it
setwd(wd_code)
# Load project-specific helper functions (cleaning utilities, custom GOF rows, bootstrap wrappers, etc.)
source('functions.R')


# =============================================================================
# ANALYSIS PIPELINE
# Run this file from wd_code (already set above). All source() paths are
# relative to that directory. Sections must be run in order; each depends on
# output files written by the preceding section.
#
# NOTE (Section 4): The 01_estimate_* scripts require an HPC cluster with
# 20+ cores and take several hours per distributional specification.
# If pre-computed .rds files are already in wd_data_results, comment out
# those four lines and run 02_analysis_main.R directly.
# =============================================================================

# --- Section 1: Read Raw Census Microdata ---
# Reads DBF files from wd_data_raw and writes cleaned CSVs to wd_data_92/02/11.
source('00 read raw/read_92_common.R')
source('00 read raw/read_92_full.R')
source('00 read raw/read_02_common.R')
source('00 read raw/read_02_full.R')
source('00 read raw/read_11_common.R')
source('00 read raw/read_11_full.R')
source('00 read raw/read_11_migrants.R')

# --- Section 2: Clean and Link Data ---
# Harmonises education codes, constructs matching cells, and links records
# across census years. Outputs go to wd_data_11/02/92 and wd_data_linked.
source('00 clean/00_01clean.R')
source('00 clean/00_02births_match_v3.R')
source('00 clean/00_03add_births_2011_v3.R')
source('00 clean/00_04roma.R')
source('00 clean/00_05_non_unique.R')
source('00 clean/00_05_unique.R')
source('00 clean/00_05_unique_genderless.R')
source('00 clean/00_05_unique_genderless_subsample.R')

# --- Section 3: Main Text Figures and Tables ---
source('01 Figure 01 A04 A05 A06 scatterplots/Figure 01 and A04 A05 A06 scatterplots.R')
source('02 Table 01 main iv results/Table 01 main iv results.R')
source('03 Table 02 A08 heterogeneity and town size/Table 02 A08 heterogeneity and town size.R')
source('04 Table 03 marriage/04 Table 03 marriage.R')

# --- Section 4: Structural Model ---
# 00_main_structural.R prepares estimation inputs; can be run on any machine.
source('05 Structural model estimation/00_main_structural.R')
# HPC required for the four lines below (20+ cores, several hours each).
# Comment out if pre-computed .rds files are already in wd_data_results.
source('05 Structural model estimation/01_estimate_model_het_parallel.R')
source('05 Structural model estimation/01_estimate_model_het_parallel_lognormal.R')
source('05 Structural model estimation/01_estimate_model_het_parallel_triangle.R')
source('05 Structural model estimation/01_estimate_model_het_parallel_uniform.R')
source('05 Structural model estimation/02_analysis_main.R')

# --- Section 5: Structural Figure and Survey Table ---
source('06 Figure 02 structural estimates/Figure 02 structural estimates.R')
source('07 Table 04 survey/00_roma_share_by_locality.R')
source('07 Table 04 survey/Table 04 survey.R')

# --- Section 6: Appendix Figures ---
source('AA Figure A01 match rates by town pop/AA Figure A01 match rates by town pop.R')
# locality_gender_ratios.R must run before Figure A02.R (computes inputs used by A02)
source('AA Figure A02 gender-adjusted scatterplot/locality_gender_ratios.R')
source('AA Figure A02 gender-adjusted scatterplot/Figure A02.R')
source('AA Figure A03 household head scatterplot/Figure A03 household heads.R')
source('AA Figure A07 A08 sample representativeness/Figures A07 A08 sample representativeness.R')
source('AA Figure A09 structural model fit/Figure 09 structural model fit.R')

# --- Section 7: Appendix Tables ---
source('AA Table A01 mismatch rates/Table A01 mismatch rates.R')
source('AA Table A02 mismatch Roma by education/Table A02 mismatch Roma by education.R')
source('AA Table A03 conditional independence mismatch/Table A03 conditional independence mismatch.R')
source('AA Table A04 IV mothers sample/Table A04 IV mothers sample.R')
source('AA Table A05 no fixed effects/Table A05 no fixed effects.R')
source('AA Table A06 2002-2011/Table A06 2002-2011.R')
source('AA Table A07 mother tongue/Table A07 mother tongue.R')
source('AA Table A09 IV household enumerator and non declaration/AA Table 09 household enumerator and non declaration.R')
source('AA Table A10 IV siblings/Table A10 IV siblings.R')
source('AA Table A11 marriage/Table A11 marriage.R')
source('AA Table A12 structural parameters distribution/Table A12 structural parameters distribution.R')
source('AA Table A13 survey ethnic identification and education/Table A13.R')
source('AA Table A14 survey Roma markers/Table A14 survey Roma markers.R')

# --- Section 8: In-Text Statistics ---
source('AA text/text survey easy to identify Roma.R')
source('AA text/text match rates and sample sizes for use in text.R')
source('AA text/text percent with no reported ethnicity in 2011.R')
source('AA text/text share of male hh heads.R')
