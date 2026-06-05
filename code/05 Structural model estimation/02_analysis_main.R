# Post-estimation structural model analysis; uses saved .rds results; produces output for Figure 2
# --- Setup ---

# wd_structural: root directory for structural model scripts and saved .rds results
wd_structural<-paste0(wd_code,'/05 Structural model estimation/')
setwd(wd_structural)

# --- Load Full-Population Census Cross-Sections for Extrapolation ---
# These unconditional cross-sections (all ethnicities, all ages) are used to
# compute education-group population shares for extrapolating structural estimates
# from the matched Roma sample to the full Roma population.

#import 2011 data for extrapolating estimates to full pop
setwd(wd_data_11)
filename<-"data_2011_clean.csv"
# First pass: read only the columns needed, using the lightweight read_sample() helper
data_all_2011<-read_sample(filename) %>%
  dplyr::select(EDUC,cell_id_1992,cell_id_2002,ROMA,AA,SCU)
# Second pass: read_data() applies locality-code harmonisation and other cleaning;
# the mutate block corrects SCU coding artifacts for young adults (age 18-25):
# individuals in that age window whose raw SCU suggests incomplete schooling are
# reclassified to the highest plausible EDUC level they could have attained.
data_all_2011<-read_data(filename,data_all_2011) %>%
  mutate(EDUC=case_when((2011-AA) %in% 18:25 & SCU<=65 & SCU>0 ~ "Postsec",
                        (2011-AA) %in% 18:25 & SCU<=92 & SCU>0 ~ "General HS",
                        TRUE ~ EDUC)) %>%
  # Set EDUC as an ordered factor so education-group aggregations respect the
  # natural ordering used throughout the paper.
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))

#2002
setwd(wd_data_02)
filename<-"data_2002_clean.csv"
data_all_2002<-read_sample(filename) %>%
  dplyr::select(EDUC,cell_id,ROMA,AA,SCU)
# SCU thresholds differ across census waves; 2002 uses codes <=49 for Postsec
# and <=94 for General HS in the young-adult correction.
data_all_2002<-read_data(filename,data_all_2002) %>%
  mutate(EDUC=case_when((2002-AA) %in% 18:25 & SCU<=49 & SCU>0 ~ "Postsec",
                        (2002-AA) %in% 18:25 & SCU<=94 & SCU>0 ~ "General HS",
                        TRUE ~ EDUC)) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))

#1992
setwd(wd_data_92)
filename<-"data_1992_clean.csv"
data_all_1992<-read_sample(filename) %>%
  dplyr::select(EDUC,cell_id,ROMA,AA,SCU)
# 1992 SCU codes: <=61 maps to Postsec, <=93 maps to General HS for young adults.
data_all_1992<-read_data(filename,data_all_1992) %>%
  mutate(EDUC=case_when((1992-AA) %in% 18:25 & SCU<=61 & SCU>0 ~ "Postsec",
                        (1992-AA) %in% 18:25 & SCU<=93 & SCU>0 ~ "General HS",
                        TRUE ~ EDUC)) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))


# --- Render Structural Model Reports ---
# Each .Rmd loads its own set of .rds bootstrap results (saved by 01_estimation_*.R),
# computes model-implied passing rates by education group, and produces the panels
# for Figure 2 under a different distributional assumption for cost heterogeneity.
setwd(wd_structural)
# Normal distribution assumption for the utility cost of Roma identification
rmarkdown::render("02_analysis_normal.Rmd",knit_root_dir = getwd())
# Uniform distribution assumption
rmarkdown::render("02_analysis_uniform.Rmd",knit_root_dir = getwd())
# Triangular distribution assumption
rmarkdown::render("02_analysis_triangle.Rmd",knit_root_dir = getwd())
# Lognormal distribution assumption
rmarkdown::render("02_analysis_lognormal.Rmd",knit_root_dir = getwd())

# --- Superseded heterogeneity-grid robustness renders (archived) ---
# setwd(wd_structural)
# rmarkdown::render("model_01_analysis_v5_het000.Rmd",knit_root_dir = getwd())
# rmarkdown::render("model_01_analysis_v5_het1000.Rmd",knit_root_dir = getwd())
# rmarkdown::render("model_01_analysis_v5_het500.Rmd",knit_root_dir = getwd())
# rmarkdown::render("model_01_analysis_v5_het250.Rmd",knit_root_dir = getwd())
# # rmarkdown::render("model_01_analysis_v5_het100.Rmd",knit_root_dir = getwd())
# # rmarkdown::render("model_01_analysis_v5_het200.Rmd",knit_root_dir = getwd())
