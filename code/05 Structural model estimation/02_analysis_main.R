# =====================================================================
# Post-estimation analysis driver for the structural model.
# Produces:  rendered HTML reports (one per heterogeneity distribution) via the
#            02_analysis_*.Rmd files
# Inputs:    cleaned 1992/2002/2011 census files (data_1992/2002/2011_clean.csv)
#            and the saved estimation results (.rds) read inside the Rmd reports
# Summary:   Loads each census round and recodes education (reassigning some
#            young adults to Postsec / General HS), then renders the
#            distribution-specific analysis reports that extrapolate the
#            estimated latent Roma shares to the full population.
# =====================================================================

# ---- Packages and paths ----
library(pacman)
pacman::p_load(tidyverse,data.table,fixest,xtable,modelsummary,haven,GA,dtplyr,foreach,doParallel,kableExtra,Hmisc)

wd_structural<-paste0(wd_code,'/05 Structural model estimation/')
setwd(wd_structural)

# ---- Load and recode 2011 census ----
# Read a column subset, then for young adults (aged 18-25 in 2011) reclassify
# education to Postsec or General HS based on the schooling-years code (SCU);
# finally fix the EDUC factor level order used throughout the reports.
#import 2011 data for extrapolating estimates to full pop
setwd(wd_data_11)
filename<-"data_2011_clean.csv"
data_all_2011<-read_sample(filename) %>%
  dplyr::select(EDUC,cell_id_1992,cell_id_2002,ROMA,AA,SCU) 
data_all_2011<-read_data(filename,data_all_2011) %>%
  mutate(EDUC=case_when((2011-AA) %in% 18:25 & SCU<=65 & SCU>0 ~ "Postsec",
                        (2011-AA) %in% 18:25 & SCU<=92 & SCU>0 ~ "General HS",
                        TRUE ~ EDUC)) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))

# ---- Load and recode 2002 census (same recoding logic, 2002 thresholds) ----
#2002
setwd(wd_data_02)
filename<-"data_2002_clean.csv"
data_all_2002<-read_sample(filename) %>%
  dplyr::select(EDUC,cell_id,ROMA,AA,SCU) 
data_all_2002<-read_data(filename,data_all_2002) %>%
  mutate(EDUC=case_when((2002-AA) %in% 18:25 & SCU<=49 & SCU>0 ~ "Postsec",
                        (2002-AA) %in% 18:25 & SCU<=94 & SCU>0 ~ "General HS",
                        TRUE ~ EDUC)) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))

# ---- Load and recode 1992 census (same recoding logic, 1992 thresholds) ----
#1992
setwd(wd_data_92)
filename<-"data_1992_clean.csv"
data_all_1992<-read_sample(filename) %>%
  dplyr::select(EDUC,cell_id,ROMA,AA,SCU) 
data_all_1992<-read_data(filename,data_all_1992) %>%
  mutate(EDUC=case_when((1992-AA) %in% 18:25 & SCU<=61 & SCU>0 ~ "Postsec",
                        (1992-AA) %in% 18:25 & SCU<=93 & SCU>0 ~ "General HS",
                        TRUE ~ EDUC)) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))


# ---- Render the four distribution-specific analysis reports ----
# Each Rmd reads its matching results .rds and produces an HTML report.
#get reports
setwd(wd_structural)
rmarkdown::render("02_analysis_normal.Rmd",knit_root_dir = getwd())
rmarkdown::render("02_analysis_uniform.Rmd",knit_root_dir = getwd())
rmarkdown::render("02_analysis_triangle.Rmd",knit_root_dir = getwd())
rmarkdown::render("02_analysis_lognormal.Rmd",knit_root_dir = getwd())