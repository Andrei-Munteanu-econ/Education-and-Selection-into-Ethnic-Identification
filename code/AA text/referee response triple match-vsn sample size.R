# =====================================================================
# Referee response — sample size of triple-matched individuals in VSN data
# Produces:  console output only (no files written)
# Inputs:    data_1992_2002_2011_unique.csv (triple-linked 1992/2002/2011
#            sample), matches_census_final.csv (census-to-census linkages,
#            including birth-record / VSN mother ids)
# Summary:   Identifies triple-matched individuals who also appear as
#            mothers in the VSN (birth-record) linkage, then counts how
#            many are Roma in 1992, Roma in any of the three censuses, and
#            how many of the latter have at least a university education.
# =====================================================================

# ---- Load the triple-matched (1992-2002-2011) sample ----
setwd(wd_data_linked)
data<-fread('data_1992_2002_2011_unique.csv')

#load linkages
setwd(wd_data_11)
filename<-'matches_census_final.csv'
lnk<-read_sample(filename) %>%
  select(id11,id11_MOM,copil_id)
lnk<-read_data(filename,lnk) 

#find triple matched sample inidividuals who are in vsn data
matched<-data %>%
  filter(id11 %in% id11_MOM)

#how many of them are Roma in 1992?
matched_Roma<-matched %>%
  filter(ROMA_1992==T)

#how many are Roma in 1992, 2002, or 2011?
matched_roma_any<- matched %>%
  filter(ROMA_1992==T | ROMA_2002==T | ROMA_2011==T)

#how many of these have university and above?
matched_roma_any<- matched %>%
  filter(years_2011>=16)