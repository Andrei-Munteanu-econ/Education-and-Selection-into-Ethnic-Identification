# =====================================================================
# Append birth-certificate information to the cleaned 2011 census
# Produces:  data_2011_clean_births.csv  (intermediate; written to wd_data_11)
# Inputs:    cleaned 2011 census (data_2011_clean_v2.csv), birth records
#            (census2011_mergedwith_nv2003_2005_2011_cnp.dta), and the
#            census-birth linkage from step 00_02 (matches_census_final.csv)
# Summary:   Joins each 2011 census person to their matched birth certificate
#            via the linkage table, bringing in birth-certificate ethnicity
#            (nat), child birthdate, and mother's schooling. Writes the
#            enriched census file. No paper exhibit.
# =====================================================================

# ---- Load cleaned 2011 census ----
#load census data
setwd(wd_data_11)
filename<-'data_2011_clean_v2.csv'
data_2011<-read_sample(filename)
data_2011<-read_data(filename,data_2011)

#load birth data
setwd(wd_data_births)
data_births<-read_dta('census2011_mergedwith_nv2003_2005_2011_cnp.dta')

data_births<-data_births %>%
  mutate(AA_BIRTH=as.numeric(substr(datan,1,4)),
         LL_BIRTH=as.numeric(substr(datan,6,7)),
         ZZ_BIRTH=as.numeric(substr(datan,9,10)))

#load census-birth linkage
setwd(wd_data_11)
filename<-'matches_census_final.csv'
lnk<-read_sample(filename) %>%
  select(id11,id11_MOM,copil_id)
lnk<-read_data(filename,lnk) 



# ---- Join census to births via the linkage table and write ----
#link data
data_2011_linked<-data_2011 %>%
  left_join(lnk,by="id11",suffix=c("","_BC")) %>%
  left_join(data_births %>% select(nat,copil_id,AA_BIRTH,LL_BIRTH,ZZ_BIRTH,scoala_m ),by="copil_id",suffixes=c("","_BIRTH")) 
  

setwd(wd_data_11)
fwrite(data_2011_linked,"data_2011_clean_births.csv")