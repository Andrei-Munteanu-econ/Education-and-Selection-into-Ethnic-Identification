# Adds birth-certificate ethnicity to 2011 census; outputs updated data_2011_clean_births.csv

# --- Load 2011 Census Data ---

#load census data
setwd(wd_data_11)
filename<-'data_2011_clean_v2.csv'
# read_sample() reads a header/sample to infer column types before full load
data_2011<-read_sample(filename)
data_2011<-read_data(filename,data_2011)

# --- Load Birth-Certificate Data ---

#load birth data
setwd(wd_data_births)
# Birth records linked to 2011 census via CNP (personal numeric code); datan encodes birth date and sex
data_births<-read_dta('census2011_mergedwith_nv2003_2005_2011_cnp.dta')

# Parse the composite datan string to extract birth year, month/sex indicator, and ethnicity code
# datan format: YYYY-MM-ZZ (positions 1-4 = year, 6-7 = month/sex code, 9-10 = ethnicity code)
data_births<-data_births %>%
  mutate(AA_BIRTH=as.numeric(substr(datan,1,4)),
         LL_BIRTH=as.numeric(substr(datan,6,7)),
         # ZZ_BIRTH: ethnicity as recorded in the birth certificate (separate from self-reported census ZZ)
         ZZ_BIRTH=as.numeric(substr(datan,9,10)))

# --- Load Census-Birth Record Linkage ---

#load census-birth linkage
setwd(wd_data_11)
filename<-'matches_census_final.csv'
# id11: census 2011 individual identifier; id11_MOM: census ID of the mother; copil_id: child ID in birth records
lnk<-read_sample(filename) %>%
  select(id11,id11_MOM,copil_id)
lnk<-read_data(filename,lnk)



# --- Merge Census Records with Birth-Certificate Ethnicity ---

#link data
# Left join preserves all 2011 census records; unmatched individuals get NA for birth-certificate fields
# scoala_m: mother's education as recorded in the birth certificate (used as an alternative/robustness measure)
data_2011_linked<-data_2011 %>%
  left_join(lnk,by="id11",suffix=c("","_BC")) %>%
  left_join(data_births %>% select(nat,copil_id,AA_BIRTH,LL_BIRTH,ZZ_BIRTH,scoala_m ),by="copil_id",suffixes=c("","_BIRTH"))


# --- Write Output ---

setwd(wd_data_11)
# Output: 2011 census file augmented with birth-certificate ethnicity and mother's schooling
fwrite(data_2011_linked,"data_2011_clean_births.csv")
