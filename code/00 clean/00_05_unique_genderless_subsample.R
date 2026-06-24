# =====================================================================
# Slim down the genderless unique-cell files to a lightweight subsample
# Produces:  data_2002_unique_genderless_subsample.csv,
#            data_1992_unique_genderless_subsample.csv, and two 2011 subsample
#            files; intermediate, written to each wave's data directory
# Inputs:    data_2002_unique_genderless.csv, data_1992_unique_genderless.csv,
#            data_2011_unique_02_genderless.csv, data_2011_unique_92_genderless.csv
#            (outputs of 00_05_unique_genderless.R)
# Summary:   Reads each genderless unique-cell file and keeps only a small set
#            of columns (locality codes, cell IDs, EDUC, ROMA, SEX, population
#            counts, years), then rewrites them as *_subsample files for lighter
#            downstream use. No paper exhibit.
# =====================================================================

#data ----
setwd(wd_data_02)
data_02_genderless<-read_sample('data_2002_unique_genderless.csv')
data_02_genderless<-data_02_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
data_02_genderless<-read_data('data_2002_unique_genderless.csv',data_02_genderless)


setwd(wd_data_92)
data_92_genderless<-read_sample('data_1992_unique_genderless.csv') 
data_92_genderless<-data_92_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
data_92_genderless<-read_data('data_1992_unique_genderless.csv',data_92_genderless)


setwd(wd_data_11)
data_11_02_genderless<-read_sample('data_2011_unique_02_genderless.csv') 
data_11_02_genderless<-data_11_02_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
data_11_02_genderless<-read_data('data_2011_unique_02_genderless.csv',data_11_02_genderless)

setwd(wd_data_11)
data_11_92_genderless<-read_sample('data_2011_unique_92_genderless.csv') 
data_11_92_genderless<-data_11_92_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
data_11_92_genderless<-read_data('data_2011_unique_92_genderless.csv',data_11_92_genderless)


setwd(wd_data_11)
fwrite(data_11_92_genderless,'data_2011_unique_92_genderless_subsample.csv')
fwrite(data_11_02_genderless,'data_2011_unique_02_genderless_subsample.csv')
setwd(wd_data_92)
fwrite(data_92_genderless,'data_1992_unique_genderless_subsample.csv')
setwd(wd_data_02)
fwrite(data_02_genderless,'data_2002_unique_genderless_subsample.csv')

