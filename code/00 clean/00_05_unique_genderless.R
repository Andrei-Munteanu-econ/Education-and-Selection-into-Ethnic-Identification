# =====================================================================
# Build the linked panel of UNIQUE birthdate-locality cells, ignoring sex
# Produces:  data_2002_2011_unique_genderless.csv,
#            data_1992_2011_unique_genderless.csv,
#            data_1992_2002_2011_unique_genderless.csv (and per-wave _unique /
#            _roma versions); intermediate, written to each dir and wd_data_linked
# Inputs:    data_2011_clean_births.csv, data_2002_clean.csv, data_1992_clean.csv
# Summary:   Same as 00_05_unique but uniqueness is defined on the genderless
#            cell ID (locality x birthdate, without sex). Keeps cells whose
#            genderless key is unique within a wave, merges 2011 to 2002, 2011
#            to 1992, and the triple intersection, and writes full and Roma-only
#            linked panels. No paper exhibit.
# =====================================================================

#data ----
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
data_2011<-read_sample(filename)
data_2011<-data_2011 %>%
  select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id11,NOP,id11_MOM,id11_POP,id11_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id_2002,cell_id_1992,cell_id_genderless_2002,cell_id_genderless_1992,SEX,SIRUTA,SIRSUP,SCA_MOM,SCA_POP,SCA_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,pop_SIRUTA_2011,pop_SIRSUP_2011,GRUD,STATUT,nat,copil_id,id11_MOM_BC,source,category,scoala_m,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_2011<-read_data(filename,data_2011)
data_2011<-data_2011 %>%  mutate(census='2011') 

setwd(wd_data_02)
filename<-'data_2002_clean.csv'
data_2002<-read_sample(filename)
data_2002<-data_2002 %>%
  select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id02,id02_MOM,id02_POP,id02_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_2002,pop_SIRSUP_2002,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE) 
data_2002<-read_data(filename,data_2002)
data_2002<-data_2002 %>%  mutate(census='2002') 

setwd(wd_data_92)
filename<-'data_1992_clean.csv'
data_1992<-read_sample(filename)
data_1992<-data_1992 %>%
  select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id92,id92_MOM,id92_POP,id92_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_1992,pop_SIRSUP_1992,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE) 
data_1992<-read_data(filename,data_1992)
data_1992<-data_1992 %>%  mutate(census='1992') 



# ---- Keep only cells with a unique (locality x birthdate) key per wave ----
### match 2002-2011 Roma
data_2002_unique<-data_2002 %>%
  filter(!cell_id_genderless %in% cell_id_genderless[duplicated(cell_id_genderless)])
setwd(wd_data_02)
fwrite(data_2002_unique,'data_2002_unique_genderless.csv')
fwrite(data_2002_unique %>% filter(ROMA==T),'data_2002_roma_unique_genderless.csv')

data_2011_unique_02<-data_2011 %>%
  filter(!cell_id_genderless_2002 %in% cell_id_genderless_2002[duplicated(cell_id_genderless_2002)]) 
setwd(wd_data_11)
fwrite(data_2011_unique_02,'data_2011_unique_02_genderless.csv')

#1992-2011
data_1992_unique<-data_1992 %>%
  filter(!cell_id_genderless %in% cell_id_genderless[duplicated(cell_id_genderless)]) 
setwd(wd_data_92)
fwrite(data_1992_unique,'data_1992_unique_genderless.csv')
fwrite(data_1992_unique %>% filter(ROMA==T),'data_1992_roma_unique_genderless.csv')

data_2011_unique_92<-data_2011 %>%
  filter(!cell_id_genderless_1992 %in% cell_id_genderless_1992[duplicated(cell_id_genderless_1992)])  
setwd(wd_data_11)
fwrite(data_2011_unique_92,'data_2011_unique_92_genderless.csv')

#triple
data_2011_unique<-data_2011 %>%
  filter(!cell_id_genderless_1992 %in% cell_id_genderless_1992[duplicated(cell_id_genderless_1992)]) %>%
  filter(!cell_id_genderless_2002 %in% cell_id_genderless_2002[duplicated(cell_id_genderless_2002)]) 
  


#merge----
##two by two----
setwd(wd_data_02)
data_2002_unique<-fread('data_2002_unique_genderless.csv')
setwd(wd_data_92)
data_1992_unique<-fread('data_1992_unique_genderless.csv')
setwd(wd_data_11)
data_2011_unique_02<-fread('data_2011_unique_02_genderless.csv')
data_2011_unique_92<-fread('data_2011_unique_92_genderless.csv')


data_2002_2011<-base::merge(data_2011_unique_02,data_2002_unique,by.x="cell_id_genderless_2002",by.y="cell_id_genderless",suffixes=c("_2011","_2002")) %>% mutate(census=2002)
data_1992_2011<-base::merge(data_2011_unique_92,data_1992_unique,by.x="cell_id_genderless_1992",by.y="cell_id_genderless",suffixes=c("_2011","_1992")) %>% mutate(census=1992)

setwd(wd_data_linked)
fwrite(data_2002_2011,'data_2002_2011_unique_genderless.csv')
fwrite(data_1992_2011,'data_1992_2011_unique_genderless.csv')

##triple sample----
data_triple<-data_2011_unique %>% select(-cell_id_2002,-cell_id_1992) %>%
  base::merge(data_2002_unique,by.x="cell_id_genderless_2002",by.y="cell_id_genderless",suffixes=c("","_2002")) %>% 
  base::merge(data_1992_unique,by.x="cell_id_genderless_1992",by.y="cell_id_genderless",suffixes=c("_2011","_1992"))

setwd(wd_data_linked)
fwrite(data_triple,'data_1992_2002_2011_unique_genderless.csv')

#roma----
setwd(wd_data_linked)
fwrite(data_2002_2011 %>% filter(ROMA_2002==T),'data_2002_2011_roma_unique_genderless.csv')
fwrite(data_1992_2011 %>% filter(ROMA_1992==T),'data_1992_2011_roma_unique_genderless.csv')
fwrite(data_triple %>% filter(ROMA_1992==T),'data_1992_2002_2011_roma_unique_genderless.csv')

