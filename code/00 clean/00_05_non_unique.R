# =====================================================================
# Build the linked panel from NON-UNIQUE cells by averaging within each cell
# Produces:  data_2002_2011_non_unique.csv, data_1992_2011_non_unique.csv
#            (and Roma-only versions); intermediate, written to wd_data_linked
# Inputs:    data_2011_clean_births.csv, data_2002_clean.csv, data_1992_clean.csv
# Summary:   Variant of 00_05_unique that does NOT require unique cells. Instead
#            it collapses each (locality x birthdate x sex) cell to its mean
#            schooling (years), mean Roma share (ROMA), and a count (n) per wave,
#            then merges 2011 to 2002 and 2011 to 1992 on the cell key. This
#            retains many-to-one cells via cell-level averages rather than
#            dropping them. No paper exhibit.
# =====================================================================

#data ----
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
data_2011<-read_sample(filename)
data_2011<-data_2011 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id11,NOP,id11_MOM,id11_POP,id11_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id_2002,cell_id_1992,cell_id_genderless_2002,cell_id_genderless_1992,SEX,SIRUTA,SIRSUP,MAPA,SCA_MOM,SCA_POP,SCA_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,pop_SIRUTA_2011,pop_SIRSUP_2011,GRUD,STATUT,nat,copil_id,id11_MOM_BC,source,category,scoala_m,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_2011<-read_data(filename,data_2011)
data_2011<-data_2011 %>%  mutate(census='2011') 

setwd(wd_data_02)
filename<-'data_2002_clean.csv'
data_2002<-read_sample(filename)
data_2002<-data_2002 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id02,id02_MOM,id02_POP,id02_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,MAPA,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_2002,pop_SIRSUP_2002,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE) 
data_2002<-read_data(filename,data_2002)
data_2002<-data_2002 %>%  mutate(census='2002') 

setwd(wd_data_92)
filename<-'data_1992_clean.csv'
data_1992<-read_sample(filename)
data_1992<-data_1992 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id92,id92_MOM,id92_POP,id92_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,MAPA,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_1992,pop_SIRSUP_1992,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE) 
data_1992<-read_data(filename,data_1992)
data_1992<-data_1992 %>%  mutate(census='1992') 



# ---- Collapse each cell to mean schooling, Roma share, and count (per wave) ----
### match 2002-2011 Roma
data_2002_non_unique <- data_2002[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id, SIRSUP, pop_SIRSUP_2002,AA,LL,ZZ)]

data_2011_non_unique_02<-data_2011[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id_2002, SIRSUP, pop_SIRSUP_2011,AA,LL,ZZ)]

#1992-2011
data_1992_non_unique <- data_1992[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id, SIRSUP, pop_SIRSUP_1992,AA,LL,ZZ)]

data_2011_non_unique_92<-data_2011[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id_1992, SIRSUP, pop_SIRSUP_2011,AA,LL,ZZ)]


# ---- Merge cell-level aggregates across waves ----
data_2002_2011<-merge.data.table(data_2011_non_unique_02,
                            data_2002_non_unique,
                            by.x="cell_id_2002",
                            by.y="cell_id",
                            suffixes=c("_2011","_2002")) %>% 
  mutate(census=2002)
data_1992_2011<-base::merge(data_2011_non_unique_92,
                            data_1992_non_unique,
                            by.x="cell_id_1992",
                            by.y="cell_id",
                            suffixes=c("_2011","_1992")) %>% 
  mutate(census=1992)
# data_triple<-data_2011_unique %>% select(-cell_id_genderless_1992,-cell_id_genderless_2002) %>%
#   base::merge(data_2002_unique,by.x="cell_id_2002",by.y="cell_id",suffixes=c("","_2002")) %>% 
#   base::merge(data_1992_unique,by.x="cell_id_1992",by.y="cell_id",suffixes=c("_2011","_1992"))

# ---- Write linked panels (full, then Roma-only where Roma share > 0) ----
setwd(wd_data_linked)
fwrite(data_2002_2011,'data_2002_2011_non_unique.csv')
fwrite(data_1992_2011,'data_1992_2011_non_unique.csv')
# fwrite(data_triple,'data_1992_2002_2011_unique.csv')
#roma
setwd(wd_data_linked)
fwrite(data_2002_2011 %>% filter(ROMA_2002>0),'data_2002_2011_roma_non_unique.csv')
fwrite(data_1992_2011 %>% filter(ROMA_1992>0),'data_1992_2011_roma_non_unique.csv')
# fwrite(data_triple %>% filter(ROMA_1992>0),'data_1992_2002_2011_roma_non_unique.csv')