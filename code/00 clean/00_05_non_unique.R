# Builds 1992-2011 non-uniquely-matched sample (for robustness checks)

# --- Data Loading ---

# Load 2011 census clean file (births-restricted version)
#data ----
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# read_sample() loads the column schema; read_data() fills the actual rows
data_2011<-read_sample(filename)
# Retain only the variables needed for the matching and analysis steps below;
# includes cross-census cell identifiers (cell_id_2002, cell_id_1992, cell_id_genderless_*)
# used to link 2011 records back to their 2002 and 1992 counterparts
data_2011<-data_2011 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id11,NOP,id11_MOM,id11_POP,id11_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id_2002,cell_id_1992,cell_id_genderless_2002,cell_id_genderless_1992,SEX,SIRUTA,SIRSUP,MAPA,SCA_MOM,SCA_POP,SCA_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,pop_SIRUTA_2011,pop_SIRSUP_2011,GRUD,STATUT,nat,copil_id,id11_MOM_BC,source,category,scoala_m,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_2011<-read_data(filename,data_2011)
data_2011<-data_2011 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  mutate(census='2011')

# Load 2002 census clean file
setwd(wd_data_02)
filename<-'data_2002_clean.csv'
data_2002<-read_sample(filename)
# cell_id and cell_id_genderless here are constructed for matching TO a future census
data_2002<-data_2002 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id02,id02_MOM,id02_POP,id02_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,MAPA,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_2002,pop_SIRSUP_2002,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_2002<-read_data(filename,data_2002)
data_2002<-data_2002 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  mutate(census='2002')

# Load 1992 census clean file
setwd(wd_data_92)
filename<-'data_1992_clean.csv'
data_1992<-read_sample(filename)
# cell_id here uses the 1992-census construction; matched against cell_id_1992 in 2011
data_1992<-data_1992 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id92,id92_MOM,id92_POP,id92_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,MAPA,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_1992,pop_SIRSUP_1992,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_1992<-read_data(filename,data_1992)
data_1992<-data_1992 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  mutate(census='1992')



# --- Non-Unique Cell Collapse (2002-2011 Pair) ---

### match 2002-2011 Roma

# Collapse 2002 records to cell-level averages, keeping ALL cells (n >= 1).
# Unlike the unique-match sample (which keeps only n == 1 cells), this
# non-unique version retains cells where multiple individuals share the same
# matching key, averaging their outcomes. This allows robustness checks on
# whether results depend on restricting to uniquely-matched individuals.
# n: [cell count] number of individuals sharing this cell_id in 2002
data_2002_non_unique <- data_2002[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id, SIRSUP, pop_SIRSUP_2002,AA,LL,ZZ)]
# setwd(wd_data_02)
# fwrite(data_2002_unique,'data_2002_unique.csv')
# fwrite(data_2002_unique %>% filter(ROMA==T),'data_2002_roma_unique.csv')

# Collapse 2011 records using the 2002-vintage cell identifier stored in cell_id_2002
data_2011_non_unique_02<-data_2011[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id_2002, SIRSUP, pop_SIRSUP_2011,AA,LL,ZZ)]
# setwd(wd_data_11)
# fwrite(data_2011_unique_02,'data_2011_unique_02.csv')

# --- Non-Unique Cell Collapse (1992-2011 Pair) ---

#1992-2011
# Collapse 1992 records to cell-level averages across all individuals in each cell
data_1992_non_unique <- data_1992[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id, SIRSUP, pop_SIRSUP_1992,AA,LL,ZZ)]
# setwd(wd_data_92)
# fwrite(data_1992_unique,'data_1992_unique.csv')
# fwrite(data_1992_unique %>% filter(ROMA==T),'data_1992_roma_unique.csv')

# Collapse 2011 records using the 1992-vintage cell identifier stored in cell_id_1992;
# this is the key variable that bridges the two censuses for the IV analysis
data_2011_non_unique_92<-data_2011[, .(
  years = mean(years, na.rm = TRUE),
  ROMA = mean(ROMA, na.rm = TRUE),
  n=.N
), by = .(cell_id_1992, SIRSUP, pop_SIRSUP_2011,AA,LL,ZZ)]
# setwd(wd_data_11)
# fwrite(data_2011_unique_92,'data_2011_unique_92.csv')


# --- Cross-Census Merge ---

# Merge collapsed 2011 records onto collapsed 2002 records by matching cell identifier;
# suffixes distinguish same-named variables (e.g., years_2011 vs years_2002, ROMA_2011 vs ROMA_2002)
data_2002_2011<-merge.data.table(data_2011_non_unique_02,
                            data_2002_non_unique,
                            by.x="cell_id_2002",
                            by.y="cell_id",
                            suffixes=c("_2011","_2002")) %>%
  mutate(census=2002)
# Merge for the longer 1992-2011 panel; years_1992 serves as the IV instrument,
# years_2011 is the endogenous regressor, and ROMA_1992/ROMA_2011 track ethnic re-identification
data_1992_2011<-base::merge(data_2011_non_unique_92,
                            data_1992_non_unique,
                            by.x="cell_id_1992",
                            by.y="cell_id",
                            suffixes=c("_2011","_1992")) %>%
  mutate(census=1992)
# data_triple<-data_2011_unique %>% select(-cell_id_genderless_1992,-cell_id_genderless_2002) %>%
#   base::merge(data_2002_unique,by.x="cell_id_2002",by.y="cell_id",suffixes=c("","_2002")) %>%
#   base::merge(data_1992_unique,by.x="cell_id_1992",by.y="cell_id",suffixes=c("_2011","_1992"))

# --- Save Linked Non-Unique Files ---

# Output: full non-unique linked panels (all ethnic groups) for robustness checks
setwd(wd_data_linked)
fwrite(data_2002_2011,'data_2002_2011_non_unique.csv')
fwrite(data_1992_2011,'data_1992_2011_non_unique.csv')
# fwrite(data_triple,'data_1992_2002_2011_unique.csv')

# Output: Roma-only subsets (ROMA > 0 allows for fractional averages in non-unique cells)
#roma
setwd(wd_data_linked)
fwrite(data_2002_2011 %>% filter(ROMA_2002>0),'data_2002_2011_roma_non_unique.csv')
fwrite(data_1992_2011 %>% filter(ROMA_1992>0),'data_1992_2011_roma_non_unique.csv')
# fwrite(data_triple %>% filter(ROMA_1992>0),'data_1992_2002_2011_roma_non_unique.csv')
