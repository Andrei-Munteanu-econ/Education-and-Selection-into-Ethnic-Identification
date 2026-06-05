# Builds 1992-2011 uniquely-matched sample with gender constraint; outputs data_1992_2011_unique.csv
#data ----
# --- Load 2011 Census Records ---
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# Read just the column headers / metadata via the project's read_sample() helper
data_2011<-read_sample(filename)
# Keep only the variables needed for linking and analysis; drops heavy auxiliary columns
data_2011<-data_2011 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id11,NOP,id11_MOM,id11_POP,id11_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id_2002,cell_id_genderless_2002,cell_id_genderless_1992,cell_id_1992,SEX,SIRUTA,SIRSUP,MAPA,SCA_MOM,SCA_POP,SCA_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,pop_SIRUTA_2011,pop_SIRSUP_2011,GRUD,STATUT,nat,copil_id,id11_MOM_BC,source,category,scoala_m,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
# Read the actual data rows using the project's read_data() helper
data_2011<-read_data(filename,data_2011)
data_2011<-data_2011 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  # Tag records so that census wave can be identified after stacking / merging
  mutate(census='2011')

# --- Load 2002 Census Records ---
setwd(wd_data_02)
filename<-'data_2002_clean.csv'
data_2002<-read_sample(filename)
# 2002 uses cell_id (not census-specific variant) as the within-census unique identifier
data_2002<-data_2002 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id02,id02_MOM,id02_POP,id02_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,MAPA,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_2002,pop_SIRSUP_2002,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_2002<-read_data(filename,data_2002)
data_2002<-data_2002 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  mutate(census='2002')

# --- Load 1992 Census Records ---
setwd(wd_data_92)
filename<-'data_1992_clean.csv'
data_1992<-read_sample(filename)
# 1992 structure mirrors 2002: cell_id is the within-census matching key
data_1992<-data_1992 %>%
  dplyr::select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id92,id92_MOM,id92_POP,id92_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,MAPA,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_1992,pop_SIRSUP_1992,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_1992<-read_data(filename,data_1992)
data_1992<-data_1992 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  mutate(census='1992')



# --- Uniqueness Filtering: 2002-2011 Match ---
# [data_2002_unique]: 2002 records whose cell_id appears exactly once — safe to treat as a unique person
# Dropping duplicates on cell_id prevents many-to-one (and one-to-many) false matches across censuses
### match 2002-2011 Roma
data_2002_unique<-data_2002 %>%
  filter(!cell_id %in% cell_id[duplicated(cell_id)])
setwd(wd_data_02)
# Output: deduplicated 2002 full sample (used as the right-hand side of the 2002-2011 merge)
fwrite(data_2002_unique,'data_2002_unique.csv')
# Output: Roma-only subset of the deduplicated 2002 sample (used in Roma-specific analyses)
fwrite(data_2002_unique %>% filter(ROMA==T),'data_2002_roma_unique.csv')

# [data_2011_unique_02]: 2011 records whose cell_id_2002 (backward-looking key to 2002) is non-duplicate
# Ensures each 2002 cell maps to at most one 2011 record, preventing many-to-one matches from the 2011 side
data_2011_unique_02<-data_2011 %>%
  filter(!cell_id_2002 %in% cell_id_2002[duplicated(cell_id_2002)])
setwd(wd_data_11)
# Output: 2011 records with unique links back to 2002 (left-hand side of 2002-2011 merge)
fwrite(data_2011_unique_02,'data_2011_unique_02.csv')

# --- Uniqueness Filtering: 1992-2011 Match ---
#1992-2011
# [data_1992_unique]: 1992 records with a unique cell_id — baseline year for the IV strategy
data_1992_unique<-data_1992 %>%
  filter(!cell_id %in% cell_id[duplicated(cell_id)])
setwd(wd_data_92)
# Output: deduplicated 1992 full sample
fwrite(data_1992_unique,'data_1992_unique.csv')
# Output: Roma-only subset of deduplicated 1992 sample; the base population for estimating ethnic passing
fwrite(data_1992_unique %>% filter(ROMA==T),'data_1992_roma_unique.csv')

# [data_2011_unique_92]: 2011 records with a unique cell_id_1992 (backward-looking key to 1992)
data_2011_unique_92<-data_2011 %>%
  filter(!cell_id_1992 %in% cell_id_1992[duplicated(cell_id_1992)])
setwd(wd_data_11)
# Output: 2011 records with unique links back to 1992 (left-hand side of 1992-2011 merge)
fwrite(data_2011_unique_92,'data_2011_unique_92.csv')

# --- Uniqueness Filtering: Triple-Census Match (1992-2002-2011) ---
# Intersection of the two uniqueness conditions: only 2011 records that link uniquely to BOTH
# the 1992 and the 2002 censuses are kept; this is the strictest sample used in robustness checks
#triple
data_2011_unique<-data_2011 %>%
  filter(!cell_id_1992 %in% cell_id_1992[duplicated(cell_id_1992)])  %>%
  filter(!cell_id_2002 %in% cell_id_2002[duplicated(cell_id_2002)])

# --- Merge Unique Panels ---
# (Commented-out reload block allows re-entry at this step without re-running the filtering above)
#merge unique
# setwd(wd_data_02)
# data_2002_unique<-fread('data_2002_unique.csv')
# setwd(wd_data_92)
# data_1992_unique<-fread('data_1992_unique.csv')
# setwd(wd_data_11)
# data_2011_unique_02<-fread('data_2011_unique_02.csv')
# data_2011_unique_92<-fread('data_2011_unique_92.csv')


# [data_2002_2011]: Linked panel where each row is one individual observed in both 2002 and 2011
# suffix "_2011" / "_2002" disambiguates same-name variables from each wave
data_2002_2011<-base::merge(data_2011_unique_02,data_2002_unique,by.x="cell_id_2002",by.y="cell_id",suffixes=c("_2011","_2002")) %>% mutate(census=2002)
# [data_1992_2011]: Linked panel for the main IV analysis (1992 education instruments 2011 identification)
data_1992_2011<-base::merge(data_2011_unique_92,data_1992_unique,by.x="cell_id_1992",by.y="cell_id",suffixes=c("_2011","_1992")) %>% mutate(census=1992)
# [data_triple]: Three-wave balanced panel; genderless cell IDs dropped before merging to avoid
# column conflicts, since they are not needed once the gender-strict cell_id is used for matching
data_triple<-data_2011_unique %>% select(-cell_id_genderless_1992,-cell_id_genderless_2002) %>%
  base::merge(data_2002_unique,by.x="cell_id_2002",by.y="cell_id",suffixes=c("","_2002")) %>%
  base::merge(data_1992_unique,by.x="cell_id_1992",by.y="cell_id",suffixes=c("_2011","_1992"))

# --- Save Linked Panels ---
setwd(wd_data_linked)
# Output: full 2002-2011 unique matched panel
fwrite(data_2002_2011,'data_2002_2011_unique.csv')
# Output: full 1992-2011 unique matched panel (main analysis dataset)
fwrite(data_1992_2011,'data_1992_2011_unique.csv')
# Output: triple-census matched panel (robustness / attrition checks)
fwrite(data_triple,'data_1992_2002_2011_unique.csv')
# --- Save Roma-Restricted Subsets ---
# Conditioning on ROMA_1992 == TRUE isolates the population at risk of ethnic passing
#roma
setwd(wd_data_linked)
# Output: 2002-2011 panel restricted to individuals identified as Roma in 2002
fwrite(data_2002_2011 %>% filter(ROMA_2002==T),'data_2002_2011_roma_unique.csv')
# Output: 1992-2011 panel restricted to individuals identified as Roma in 1992 (primary estimation sample)
fwrite(data_1992_2011 %>% filter(ROMA_1992==T),'data_1992_2011_roma_unique.csv')
# Output: triple-census panel restricted to Roma in 1992
fwrite(data_triple %>% filter(ROMA_1992==T),'data_1992_2002_2011_roma_unique.csv')
