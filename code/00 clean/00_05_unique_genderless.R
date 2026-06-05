# Builds main 1992-2011 uniquely-matched sample (no gender constraint); outputs data_1992_2011_unique_genderless.csv
#data ----
# --- Load 2011 Census Records ---
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# read_sample() loads a representative subset defined by the project's sampling protocol
data_2011<-read_sample(filename)
# Keep only the variables needed for linking and analysis; drop all other 2011 columns to reduce memory
data_2011<-data_2011 %>%
  select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id11,NOP,id11_MOM,id11_POP,id11_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id_2002,cell_id_1992,cell_id_genderless_2002,cell_id_genderless_1992,SEX,SIRUTA,SIRSUP,SCA_MOM,SCA_POP,SCA_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,pop_SIRUTA_2011,pop_SIRSUP_2011,GRUD,STATUT,nat,copil_id,id11_MOM_BC,source,category,scoala_m,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
# read_data() applies any additional cleaning/recoding steps defined for the 2011 file
data_2011<-read_data(filename,data_2011)
data_2011<-data_2011 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  # Tag each row with its census wave; used as a diagnostic after merging
  mutate(census='2011')

# --- Load 2002 Census Records ---
setwd(wd_data_02)
filename<-'data_2002_clean.csv'
data_2002<-read_sample(filename)
# cell_id_genderless: matching key without sex, equal to SIRSUP + AA + ZZ; broader than cell_id and avoids
# spurious non-matches caused by census gender-coding inconsistencies across waves
data_2002<-data_2002 %>%
  select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id02,id02_MOM,id02_POP,id02_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_2002,pop_SIRSUP_2002,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_2002<-read_data(filename,data_2002)
data_2002<-data_2002 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  mutate(census='2002')

# --- Load 1992 Census Records ---
setwd(wd_data_92)
filename<-'data_1992_clean.csv'
data_1992<-read_sample(filename)
data_1992<-data_1992 %>%
  select(ET,EDUC,ROMA,SCA,AA,LL,ZZ,HHID,ET_MOM,ET_POP,ET_SPOUSE,LIM,LIM_MOM,LIM_POP,LIM_SPOUSE,id92,id92_MOM,id92_POP,id92_SPOUSE,REL,REL_MOM,REL_POP,REL_SPOUSE,cell_id,SEX,SIRUTA,SIRSUP,EDUC_MOM,EDUC_POP,EDUC_SPOUSE,years,years_POP,years_MOM,years_SPOUSE,cell_id,cell_id_genderless,pop_SIRUTA_1992,pop_SIRSUP_1992,GRUD,STATUT,source,category,OCUP,OCUP_MOM,OCUP_POP,OCUP_SPOUSE)
data_1992<-read_data(filename,data_1992)
data_1992<-data_1992 %>%
  #filter(AA %in% c(1972,1973,1974,1975)) %>%
  mutate(census='1992')



### match 2002-2011 Roma
# --- Uniqueness Filtering: Drop Non-Unique Genderless Cells per Wave ---
# Keep only records whose genderless cell appears exactly once within the same census wave.
# This ensures that each matching key maps to a single individual, preventing false cross-wave links.

# data_2002_unique: 2002 records with a unique genderless cell (no duplicates on cell_id_genderless)
data_2002_unique<-data_2002 %>%
  filter(!cell_id_genderless %in% cell_id_genderless[duplicated(cell_id_genderless)])
setwd(wd_data_02)
# Output: intermediate unique 2002 records (full set and Roma-only subset) for the 2002-2011 linkage
fwrite(data_2002_unique,'data_2002_unique_genderless.csv')
fwrite(data_2002_unique %>% filter(ROMA==T),'data_2002_roma_unique_genderless.csv')

# data_2011_unique_02: 2011 records whose genderless 2002-side key is not duplicated within the 2011 file
# (the 2011 file carries cell_id_genderless_2002 pre-constructed to match the 2002 nomenclature)
data_2011_unique_02<-data_2011 %>%
  filter(!cell_id_genderless_2002 %in% cell_id_genderless_2002[duplicated(cell_id_genderless_2002)])
setwd(wd_data_11)
fwrite(data_2011_unique_02,'data_2011_unique_02_genderless.csv')

#1992-2011
# data_1992_unique: 1992 records with a unique genderless cell; used as the 1992 anchor for the IV sample
data_1992_unique<-data_1992 %>%
  filter(!cell_id_genderless %in% cell_id_genderless[duplicated(cell_id_genderless)])
setwd(wd_data_92)
fwrite(data_1992_unique,'data_1992_unique_genderless.csv')
fwrite(data_1992_unique %>% filter(ROMA==T),'data_1992_roma_unique_genderless.csv')

# data_2011_unique_92: 2011 records whose genderless 1992-side key is unique within the 2011 file
data_2011_unique_92<-data_2011 %>%
  filter(!cell_id_genderless_1992 %in% cell_id_genderless_1992[duplicated(cell_id_genderless_1992)])
setwd(wd_data_11)
fwrite(data_2011_unique_92,'data_2011_unique_92_genderless.csv')

#triple
# For the triple (1992-2002-2011) sample, a 2011 record must be unique on BOTH the 1992-side
# and the 2002-side genderless keys simultaneously, ensuring unambiguous three-way linkage
data_2011_unique<-data_2011 %>%
  filter(!cell_id_genderless_1992 %in% cell_id_genderless_1992[duplicated(cell_id_genderless_1992)]) %>%
  filter(!cell_id_genderless_2002 %in% cell_id_genderless_2002[duplicated(cell_id_genderless_2002)])



#merge----
##two by two----
# --- Reload Unique Files from Disk (ensures consistency with saved intermediates) ---
setwd(wd_data_02)
data_2002_unique<-fread('data_2002_unique_genderless.csv')
setwd(wd_data_92)
data_1992_unique<-fread('data_1992_unique_genderless.csv')
setwd(wd_data_11)
data_2011_unique_02<-fread('data_2011_unique_02_genderless.csv')
data_2011_unique_92<-fread('data_2011_unique_92_genderless.csv')


# --- Pairwise Cross-Wave Merges ---
# data_2002_2011: merge 2011 and 2002 unique records on the genderless cell key;
# suffixes distinguish same-named columns (e.g., ROMA_2011 vs ROMA_2002, years_2011 vs years_2002)
data_2002_2011<-base::merge(data_2011_unique_02,data_2002_unique,by.x="cell_id_genderless_2002",by.y="cell_id_genderless",suffixes=c("_2011","_2002")) %>% mutate(census=2002)
# data_1992_2011: the main IV analysis dataset — links 1992 education (instrument) to 2011 ethnicity (outcome)
data_1992_2011<-base::merge(data_2011_unique_92,data_1992_unique,by.x="cell_id_genderless_1992",by.y="cell_id_genderless",suffixes=c("_2011","_1992")) %>% mutate(census=1992)

setwd(wd_data_linked)
# Output: pairwise linked datasets used in main and robustness analyses
fwrite(data_2002_2011,'data_2002_2011_unique_genderless.csv')
fwrite(data_1992_2011,'data_1992_2011_unique_genderless.csv')

##triple sample----
# data_triple: individuals matched across all three censuses; used to study persistence of passing and
# to validate the IV by checking education change between 1992 and 2002 relative to 2011 outcome
data_triple<-data_2011_unique %>% select(-cell_id_2002,-cell_id_1992) %>%
  base::merge(data_2002_unique,by.x="cell_id_genderless_2002",by.y="cell_id_genderless",suffixes=c("","_2002")) %>%
  base::merge(data_1992_unique,by.x="cell_id_genderless_1992",by.y="cell_id_genderless",suffixes=c("_2011","_1992"))

setwd(wd_data_linked)
# Output: triple-linked dataset (1992-2002-2011) used in robustness and dynamic analyses
fwrite(data_triple,'data_1992_2002_2011_unique_genderless.csv')

#roma----
# --- Roma-Restricted Subsets of Linked Datasets ---
# Restricting to ROMA==T in the earlier wave isolates individuals who were Roma-identified at baseline,
# enabling estimation of the "passing" probability (switching from Roma to non-Roma identification)
setwd(wd_data_linked)
fwrite(data_2002_2011 %>% filter(ROMA_2002==T),'data_2002_2011_roma_unique_genderless.csv')
# Output: main analytic sample for IV estimation — 1992 Roma individuals re-observed in 2011
fwrite(data_1992_2011 %>% filter(ROMA_1992==T),'data_1992_2011_roma_unique_genderless.csv')
fwrite(data_triple %>% filter(ROMA_1992==T),'data_1992_2002_2011_roma_unique_genderless.csv')

