# Constructs Roma-identified subsample from linked 2011 census-birth data; outputs data_2011_roma.csv

# --- Load 2011 Census (Births File) ---
# Read only the columns needed to identify Roma-connected cells across census years
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# read_sample() fetches the column manifest; read_data() reloads the full rows for those columns
data_2011<-read_sample(filename) %>%
  select(cell_id_genderless_2002,cell_id_genderless_1992,ROMA,ET_MOM,ET_POP,ET_SPOUSE,nat,source,id11_MOM_BC,copil_id)
data_2011<-read_data(filename,data_2011)

# --- Load 2002 Census ---
setwd(wd_data_02)
filename<-'data_2002_clean.csv'
data_2002<-read_sample(filename) %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE,source)
data_2002<-read_data(filename,data_2002)

# --- Load 1992 Census ---
setwd(wd_data_92)
filename<-'data_1992_clean.csv'
data_1992<-read_sample(filename) %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE,source)
data_1992<-read_data(filename,data_1992)

#Get Roma Cells
# Strategy: a cell_id_genderless is flagged as "Roma" if ANY individual in that cell
# self-identifies as Roma OR has a Roma parent/spouse OR was born to a Roma mother
# (nat==12 in the birth register). This broad net captures individuals who may have
# passed in the current census but are connected to the Roma community via relatives.

#2011 - 2002 ----
# Cells derived from 2011 records matched to 2002 locality codes, non-common dwellings only.
# source=="common" (communal dwellings: dorms, barracks) are handled separately below
# because household-level family links are unavailable there.
roma_2011_2002<-data_2011 %>%
  filter(source != "common") %>%
  select(cell_id_genderless_2002,ROMA,ET_MOM,ET_POP,ET_SPOUSE,nat) %>%
  group_by(cell_id_genderless_2002) %>%
  # ET_MOM/ET_POP/ET_SPOUSE use extended ethnicity codes: 1200-1299 covers all Roma sub-codes in 2011
  filter(any(ROMA==T) | ET_MOM %in% 1200:1299 | ET_POP%in% 1200:1299 | ET_SPOUSE %in% 1200:1299 | nat==12)  %>%
  ungroup %>%
  select(cell_id_genderless_2002) %>%
  rename(cell_id_genderless=cell_id_genderless_2002)
gc()

# Common-dwelling cells: only self-identification available (no family links in that source)
roma_2011_2002_common<-data_2011 %>%
  filter(source == "common") %>%
  select(cell_id_genderless_2002,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless_2002)
gc()

#2011 - 1992 ----
# Parallel construction using 1992 matching cell (cell_id_genderless_1992)
roma_2011_1992<-data_2011 %>%
  filter(source != "common") %>%
  select(cell_id_genderless_1992,ROMA,ET_MOM,ET_POP,ET_SPOUSE,nat) %>%
  group_by(cell_id_genderless_1992) %>%
  filter(any(ROMA==T) | ET_MOM %in% 1200:1299 | ET_POP%in% 1200:1299 | ET_SPOUSE %in% 1200:1299 | nat==12)  %>%
  ungroup %>%
  select(cell_id_genderless_1992) %>%
  rename(cell_id_genderless=cell_id_genderless_1992)
gc()

roma_2011_1992_common<-data_2011 %>%
  filter(source == "common") %>%
  select(cell_id_genderless_1992,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless_1992)
gc()

#2002
# In 2002, Roma sub-codes collapse to a single code (12), so no range filter is needed
roma_2002<-data_2002 %>%
  filter(source != "common") %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE) %>%
  group_by(cell_id_genderless) %>%
  filter(any(ROMA==T) | ET_MOM==12 | ET_POP==12 | ET_SPOUSE==12 ) %>%
  ungroup %>%
  select(cell_id_genderless)
gc()

roma_2002_common<-data_2002 %>%
  filter(source == "common") %>%
  select(cell_id_genderless,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless)
gc()

#1992
roma_1992<-data_1992 %>%
  filter(source != "common") %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE) %>%
  group_by(cell_id_genderless) %>%
  filter(any(ROMA==T) | ET_MOM==12 | ET_POP==12 | ET_SPOUSE==12 ) %>%
  ungroup %>%
  select(cell_id_genderless)
gc()

roma_1992_common<-data_1992 %>%
  filter(source == "common") %>%
  select(cell_id_genderless,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless)
gc()

#any
# Union all Roma-flagged cells across all census years and sources.
# A cell appearing in ANY year/source qualifies as Roma-connected.
roma_any<-bind_rows(roma_2011_2002,roma_2011_1992,roma_2002,roma_1992,
                    roma_1992_common,roma_2002_common,roma_2011_1992_common,roma_2011_2002_common)
# Drop duplicates: the same cell may appear in multiple waves; we only need the unique set
roma_any<-roma_any %>% select(cell_id_genderless)

#write
# Output: roma_any.csv -- reference set of Roma-connected genderless cells used for subsetting
setwd(wd_data_linked)
fwrite(roma_any,"roma_any.csv")


# --- Subset Each Census to Roma-Connected Cells ---
# A 2011 record is kept if its 2002 or 1992 matching cell appears in roma_any,
# ensuring we include individuals who may not self-identify as Roma in 2011
# but belong to a Roma-connected cell from an earlier census (i.e., potential passers)
data_2011_roma<-data_2011 %>%
  filter(cell_id_genderless_2002 %in% roma_any$cell_id_genderless|
           cell_id_genderless_1992 %in% roma_any$cell_id_genderless  )

data_2002_roma<-data_2002 %>%
  filter(cell_id_genderless %in% roma_any$cell_id_genderless)

data_1992_roma<-data_1992 %>%
  filter(cell_id_genderless %in% roma_any$cell_id_genderless)


# Output: Roma-connected subsets written to their respective census data directories
setwd(wd_data_11)
fwrite(data_2011_roma,"data_2011_roma.csv")
setwd(wd_data_02)
fwrite(data_2002_roma,"data_2002_roma.csv")
setwd(wd_data_92)
fwrite(data_1992_roma,"data_1992_roma.csv")
