# In-text statistic: share of individuals with no declared ethnicity in 2011 census

# --- Load full 2011 cross-section ---

# Switch to the directory holding the 2011 standalone census file
setwd(wd_data_11)
filename<-'data_2011_clean.csv'
# read_sample() pulls a column-selected preview used to determine types; read_data() loads the full file
# ET: raw ethnicity code (9999 flags records where ethnicity was not reported)
data_11<-read_sample(filename) %>%
  select(ROMA,years,ET,AA,pop_SIRSUP_2011,SEX)
data_11<-read_data(filename,data_11)
# Tag with census wave so datasets can be distinguished if combined later
data_11<-data_11 %>%
  mutate(Census="2011")

# --- Load 1992-2011 linked panel ---

setwd(wd_data_linked)
filename<-'data_1992_2011_unique.csv'
# ET_2011 retains the raw 2011 ethnicity code for the missing-ethnicity check below
data_92_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ET_2011)
data_92_11<-read_data(filename,data_92_11)
# Standardise column names to a common schema (ROMA, years, AA) shared across waves
data_92_11<-data_92_11 %>%
  mutate(Census="1992 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011)

# --- Load 2002-2011 linked panel ---

setwd(wd_data_linked)
filename<-'data_2002_2011_unique.csv'
data_02_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ET_2011)
data_02_11<-read_data(filename,data_02_11)
data_02_11<-data_02_11 %>%
  mutate(Census="2002 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011)

# Finalise sex column name in linked panels to match the common schema
data_02_11<-data_02_11 %>%
  rename(SEX=SEX_2011)
data_92_11<-data_92_11 %>%
  rename(SEX=SEX_2011)

# --- Compute shares with no reported ethnicity (ET == 9999) ---

# ET == 9999 is the sentinel used in the cleaned files to flag a missing/non-response ethnicity entry.
# These shares are cited in the paper to document the extent of ethnicity non-response in 2011
# and to motivate why the linked-panel samples (which condition on a 1992/2002 observation) show
# lower non-response rates than the full 2011 cross-section.
#missing 2011
sum(data_11$ET==9999)/nrow(data_11) #5.9\%
sum(data_92_11$ET_2011==9999)/nrow(data_92_11) #2.3\%
sum(data_02_11$ET_2011==9999)/nrow(data_02_11) #2.6\%
