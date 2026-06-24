# =====================================================================
# In-text statistic — share with no reported ethnicity in 2011
# Produces:  console output only (no files written)
# Inputs:    data_2011_clean.csv (2011 census), data_1992_2011_unique.csv
#            and data_2002_2011_unique.csv (linked samples)
# Summary:   Computes the fraction of records whose 2011 ethnicity code
#            (ET) is the missing/not-stated value 9999, in the full 2011
#            census and in each linked sample, for the share quoted in
#            the text.
# =====================================================================

# ---- 2011 census: load and keep the variables of interest ----
setwd(wd_data_11)
filename<-'data_2011_clean.csv'
data_11<-read_sample(filename) %>%
  select(ROMA,years,ET,AA,pop_SIRSUP_2011,SEX)
data_11<-read_data(filename,data_11)
data_11<-data_11 %>%
  mutate(Census="2011")

# ---- 1992->2011 linked sample: load and keep the 2011-side variables ----
setwd(wd_data_linked)
filename<-'data_1992_2011_unique.csv'
data_92_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ET_2011)
data_92_11<-read_data(filename,data_92_11)
data_92_11<-data_92_11 %>%
  mutate(Census="1992 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011)

# ---- 2002->2011 linked sample: load and keep the 2011-side variables ----
setwd(wd_data_linked)
filename<-'data_2002_2011_unique.csv'
data_02_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ET_2011)
data_02_11<-read_data(filename,data_02_11)
data_02_11<-data_02_11 %>%
  mutate(Census="2002 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011)

data_02_11<-data_02_11 %>%
  rename(SEX=SEX_2011)
data_92_11<-data_92_11 %>%
  rename(SEX=SEX_2011)

# ---- Share with missing/not-stated 2011 ethnicity (ET code 9999) in each sample ----
sum(data_11$ET==9999)/nrow(data_11) #5.9\%
sum(data_92_11$ET_2011==9999)/nrow(data_92_11) #2.3\%
sum(data_02_11$ET_2011==9999)/nrow(data_02_11) #2.6\%
