# In-text statistics: match rates and sample sizes across census years

# --- Setup: Load Linked and Raw Census Data ---

# Switch to the folder holding linked (matched) census files
setwd(wd_data_linked)
# data_2002_2011: individuals matched across the 2002 and 2011 censuses
data_2002_2011<-fread('data_2002_2011_unique.csv')
# data_1992_2011: individuals matched across the 1992 and 2011 censuses
data_1992_2011<-fread('data_1992_2011_unique.csv')
# data_1992_2002_2011: individuals matched across all three censuses (main analysis sample)
data_1992_2002_2011<-fread('data_1992_2002_2011_unique.csv')
# Switch to the folder holding the cleaned 2011 census (universe denominator for coverage rates)
setwd(wd_data_11)
# data_2011: full stable-population sample from the 2011 census
data_2011<-fread('data_2011_clean.csv')

# --- Slim DataFrames to Only Needed Columns ---

# Keep only ROMA flag, urbanisation code, birth year, and locality population for the 2011 base
data_2011<-data_2011 %>%
  select(ROMA,MEDIU,AA,pop_SIRSUP_2011)
# For matched samples, keep only the 2011 birth year and locality population (used as denominators)
data_2002_2011<-data_2002_2011 %>%
  select(AA_2011,pop_SIRSUP_2011)
data_1992_2011<-data_1992_2011 %>%
  select(AA_2011,pop_SIRSUP_2011)
data_1992_2002_2011<-data_1992_2002_2011 %>%
  select(AA_2011,pop_SIRSUP_2011)

# --- Define Age-Eligible Sub-Populations for Coverage Denominators ---

# Individuals born by 2001 could plausibly appear in a 2002 census match (age >= 1 in 2002)
data_2011_matchable_02<-data_2011 %>% filter(AA<=2001)
# Individuals born by 1991 could plausibly appear in a 1992 census match (age >= 1 in 1992)
data_2011_matchable_92<-data_2011 %>% filter(AA<=1991)

# --- Urban/Rural Share of Roma in 2011 ---

# MEDIU: urbanisation code (urban vs. rural); used to describe Roma residential composition in text
summary_roma_11_urb<-data_2011 %>%
  group_by(ROMA,MEDIU) %>%
  summarise(n=n()) %>%
  group_by(ROMA) %>%
  mutate(p=n/sum(n))

# Share of Roma (and non-Roma) born on or before 1990, i.e., old enough to have appeared in 1992 census
summary_roma_11_young<-data_2011 %>%
  group_by(ROMA,AA<=1990) %>%
  summarise(n=n()) %>%
  group_by(ROMA) %>%
  mutate(p=n/sum(n))



#20,121,641 people in 2011 census; stable pop is 19,042,936

# --- Coverage Rates: Matched Samples Relative to Full 2011 Stable Population ---

#coverage----
# Each ratio is the share of the 2011 stable population (19.04 M) that appears in a given linked file
nrow(data_1992_2011)/19042936
nrow(data_2002_2011)/19042936
nrow(data_1992_2002_2011)/19042936

# --- Coverage Rates: Matched Samples Relative to All 2011 Census Respondents ---

#coverage----
# Denominator is now the total rows in data_2011 (the cleaned file, not the fixed 19 M figure)
nrow(data_1992_2011)/nrow(data_2011)
nrow(data_2002_2011)/nrow(data_2011)
nrow(data_1992_2002_2011)/nrow(data_2011)

# --- Coverage Rates: Matched Samples Relative to Age-Eligible 2011 Population ---

#coverage----
# Most informative denominator: only people old enough to have been alive at the earlier census
nrow(data_1992_2011)/nrow(data_2011_matchable_92)
nrow(data_2002_2011)/nrow(data_2011_matchable_02)
nrow(data_1992_2002_2011)/nrow(data_2011_matchable_92)

# --- Coverage Rates: Restricting Both Numerator and Denominator to Age-Eligible ---

#
# Further restricts the matched sample to records whose 2011 birth year is also age-eligible,
# making numerator and denominator fully comparable on the birth-year criterion
nrow(data_1992_2011 %>% filter(AA_2011<=1991))/nrow(data_2011 %>% filter(AA<=1991))
nrow(data_2002_2011 %>% filter(AA_2011<=2001))/nrow(data_2011 %>% filter(AA<=2001))
nrow(data_1992_2002_2011 %>% filter(AA_2011<=1991))/nrow(data_2011_matchable_92 %>% filter(AA<=1991))

# --- Coverage Rates in Small Localities (Pop <= 10,000) ---

#restrict to pop in small towns
# The main analysis focuses on small localities where Roma communities are more self-contained
# and cell-based matching is more reliable; this checks how coverage rates differ there
nrow(data_1992_2011 %>% filter(pop_SIRSUP_2011<=10000 ))/nrow(data_2011_matchable_92 %>% filter(pop_SIRSUP_2011<=10000))
nrow(data_2002_2011 %>% filter(pop_SIRSUP_2011<=10000))/nrow(data_2011_matchable_02 %>% filter(pop_SIRSUP_2011<=10000))
nrow(data_1992_2002_2011 %>% filter(pop_SIRSUP_2011<=10000))/nrow(data_2011_matchable_92 %>% filter(pop_SIRSUP_2011<=10000))

# --- Geographic Concentration of Roma in Small Localities ---

#self-declared Roma in small localities:
# Share of all Roma living in localities with population <= 10,000 (justifies focusing on small towns)
nrow(data_2011 %>% filter(pop_SIRSUP_2011<=10000 & ROMA==T ))/nrow(data_2011 %>% filter(ROMA==T ))
# Share of the overall population living in localities with population <= 10,000 (comparison benchmark)
nrow(data_2011 %>% filter(pop_SIRSUP_2011<=10000 ))/nrow(data_2011)


# --- Absolute Sample Sizes Reported in Text ---

#how many matched individuals in each sample?
nrow(data_2002_2011) #7,516,703
nrow(data_1992_2011) #5,479,009
nrow(data_1992_2002_2011) #4,454,321

# --- Load Genderless (cell_id_genderless) Matched Samples ---

# cell_id_genderless drops sex from the matching cell, producing broader (less restrictive) matches;
# used in the main analysis to avoid artefacts from gender-ratio imbalances in Roma communities
setwd(wd_data_linked)
data_2002_2011_sexless<-fread('data_2002_2011_unique_genderless.csv')
data_1992_2011_sexless<-fread('data_1992_2011_unique_genderless.csv')
data_1992_2002_2011_sexless<-fread('data_1992_2002_2011_unique_genderless.csv')
# Report sizes; smaller than gendered matches because genderless cells can produce more non-unique matches
# that are dropped by the _unique filter
nrow(data_2002_2011_sexless) #6,286,727
nrow(data_1992_2011_sexless) #4,545,737
nrow(data_1992_2002_2011_sexless) #3,630,390
