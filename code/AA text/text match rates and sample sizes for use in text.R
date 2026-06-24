# =====================================================================
# In-text statistics — match rates and linked-sample sizes
# Produces:  console output only (no files written)
# Inputs:    data_2002_2011_unique.csv, data_1992_2011_unique.csv,
#            data_1992_2002_2011_unique.csv (linked census samples),
#            data_2011_clean.csv, plus the genderless variants
# Summary:   Computes the coverage / match rates quoted in the text:
#            linked individuals as a share of the stable 2011 population,
#            of all 2011 census records, and of the subset old enough to
#            be matchable to the baseline census; also restricts to small
#            towns and to self-declared Roma, and prints the raw matched
#            sample sizes for each pairwise/triple linkage.
# =====================================================================

# ---- Load the three linked samples and the 2011 cleaned census ----
setwd(wd_data_linked)
data_2002_2011<-fread('data_2002_2011_unique.csv')
data_1992_2011<-fread('data_1992_2011_unique.csv')
data_1992_2002_2011<-fread('data_1992_2002_2011_unique.csv')
setwd(wd_data_11)
data_2011<-fread('data_2011_clean.csv')

# ---- Keep only the columns needed from each dataset ----
data_2011<-data_2011 %>%
  select(ROMA,MEDIU,AA,pop_SIRSUP_2011)
data_2002_2011<-data_2002_2011 %>%
  select(AA_2011,pop_SIRSUP_2011)
data_1992_2011<-data_1992_2011 %>%
  select(AA_2011,pop_SIRSUP_2011)
data_1992_2002_2011<-data_1992_2002_2011 %>%
  select(AA_2011,pop_SIRSUP_2011)

# ---- Define the matchable 2011 subsets: born by 2001 (for 2002 link) and by 1991 (for 1992 link) ----
data_2011_matchable_02<-data_2011 %>% filter(AA<=2001)
data_2011_matchable_92<-data_2011 %>% filter(AA<=1991)

# ---- Roma urban/rural split in 2011 (share urban vs. rural by reported-Roma status) ----
summary_roma_11_urb<-data_2011 %>%
  group_by(ROMA,MEDIU) %>%
  summarise(n=n()) %>%
  group_by(ROMA) %>%
  mutate(p=n/sum(n))

# ---- Roma age split in 2011 (share born on/before 1990 by reported-Roma status) ----
summary_roma_11_young<-data_2011 %>%
  group_by(ROMA,AA<=1990) %>%
  summarise(n=n()) %>%
  group_by(ROMA) %>%
  mutate(p=n/sum(n))
  


# 20,121,641 people in 2011 census; stable (de jure) population is 19,042,936

# ---- Coverage: linked individuals as a share of the stable 2011 population ----
nrow(data_1992_2011)/19042936
nrow(data_2002_2011)/19042936
nrow(data_1992_2002_2011)/19042936

# ---- Coverage: linked individuals as a share of all 2011 census records ----
nrow(data_1992_2011)/nrow(data_2011)
nrow(data_2002_2011)/nrow(data_2011)
nrow(data_1992_2002_2011)/nrow(data_2011)

# ---- Coverage: linked individuals as a share of the matchable 2011 subset (old enough to appear in baseline census) ----
nrow(data_1992_2011)/nrow(data_2011_matchable_92)
nrow(data_2002_2011)/nrow(data_2011_matchable_02)
nrow(data_1992_2002_2011)/nrow(data_2011_matchable_92)

# ---- Coverage among the cohort born by the baseline-census year (1991 or 2001) ----
nrow(data_1992_2011 %>% filter(AA_2011<=1991))/nrow(data_2011 %>% filter(AA<=1991))
nrow(data_2002_2011 %>% filter(AA_2011<=2001))/nrow(data_2011 %>% filter(AA<=2001))
nrow(data_1992_2002_2011 %>% filter(AA_2011<=1991))/nrow(data_2011_matchable_92 %>% filter(AA<=1991))

# ---- Coverage restricted to small localities (2011 population <= 10,000) ----
nrow(data_1992_2011 %>% filter(pop_SIRSUP_2011<=10000 ))/nrow(data_2011_matchable_92 %>% filter(pop_SIRSUP_2011<=10000))
nrow(data_2002_2011 %>% filter(pop_SIRSUP_2011<=10000))/nrow(data_2011_matchable_02 %>% filter(pop_SIRSUP_2011<=10000))
nrow(data_1992_2002_2011 %>% filter(pop_SIRSUP_2011<=10000))/nrow(data_2011_matchable_92 %>% filter(pop_SIRSUP_2011<=10000))

# ---- Share of self-declared Roma (and of everyone) living in small localities in 2011 ----
nrow(data_2011 %>% filter(pop_SIRSUP_2011<=10000 & ROMA==T ))/nrow(data_2011 %>% filter(ROMA==T ))
nrow(data_2011 %>% filter(pop_SIRSUP_2011<=10000 ))/nrow(data_2011)


# ---- Matched sample sizes (with-gender linkages) ----
# How many matched individuals are in each linked sample?
nrow(data_2002_2011) #7,516,703
nrow(data_1992_2011) #5,479,009
nrow(data_1992_2002_2011) #4,454,321
# ---- Matched sample sizes (genderless linkages, which ignore sex when matching) ----
setwd(wd_data_linked)
data_2002_2011_sexless<-fread('data_2002_2011_unique_genderless.csv')
data_1992_2011_sexless<-fread('data_1992_2011_unique_genderless.csv')
data_1992_2002_2011_sexless<-fread('data_1992_2002_2011_unique_genderless.csv')
nrow(data_2002_2011_sexless) #6,286,727
nrow(data_1992_2011_sexless) #4,545,737
nrow(data_1992_2002_2011_sexless) #3,630,390
