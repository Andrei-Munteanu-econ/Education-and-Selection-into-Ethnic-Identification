# In-text statistic: share of male household heads in 2011 census

# --- 2011 Census: Share of Household Heads by Sex ---

# Load the cleaned 2011 census individual-level file
setwd(wd_data_11)
data_2011<-fread("data_2011_clean.csv")
# GRUD == 1 flags the designated household head (cap de gospodărie) in Romanian census coding;
# restricting to these records isolates one representative observation per household
stats_2011<-data_2011 %>%
  filter(GRUD==1) %>%
# SEX: 1 = male, 2 = female (consistent with LL coding elsewhere in the project)
  group_by(SEX) %>%
  summarise(n=n()) %>%
  ungroup() %>%
# Convert raw counts to shares within the household-head population
  mutate(n=n/sum(n))

# --- 2002 Census: Share of Household Heads by Sex ---

setwd(wd_data_02)
data_2002<-fread("data_2002_clean.csv")
# Same GRUD == 1 filter applied to 2002 census for comparability across waves
stats_2002<-data_2002 %>%
  filter(GRUD==1) %>%
  group_by(SEX) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(n=n/sum(n))

# --- 1992 Census: Share of Household Heads by Sex ---

setwd(wd_data_92)
data_1992<-fread("data_1992_clean.csv")
# 1992 baseline wave; reported alongside 2002 and 2011 figures to document
# the stability of the male-headed household share across the study period
stats_1992<-data_1992 %>%
  filter(GRUD==1) %>%
  group_by(SEX) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(n=n/sum(n))


