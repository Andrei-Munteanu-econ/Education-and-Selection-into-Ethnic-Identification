# =====================================================================
# In-text statistic — share of male household heads by census year
# Produces:  console output only (no files written)
# Inputs:    data_2011_clean.csv, data_2002_clean.csv, data_1992_clean.csv
# Summary:   For each census, restricts to household heads (GRUD==1) and
#            computes the sex composition (share male vs. female) among
#            them. Result objects stats_2011 / stats_2002 / stats_1992
#            hold the share by SEX, used for the figure quoted in the text.
# =====================================================================

# ---- 2011: sex composition among household heads ----
setwd(wd_data_11)
data_2011<-fread("data_2011_clean.csv")
stats_2011<-data_2011 %>%
  filter(GRUD==1) %>%
  group_by(SEX) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(n=n/sum(n))

# ---- 2002: sex composition among household heads ----
setwd(wd_data_02)
data_2002<-fread("data_2002_clean.csv")
stats_2002<-data_2002 %>%
  filter(GRUD==1) %>%
  group_by(SEX) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(n=n/sum(n))

# ---- 1992: sex composition among household heads ----
setwd(wd_data_92)
data_1992<-fread("data_1992_clean.csv")
stats_1992<-data_1992 %>%
  filter(GRUD==1) %>%
  group_by(SEX) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(n=n/sum(n))


