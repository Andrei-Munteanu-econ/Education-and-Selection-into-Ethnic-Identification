# Loads birth records (2003-2011) and links to 2011 census mothers; outputs data_2011_clean_births.csv
#Load births

# --- Load and Parse Birth Registry Records ---

# Birth registry Stata file merged with 2003, 2005, and 2011 live-birth certificates (nv) and national person IDs (cnp)
setwd(wd_data_births)
data_births<-read_dta('census2011_mergedwith_nv2003_2005_2011_cnp.dta')
# Preserve original as backup before any transformations
data_births_raw<-data_births
data_births_head<-head(data_births,100)


# --- Parse Date Components from Concatenated Date Strings ---

# datan: birth date of the child encoded as "YYYY-MM-DD" string; extract year (AA), month (LL), day (ZZ)
data_births<-data_births %>%
  mutate(AA=as.numeric(substr(datan,1,4)),
         LL=as.numeric(substr(datan,6,7)),
         ZZ=as.numeric(substr(datan,9,10)))
# data_i: registration date of the birth certificate; AA_REG/LL_REG/ZZ_REG capture year/month/day of registration
# Registration date may differ from birth date (late registrations), useful for quality filtering
data_births<-data_births %>%
  mutate(AA_REG=as.numeric(substr(data_i,1,4)),
         LL_REG=as.numeric(substr(data_i,6,7)),
         ZZ_REG=as.numeric(substr(data_i,9,10)))
# datan_m: mother's birth date string; AA_MOM/LL_MOM/ZZ_MOM are year/month/day of mother's birth
data_births<-data_births %>%
  mutate(AA_MOM=as.numeric(substr(datan_m,1,4)),
         LL_MOM=as.numeric(substr(datan_m,6,7)),
         ZZ_MOM=as.numeric(substr(datan_m,9,10)))
# datan_t: father's birth date string; AA_POP/LL_POP/ZZ_POP are year/month/day of father's birth
data_births<-data_births %>%
  mutate(AA_POP=as.numeric(substr(datan_t,1,4)),
         LL_POP=as.numeric(substr(datan_t,6,7)),
         ZZ_POP=as.numeric(substr(datan_t,9,10)))
# datac_m: mother's marriage date; ANC = year of marriage, used as an additional matching key (rounds 7-8)
data_births<-data_births %>%
  mutate(ANC=as.numeric(substr(datac_m,1,4)))

colnames(data_births)


# --- Load 2011 Census Data ---

#
setwd(wd_data_11)
filename<-'data_2011_clean_v2.csv'
data_2011<-read_sample(filename)
data_2011<-read_data(filename,data_2011)


#####
# bkp<-data_2011
# data_2011<-data_2011 %>%
#   sample_n(100000)
#####


# --- Prepare Matching Subsets ---

# Restrict 2011 census to children born 2003-2011; these are the cohorts covered by the birth certificate files
data_2011_to_match<-data_2011 %>% filter(AA %in% c(2003:2011) )
# Clean birth registry: drop records with missing or zero county code; exclude births after Oct 2011 (census cut-off)
# and remove a specific known data artifact (AA==2011, LL==10, ZZ>20 = implausible day values in last month)
data_births_to_match<-data_births %>%
  filter(!is.na(judCensus) & judCensus!=0 & (AA<2011 | LL<=10 ) & !(AA==2011 & LL==10 & ZZ>20) ) %>%
  select(judCensus,sex,copiiCensus,
         AA_MOM,LL_MOM,ZZ_MOM,
         AA_POP,LL_POP,ZZ_POP,
         AA,LL,ZZ,
         AA_REG,LL_REG,ZZ_REG,
         sca,statut,ocup,sector_lucru,vnasca_m,copil_id,mama_id,locad_m,aa_alm,ll_alm,nat,dif,ANC)
#add mariage age, etc


# --- Sequential Probabilistic Matching Rounds ---
# Strategy: each round attempts to match unmatched birth certificates to 2011 census mothers
# using progressively fewer matching keys, accepting only unique (n==1) matches to avoid false positives.
# copil_id: unique birth-certificate identifier; id11: unique 2011 census person identifier.

#0 match moms directly, using all census variables
# Round 0: strictest match — mother's birth yr/mth/day + education + marital status + occupation
# + sector + number of children + county + marriage year/month + day-of-birth difference, no locality
matched_0<-base::merge(data_2011,
                       data_births_to_match  ,
                       by.x=c("AA","LL","ZZ","SCA","STATUT","OCUP","SECT","COPII","JUD_SECT","AA_ALM","LL_ALM","DIF"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  # Keep only uniquely matched birth certificates (n==1) to exclude ambiguous matches
  filter(n==1) %>%
  select(id11,HHID,AA,LL,ZZ,copil_id,AA_BIRTH,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG)

#match rate
matched<-matched_0
# Identify birth certificates not yet matched, to be passed to the next round
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

# Validation: share of matched census persons who also appear as a mother (id11_MOM) in the 2011 data
#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#1 all characteristics
# Round 1: same as Round 0 but adds SIRSUP (LAU2 locality code) as an extra key to resolve ambiguity
matched_1<-base::merge(data_2011,
                       data_births_to_match_unmatched  ,
                       by.x=c("AA","LL","ZZ","SCA","STATUT","OCUP","SECT","COPII","JUD_SECT","AA_ALM","LL_ALM","DIF","SIRSUP"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif","locad_m"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11,HHID,AA,LL,ZZ,copil_id,AA_BIRTH,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG)

matched<-bind_rows(matched,matched_1)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)


#############################################################
#2 add child birth yr
# Round 2: matches using child's birth year (AA) as an additional key alongside all mother characteristics
# Leverages the _MOM suffix variables in 2011 census (mother's attributes recorded on the child's census row)
matched_2<-base::merge(data_2011,
                       data_births_to_match_unmatched  ,
                       by.x=c("AA","AA_MOM","LL_MOM","ZZ_MOM","SCA_MOM","STATUT_MOM","OCUP_MOM","SECT_MOM","COPII_MOM","JUD_SECT","AA_ALM_MOM","LL_ALM_MOM","DIF_MOM"),
                       by.y=c("AA","AA_MOM","LL_MOM","ZZ_MOM","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  # Match was on child's row; recode back to mother-row variable names for stacking with other rounds
  select(id11_MOM,HHID,copil_id,AA_MOM,LL_MOM,ZZ_MOM,AA,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG) %>%
  rename(AA_BIRTH=AA) %>%
  rename_with(~str_remove(., '_MOM'))

matched<-bind_rows(matched,matched_2)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

# 2 add child birth yr and month
# Round 3: extends Round 2 by also matching on child's birth month (LL), tightening the date constraint
matched_3<-base::merge(data_2011,
                       data_births_to_match_unmatched  ,
                       by.x=c("AA","LL","AA_MOM","LL_MOM","ZZ_MOM","SCA_MOM","STATUT_MOM","OCUP_MOM","SECT_MOM","COPII_MOM","JUD_SECT","AA_ALM_MOM","LL_ALM_MOM","DIF_MOM"),
                       by.y=c("AA","LL","AA_MOM","LL_MOM","ZZ_MOM","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11_MOM,HHID,copil_id,AA_MOM,LL_MOM,ZZ_MOM,AA,AA_REG,LL,LL_REG,ZZ_BIRTH,ZZ_REG) %>%
  rename(AA_BIRTH=AA,LL_BIRTH=LL) %>%
  rename_with(~str_remove(., '_MOM'))

matched<-bind_rows(matched,matched_3)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#4 add child birth yr, mth and day
# Round 4: fully exact child date (year + month + day) alongside all mother characteristics
matched_4<-base::merge(data_2011,
                       data_births_to_match_unmatched  ,
                       by.x=c("AA","LL","ZZ","AA_MOM","LL_MOM","ZZ_MOM","SCA_MOM","STATUT_MOM","OCUP_MOM","SECT_MOM","COPII_MOM","JUD_SECT","AA_ALM_MOM","LL_ALM_MOM","DIF_MOM"),
                       by.y=c("AA","LL","ZZ","AA_MOM","LL_MOM","ZZ_MOM","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11_MOM,HHID,copil_id,AA_MOM,LL_MOM,ZZ_MOM,AA,AA_REG,LL,LL_REG,ZZ,ZZ_REG) %>%
  rename(AA_BIRTH=AA,LL_BIRTH=LL,ZZ_BIRTH=ZZ) %>%
  rename_with(~str_remove(., '_MOM'))

matched<-bind_rows(matched,matched_4)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#5 add dad birth year (without child birthdate)
# Round 5: introduces father's birth year (AA_SPOUSE in census = AA_POP in registry) as a discriminating key
# replaces child birthdate with paternal information to break remaining ties
matched_5<-base::merge(data_2011,
                       data_births_to_match_unmatched   ,
                       by.x=c("AA","LL","ZZ","AA_SPOUSE","SCA","STATUT","OCUP","SECT","COPII","JUD_SECT","AA_ALM","LL_ALM","DIF"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","AA_POP","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11_MOM,HHID,copil_id,AA_MOM,LL_MOM,ZZ_MOM,AA,AA_REG,LL,LL_REG,ZZ,ZZ_REG) %>%
  rename(AA_BIRTH=AA,LL_BIRTH=LL,ZZ_BIRTH=ZZ) %>%
  rename_with(~str_remove(., '_MOM'))

#match rate
matched<-bind_rows(matched,matched_5)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#6 add dad birth year and birth month
# Round 6: adds father's birth month (LL_SPOUSE / LL_POP) to further narrow paternal disambiguation
matched_6<-base::merge(data_2011,
                       data_births_to_match_unmatched   ,
                       by.x=c("AA","LL","ZZ","AA_SPOUSE","LL_SPOUSE","SCA","STATUT","OCUP","SECT","COPII","JUD_SECT","AA_ALM","LL_ALM","DIF"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","AA_POP","LL_POP","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11,HHID,AA,LL,ZZ,copil_id,AA_BIRTH,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG)

#match rate
matched<-bind_rows(matched,matched_6)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#7 use mariage year 1
# Round 7: uses ANC1 (first recorded marriage year in census) matched to ANC (marriage year in birth registry)
# Marriage year is a strong tie-breaker when mother's demographic profile alone is insufficient
matched_7<-base::merge(data_2011,
                       data_births_to_match_unmatched   ,
                       by.x=c("AA","LL","ZZ","AA_SPOUSE","SCA","STATUT","OCUP","SECT","COPII","JUD_SECT","AA_ALM","LL_ALM","DIF","ANC1"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","AA_POP","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif","ANC"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11,HHID,AA,LL,ZZ,copil_id,AA_BIRTH,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG)

#match rate
matched<-bind_rows(matched,matched_7)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#7 use mariage year 2
# Round 8: same as Round 7 but uses ANC2 (second marriage year in census) for women who remarried
matched_8<-base::merge(data_2011,
                       data_births_to_match_unmatched   ,
                       by.x=c("AA","LL","ZZ","AA_SPOUSE","SCA","STATUT","OCUP","SECT","COPII","JUD_SECT","AA_ALM","LL_ALM","DIF","ANC2"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","AA_POP","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif","ANC"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11,HHID,AA,LL,ZZ,copil_id,AA_BIRTH,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG)

#match rate
matched<-bind_rows(matched,matched_8)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#8 use dad birth year and child birth year
# Round 9: matches from the child's census row using both father's birth year and child's birth year
# This catches cases where the mother's own census row could not be linked but the child's row has enough info
matched_9<-base::merge(data_2011,
                       data_births_to_match_unmatched   ,
                       by.x=c("AA_MOM","LL_MOM","ZZ_MOM","AA_POP","AA","SCA_MOM","STATUT_MOM","OCUP_MOM","SECT_MOM","COPII_MOM","JUD_SECT","AA_ALM_MOM","LL_ALM_MOM","DIF_MOM"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","AA_POP","AA","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11_MOM,HHID,copil_id,AA_MOM,LL_MOM,ZZ_MOM,AA,AA_REG,LL,LL_REG,ZZ,ZZ_REG) %>%
  rename(AA_BIRTH=AA,LL_BIRTH=LL,ZZ_BIRTH=ZZ) %>%
  rename_with(~str_remove(., '_MOM'))


#match rate
matched<-bind_rows(matched,matched_9)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

# Distribution of matched children's birth years — sanity check for coverage across the 2003-2011 window
table(matched$AA)


# Check for any birth certificate matched to more than one census mother (should be zero after n==1 filters)
duplicates<-matched %>%
  group_by(copil_id) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(n>1)

# Output: raw matched pairs (all rounds stacked) before final deduplication
setwd(wd_data_11)
fwrite(matched,"matches_census_raw.csv")


# --- Final Deduplication: One-to-One Census-to-Birth-Certificate Mapping ---

########################
# Subset 2011 census to children born 2002-2011 and keep only person and mother identifiers
lhs<-data_2011 %>%
  select(AA,LL,ZZ,id11,id11_MOM) %>%
  filter(AA %in% 2002:2011)
rhs<-matched %>%
  select(AA_BIRTH,LL_BIRTH,ZZ_BIRTH,id11,copil_id)

#remove birth certificates with multiple matches
# For each birth certificate (copil_id), keep the census child whose date is closest to the registered birth date
# Distance metric: 365*(year diff) + 30*(month diff) + (day diff) — approximates calendar distance in days
matches_kids_final<-lhs %>%
  inner_join(rhs,by=c("id11_MOM"="id11")) %>%
  group_by(copil_id) %>%
  arrange(abs(365*(AA-AA_BIRTH)+30*(LL-LL_BIRTH)+(ZZ-ZZ_BIRTH))) %>%
  slice(1)

##remove census entries with multiple birth certificate matches
# For each census child (id11), keep the birth certificate whose date is closest — ensures 1-to-1 mapping
matches_kids_final<-matches_kids_final %>%
  group_by(id11) %>%
  arrange(abs(365*(AA-AA_BIRTH)+30*(LL-LL_BIRTH)+(ZZ-ZZ_BIRTH))) %>%
  slice(1)

# Number of unique mothers successfully linked to at least one birth certificate
length(unique(matches_kids_final$id11_MOM))


matches_kids_final<-matches_kids_final %>%
  ungroup

# Output: final deduplicated census-to-birth-certificate matches; used downstream to impute mother ethnicity for children
setwd(wd_data_11)
fwrite(matches_kids_final,"matches_census_final.csv")

########################




#
#
# ###add info to 2011 data
# #moms
# matches_moms<-matched %>%
#   select(id11,copil_id)
#
# setwd(wd_data_11)
# fwrite(matches_moms,"matches_census_births_mom.csv")
#
# #kids
# data_2011_kids<-data_2011_to_match %>%
#   select(id11_MOM,AA,LL,ZZ,NOP)
#
# matches_kids<-matched %>%
#   select(id11,AA_BIRTH,LL_BIRTH,ZZ_BIRTH,copil_id)
#
# #make sure matches are unique; 1 match per birth cerrtificate and 1 match per census respondent
# matches_kids_final<-data_2011_kids %>%
#   inner_join(matches_kids,by=c("id11_MOM"="id11")) %>%
#   group_by(copil_id) %>%
#   arrange(abs(365*(AA-AA_BIRTH)+30*(LL-LL_BIRTH)+(ZZ-ZZ_BIRTH))) %>%
#   slice(1)
#
# matches_kids_final<-matches_kids_final %>%
#   group_by(id11_MOM,NOP) %>%
#   arrange(abs(365*(AA-AA_BIRTH)+30*(LL-LL_BIRTH)+(ZZ-ZZ_BIRTH))) %>%
#   slice(1)
#
#
# hist(matches_kids_final$AA-matches_kids_final$AA_BIRTH)
#
# matches_kids_final<-matches_kids_final %>%
#   select(id11_MOM,copil_id,NOP,AA,LL,ZZ)
#
# setwd(wd_data_11)
# fwrite(matches_kids_final,"matches_census_births_kids.csv")
#
# # test<-matches_kids_final %>%
# #   group_by(id11_MOM,NOP) %>%
# #   mutate(n=n()) %>%
# #   ungroup() %>%
# #   filter(n>1)
#
