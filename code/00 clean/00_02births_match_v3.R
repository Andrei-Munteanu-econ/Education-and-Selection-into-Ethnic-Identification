# =====================================================================
# Match birth-certificate records to mothers in the 2011 census
# Produces:  matches_census_raw.csv  (all candidate matches),
#            matches_census_final.csv (de-duplicated mother-child links)
#            (intermediate; both written to wd_data_11)
# Inputs:    birth records (census2011_mergedwith_nv2003_2005_2011_cnp.dta),
#            cleaned 2011 census (data_2011_clean_v2.csv)
# Summary:   Parses birth/registration/parent dates from the birth file, then
#            links mothers in the census to birth certificates through a
#            cascade of merges that progressively relax/swap the matching keys
#            (mother characteristics, child birthdate, father birthdate,
#            marriage year). Each step keeps only unique 1:1 matches and feeds
#            the still-unmatched records to the next step. Finally resolves
#            remaining duplicates by closest birthdate. No paper exhibit.
# =====================================================================

# ---- Load and parse birth records ----
#Load births
setwd(wd_data_births)
data_births<-read_dta('census2011_mergedwith_nv2003_2005_2011_cnp.dta')
data_births_raw<-data_births

data_births<-data_births %>%
  mutate(AA=as.numeric(substr(datan,1,4)),
         LL=as.numeric(substr(datan,6,7)),
         ZZ=as.numeric(substr(datan,9,10)))
data_births<-data_births %>%
  mutate(AA_REG=as.numeric(substr(data_i,1,4)),
         LL_REG=as.numeric(substr(data_i,6,7)),
         ZZ_REG=as.numeric(substr(data_i,9,10)))
data_births<-data_births %>%
  mutate(AA_MOM=as.numeric(substr(datan_m,1,4)),
         LL_MOM=as.numeric(substr(datan_m,6,7)),
         ZZ_MOM=as.numeric(substr(datan_m,9,10)))
data_births<-data_births %>%
  mutate(AA_POP=as.numeric(substr(datan_t,1,4)),
         LL_POP=as.numeric(substr(datan_t,6,7)),
         ZZ_POP=as.numeric(substr(datan_t,9,10)))
data_births<-data_births %>%
  mutate(ANC=as.numeric(substr(datac_m,1,4)))


# ---- Load 2011 census and restrict to matchable records ----
setwd(wd_data_11)
filename<-'data_2011_clean_v2.csv'
data_2011<-read_sample(filename)
data_2011<-read_data(filename,data_2011)


data_2011_to_match<-data_2011 %>% filter(AA %in% c(2003:2011) )
data_births_to_match<-data_births %>% 
  filter(!is.na(judCensus) & judCensus!=0 & (AA<2011 | LL<=10 ) & !(AA==2011 & LL==10 & ZZ>20) ) %>%
  select(judCensus,sex,copiiCensus,
         AA_MOM,LL_MOM,ZZ_MOM,
         AA_POP,LL_POP,ZZ_POP,
         AA,LL,ZZ,
         AA_REG,LL_REG,ZZ_REG,
         sca,statut,ocup,sector_lucru,vnasca_m,copil_id,mama_id,locad_m,aa_alm,ll_alm,nat,dif,ANC)
#add mariage age, etc



# ---- Cascade of matching passes (relaxing/swapping keys at each step) ----
#0 match moms directly, using all census variables
matched_0<-base::merge(data_2011,
                       data_births_to_match  ,
                       by.x=c("AA","LL","ZZ","SCA","STATUT","OCUP","SECT","COPII","JUD_SECT","AA_ALM","LL_ALM","DIF"),
                       by.y=c("AA_MOM","LL_MOM","ZZ_MOM","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11,HHID,AA,LL,ZZ,copil_id,AA_BIRTH,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG)

#match rate
matched<-matched_0
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

#1 all characteristics
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
matched_2<-base::merge(data_2011,
                       data_births_to_match_unmatched  ,
                       by.x=c("AA","AA_MOM","LL_MOM","ZZ_MOM","SCA_MOM","STATUT_MOM","OCUP_MOM","SECT_MOM","COPII_MOM","JUD_SECT","AA_ALM_MOM","LL_ALM_MOM","DIF_MOM"),
                       by.y=c("AA","AA_MOM","LL_MOM","ZZ_MOM","sca","statut","ocup","sector_lucru","copiiCensus","judCensus","aa_alm","ll_alm","dif"),
                       suffixes=c("","_BIRTH")) %>%
  group_by(copil_id) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n==1) %>%
  select(id11_MOM,HHID,copil_id,AA_MOM,LL_MOM,ZZ_MOM,AA,AA_REG,LL_BIRTH,LL_REG,ZZ_BIRTH,ZZ_REG) %>%
  rename(AA_BIRTH=AA) %>%
  rename_with(~str_remove(., '_MOM')) 

matched<-bind_rows(matched,matched_2)
data_births_to_match_unmatched<-anti_join(data_births_to_match,matched,by="copil_id")
1-nrow(data_births_to_match_unmatched)/nrow(data_births_to_match) #match rate

#how many moms not in moms list?
sum(matched$id11 %in% data_2011$id11_MOM)/length(matched$id11)

# 2 add child birth yr and month
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

table(matched$AA)


duplicates<-matched %>%
  group_by(copil_id) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(n>1)

# ---- Write raw (pre-dedup) match table ----
setwd(wd_data_11)
fwrite(matched,"matches_census_raw.csv")


# ---- De-duplicate to one mother-child link, keeping closest birthdate ----
########################
lhs<-data_2011 %>%
  select(AA,LL,ZZ,id11,id11_MOM) %>%
  filter(AA %in% 2002:2011)
rhs<-matched %>% 
  select(AA_BIRTH,LL_BIRTH,ZZ_BIRTH,id11,copil_id)

#remove birth certificates with multiple matches
matches_kids_final<-lhs %>%
  inner_join(rhs,by=c("id11_MOM"="id11")) %>%
  group_by(copil_id) %>% 
  arrange(abs(365*(AA-AA_BIRTH)+30*(LL-LL_BIRTH)+(ZZ-ZZ_BIRTH))) %>%
  slice(1) 

##remove census entries with multiple birth certificate matches
matches_kids_final<-matches_kids_final %>%
  group_by(id11) %>% 
  arrange(abs(365*(AA-AA_BIRTH)+30*(LL-LL_BIRTH)+(ZZ-ZZ_BIRTH))) %>%
  slice(1)

length(unique(matches_kids_final$id11_MOM))


matches_kids_final<-matches_kids_final %>%
  ungroup

# ---- Write final de-duplicated census-birth linkage ----
setwd(wd_data_11)
fwrite(matches_kids_final,"matches_census_final.csv")
