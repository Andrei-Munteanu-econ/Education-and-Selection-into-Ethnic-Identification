#This script generates Figure A.3 of the Appendix


# --- Load Data ---
# Linked census panel files restricted to uniquely matched records (genderless cell_id)
# "genderless" matching avoids spurious non-matches driven by gender-ratio differences across censuses
setwd(wd_data_linked)
data_2002_2011<-fread('data_2002_2011_unique_genderless.csv')
data_1992_2011<-fread('data_1992_2011_unique_genderless.csv')



# eth: 1992 language/mother-tongue file; LIM_baseline will be derived from this for Roma sub-group splits
setwd(wd_data_92_other)
eth <- read.dbf("LIMBA92.DBF", as.is = F)
# iter: number of bootstrap replications for town-level resampling (accounts for within-town correlation)
iter<-100
# mycolors: named color vector ensures Roma and Hungarian series are visually distinguished consistently
mycolors<-c("Roma"="red","Hungarian"="gray60")

# --- Clean and Recode: 1992-2011 Panel ---
# data92: working copy of the 1992-2011 linked panel, with derived categorical variables
# Suffix convention: "_baseline" replaces "_1992" throughout (via rename_all at the end) to
# allow the same helper functions to operate on both the 1992 and 2002 base-year panels
data92<-data_1992_2011 %>%
  # Education: coarsened from years_2011 (2011 census years of schooling) into five ordered groups
  # used as the x-axis in the education persistence plots
  mutate(Education=case_when(years_2011==0 ~ "None",
                             years_2011 %in% 2:4 ~ "Primary School",
                             years_2011 %in% 8:10  ~ "Middle School",
                             years_2011 %in% 12:13 ~ "High School or Vocational",
                             years_2011 %in% 14:16 ~ "Post- Secondary")) %>%
  # OCUP_agg_2011: top-level ISCO-88 major group obtained by dividing the 4-digit code by 1000
  mutate(OCUP_agg_2011=floor(OCUP_2011/1000)) %>%
  # Occupation: four broad categories used as the x-axis in the occupation persistence plots
  mutate(Occupation=case_when(OCUP_agg_2011 %in% c(0) ~ "Unemployed",
                              OCUP_agg_2011 %in% c(1:2,4) ~ "Managers, Professionals, Clerks",
                              OCUP_agg_2011 %in% c(3,5,7:8) ~ "Skilled Labor",
                              OCUP_agg_2011 %in% c(6,9) ~ "Agriculture and Unskilled Labor")) %>%
  # Lock in the ordering so plots display education from least to most
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) %>%
  # mismatch: TRUE when the sex recorded in 1992 differs from 2011 — flags likely false census links
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  # HUN_1992/HUN_2011: Hungarian ethnicity flags; ET_2011 uses 4-digit codes (1101-1103 = Hungarian sub-groups)
  mutate(HUN_1992=ifelse(ET_1992==11,T,F),
         HUN_2011=ifelse(ET_2011 %in% c(1101,1102,1103),T,F)) %>%
  # GER_1992/GER_2011: German ethnicity flags; ET_2011 range 1300-1399 covers all German sub-groups
  mutate(GER_1992=ifelse(ET_1992==13,T,F),
         GER_2011=ifelse(ET_2011 %in% c(1300:1399),T,F)) %>%
  # OTHER_1992/OTHER_2011: all non-Romanian, non-Roma, non-Hungarian minority groups
  mutate(OTHER_1992=ifelse(ET_1992%in% 13:90,T,F),
         OTHER_2011=ifelse(ET_2011 %in% c(1300:9000),T,F))%>%
  # Standardise column naming: replace "_1992" suffix with "_baseline" so helper functions
  # (get_stats, get_town) work identically on both the 1992 and 2002 base-year datasets
  rename_all(~gsub("_1992","_baseline",.))







# --- Clean and Recode: 2002-2011 Panel ---
# data02: analogous working copy of the 2002-2011 linked panel; same transformations as data92
# mismatch here compares SEX_2002 vs SEX_2011; "_2002" suffix is replaced with "_baseline"
data02<-data_2002_2011 %>%
  mutate(Education=case_when(years_2011==0 ~ "None",
                             years_2011 %in% 2:4 ~ "Primary School",
                             years_2011 %in% 8:10  ~ "Middle School",
                             years_2011 %in% 12:13 ~ "High School or Vocational",
                             years_2011 %in% 14:16 ~ "Post- Secondary")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) %>%
  mutate(OCUP_agg_2011=floor(OCUP_2011/1000)) %>%
  mutate(Occupation=case_when(OCUP_agg_2011 %in% c(0) ~ "Unemployed",
                              OCUP_agg_2011 %in% c(1:2,4) ~ "Managers, Professionals, Clerks",
                              OCUP_agg_2011 %in% c(3,5,7:8) ~ "Skilled Labor",
                              OCUP_agg_2011 %in% c(6,9) ~ "Agriculture and Unskilled Labor")) %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  mutate(HUN_2002=ifelse(ET_2002==11,T,F),
         HUN_2011=ifelse(ET_2011 %in% c(1101,1102,1103),T,F)) %>%
  mutate(GER_2002=ifelse(ET_2002==13,T,F),
         GER_2011=ifelse(ET_2011 %in% c(1300:1399),T,F)) %>%
  mutate(OTHER_2002=ifelse(ET_2002 %in% 13:90,T,F),
         OTHER_2011=ifelse(ET_2011 %in% c(1300:9000),T,F)) %>%
  rename_all(~gsub("_2002","_baseline",.))

# --- Prepare Town Lists for Bootstrap Sampling ---
# towns02 / towns92: full set of locality codes (SIRSUP) present in each panel
# Used as the universe from which bootstrap resamples are drawn
towns02<-unique(data02$SIRSUP_baseline)
towns92<-unique(data92$SIRSUP_baseline)

# Restrict town lists to localities with Hungarian household heads in BOTH census years
# GRUD_2011==1 & GRUD_baseline==1 restricts to household heads in both waves
# This ensures the bootstrap resamples only towns where the ethnic group is actually observed
towns02_hu<-unique(unlist(data02 %>% filter(ET_baseline==11 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_hu<-unique(unlist(data92 %>% filter(ET_baseline==11 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))

# German household-head towns
towns02_ge<-unique(unlist(data02 %>% filter(ET_baseline==13 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_ge<-unique(unlist(data92 %>% filter(ET_baseline==13 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))

# Other-minority household-head towns (ET codes 14-90 cover Turkish/Tatar, Slovak, etc.)
towns02_oth<-unique(unlist(data02 %>% filter(ET_baseline %in% 14:90 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_oth<-unique(unlist(data92 %>% filter(ET_baseline %in% 14:90 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))


# Roma household-head towns (ET_baseline==12 is the Roma code)
towns02_rom<-unique(unlist(data02 %>% filter(ET_baseline==12 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom<-unique(unlist(data92 %>% filter(ET_baseline==12 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))

# Roma with Hungarian mother tongue (LIM_baseline==11): sub-group that could plausibly pass as Hungarian
towns02_rom_hu<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==11 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_hu<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==11 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))

# Roma with Romani mother tongue (LIM_baseline==12): most linguistically distinct sub-group
towns02_rom_romani<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==12 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_romani<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==12 & GRUD_2011==1 & GRUD_baseline==1) %>% select(SIRSUP_baseline), use.names = FALSE))


# --- Helper Function: get_stats (mismatch-corrected persistence by education) ---
# Computes the mismatch-corrected ethnic persistence rate by education cell.
# The correction formula removes the contribution of false census links (sex mismatches)
# from the observed persistence rate: p_real = (p_all - p_mismatch * p_eth_mismatch) / (1 - p_mismatch)
# where:
#   p_all            = raw share still identifying as the same ethnicity in 2011
#   p_mismatch       = estimated false-link share (sex-mismatch rate * 2, to account for symmetric error)
#   p_eth_mismatch   = share of mismatch records that still show the same ethnicity (upper-bounds the bias)
# town_sample is a globally-scoped bootstrap draw set before each call to this function
get_stats<-function(data){

  result<-data %>%
    # Restrict to household heads in both census waves; GRUD==1 is the head-of-household indicator
    filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    # ETH_baseline is a logical flag set by the caller to identify the ethnic group of interest
    filter(ETH_baseline==T) %>%
    # Resample localities with replacement (town-level bootstrap) then expand records accordingly
    right_join(town_sample,by="SIRSUP_baseline",relationship="many-to-many") %>%
    # Drop rows created by the right join that have no matched individual (all-NA rows from unmatched towns)
    filter(!if_all(everything(), is.na)) %>%
    group_by(Education) %>%
    # p_all: raw within-education-cell share retaining the baseline ethnicity in 2011
    mutate(p_all=mean(ETH_2011==T,na.rm=T)) %>%
    group_by(Education,p_all) %>%
    # p_eth_mismatch: among sex-mismatched records, share showing ethnic persistence (used in correction formula)
    # p_mismatch: doubled sex-mismatch rate approximates two-sided false-link probability
    summarise(n=n(),
              p_eth_mismatch=mean(ETH_2011[mismatch==T]==T,na.rm=T),
              .groups = 'drop',
              p_mismatch=sum(mismatch==T)/n()*2,n=n()) %>%
    # Replace NaN (no mismatch records in cell) with 0; no bias to correct when there are no mismatches
    mutate( p_eth_mismatch=ifelse(is.nan(p_eth_mismatch),0,p_eth_mismatch)) %>%
    group_by(Education) %>%
    # Apply mismatch correction to obtain bias-adjusted ethnic persistence rate
    mutate(p_real=(p_all-p_mismatch*p_eth_mismatch)/(1-p_mismatch)) %>%
    ungroup %>%
    select(Education,p_real)
  # result<-data %>%
  #   filter(GRUD_2011==1 & GRUD_baseline==1) %>%
  #   filter(ETH_baseline==T) %>%
  #   right_join(town_sample,by="SIRSUP_baseline",relationship="many-to-many") %>%
  #   filter(!if_all(everything(), is.na)) %>%
  #   group_by(Education) %>%
  #   mutate(p_all=mean(ETH_2011==T,na.rm=T)) %>%
  #   group_by(mismatch,Education,p_all) %>%
  #   summarise(n=n(),p_eth_mismatch=mean(ETH_2011==T,na.rm=T), .groups = 'drop') %>%
  #   group_by(Education) %>%
  #   mutate(p_mismatch=n/sum(n)*2,n=sum(n)) %>%
  #   filter(mismatch==T) %>%
  #   mutate(p_real=(p_all-p_mismatch*p_eth_mismatch)/(1-p_mismatch)) %>%
  #   ungroup %>%
  #   select(Education,p_real)

  return(result)
}

# --- Helper Function: get_town (bootstrap town draw) ---
# Draws a with-replacement sample of the same size as the input town vector.
# The result is used as the right-hand side of the right_join inside get_stats/get_stats_ocup,
# effectively reweighting observations by how many times their town is drawn.
get_town<-function(towns){
  town_sample<- data.frame(SIRSUP_baseline=
                               sample(x = towns,
                                      size = length(towns),
                                      replace = TRUE))



  return(town_sample)
}

# --- Bootstrap Loop: Education Persistence by Ethnicity ---
# Runs iter=100 bootstrap replications, each drawing a fresh town-level resample per ethnic group.
# set.seed(i) inside the loop ensures each iteration is reproducible while varying across iterations.
# Results for all seven ethnic groups and both census pairs are stored as a list of data frames.
data_1_r_temp<-list()
for(i in 1:iter){
  set.seed(i)

  #roma
  town_sample<-get_town(towns02)
  stats_2002_temp_roma<-get_stats(data02 %>%
                                    filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92)
  stats_1992_temp_roma<-get_stats(data92 %>%
                                    filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #hu
  town_sample<-get_town(towns02_hu)
  stats_2002_temp_hun<-get_stats(data02 %>%
                                   filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=HUN_baseline,ETH_2011=HUN_2011))
  town_sample<-get_town(towns92_hu)
  stats_1992_temp_hun<-get_stats(data92 %>%
                                   filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=HUN_baseline,ETH_2011=HUN_2011))

  #ge
  town_sample<-get_town(towns02_ge)
  stats_2002_temp_ger<-get_stats(data02 %>%
                                   filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=GER_baseline,ETH_2011=GER_2011))
  town_sample<-get_town(towns92_ge)
  stats_1992_temp_ger<-get_stats(data92 %>%
                                   filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=GER_baseline,ETH_2011=GER_2011))

  #other
  town_sample<-get_town(towns02_oth)
  stats_2002_temp_oth<-get_stats(data02 %>%
                                   filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=OTHER_baseline,ETH_2011=OTHER_2011))
  town_sample<-get_town(towns92_oth)
  stats_1992_temp_oth<-get_stats(data92 %>%
                                   filter(GRUD_2011==1 & GRUD_baseline==1) %>%
    mutate(ETH_baseline=OTHER_baseline,ETH_2011=OTHER_2011))

  #roma-hungarian
  # Roma who declared Hungarian as mother tongue in the baseline census (LIM_baseline==11)
  town_sample<-get_town(towns02_rom_hu)
  stats_2002_temp_roma_hun<-get_stats(data02 %>%
                                        filter(GRUD_2011==1 & GRUD_baseline==1) %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_hu)
  stats_1992_temp_roma_hun<-get_stats(data92 %>%
                                        filter(GRUD_2011==1 & GRUD_baseline==1) %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-ro
  # Roma who declared Romanian as mother tongue (LIM_baseline==10); most assimilated linguistic sub-group
  town_sample<-get_town(towns02_rom)
  stats_2002_temp_roma_ro<-get_stats(data02 %>%
                                            filter(LIM_baseline==10) %>%
                                       filter(GRUD_2011==1 & GRUD_baseline==1) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom)
  stats_1992_temp_roma_ro<-get_stats(data92 %>%
                                            filter(LIM_baseline==10) %>%
                                       filter(GRUD_2011==1 & GRUD_baseline==1) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-romani
  # Roma who declared Romani as mother tongue (LIM_baseline==12); least assimilated linguistic sub-group
  town_sample<-get_town(towns02_rom_romani)
  stats_2002_temp_roma_romani<-get_stats(data02 %>%
                                                filter(LIM_baseline==12) %>%
                                           filter(GRUD_2011==1 & GRUD_baseline==1) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_romani)
  stats_1992_temp_roma_romani<-get_stats(data92 %>%
                                                filter(LIM_baseline==12) %>%
                                           filter(GRUD_2011==1 & GRUD_baseline==1) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  # Combine all ethnic-group results for the 1992-2011 panel into one wide row per education cell
  # suffix conventions keep column names unique (e.g., p_real becomes p_real_rom for Roma)
  stats_1992_temp<-stats_1992_temp_roma %>%
    inner_join(stats_1992_temp_hun,by=c("Education"),suffix=c("","_hun")) %>%
    inner_join(stats_1992_temp_ger,by=c("Education"),suffix=c("","_ger")) %>%
    inner_join(stats_1992_temp_oth,by=c("Education"),suffix=c("","_oth")) %>%
    inner_join(stats_1992_temp_roma_hun,by=c("Education"),suffix=c("","_rom_hu")) %>%
    inner_join(stats_1992_temp_roma_ro,by=c("Education"),suffix=c("","_rom_ro")) %>%
    inner_join(stats_1992_temp_roma_romani,by=c("Education"),suffix=c("_rom","_rom_romani")) %>%
    mutate(Censuses='1992 - 2011')

  # Analogous wide join for the 2002-2011 panel
  stats_2002_temp<-stats_2002_temp_roma %>%
    inner_join(stats_2002_temp_hun,by=c("Education"),suffix=c("","_hun")) %>%
    inner_join(stats_2002_temp_ger,by=c("Education"),suffix=c("","_ger")) %>%
    inner_join(stats_2002_temp_oth,by=c("Education"),suffix=c("","_oth")) %>%
    inner_join(stats_2002_temp_roma_hun,by=c("Education"),suffix=c("","_rom_hu")) %>%
    inner_join(stats_2002_temp_roma_ro,by=c("Education"),suffix=c("","_rom_ro")) %>%
    inner_join(stats_2002_temp_roma_romani,by=c("Education"),suffix=c("_rom","_rom_romani")) %>%
    mutate(Censuses='2002 - 2011')



 data_1_r_temp[[i]]<-bind_rows(stats_1992_temp,stats_2002_temp)
}

# --- Aggregate Bootstrap Results: Education ---
# Stack all 100 bootstrap draws, reshape to long format by ethnicity, then
# summarise each (Censuses x Education x Ethnicity) cell as median + 5th/95th percentiles
# This gives point estimates with 90% bootstrap confidence intervals for the figure
data_1_r<-bind_rows(data_1_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Education,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95,na.rm=T),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05,na.rm=T),.names = "{col}_05")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) %>%
  # Map internal column-name codes to human-readable ethnicity labels
  mutate(Ethnicity=case_when(Ethnicity=="p_real_rom" ~ "Roma",
                             Ethnicity=="p_real_ger" ~ "German",
                             Ethnicity=="p_real_hun" ~ "Hungarian",
                             Ethnicity=="p_real_oth" ~ "Other",
                             Ethnicity=="p_real_roma_hu" ~ "Roma - Hungarian",
                             Ethnicity=="p_real_roma_ro" ~ "Roma - Romanian",
                             Ethnicity=="p_real_roma_romani" ~ "Roma - Romani"
                             )) %>%
  mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian","German","Other","Roma - Hungarian","Roma - Romanian","Roma - Romani"))) %>%
  # Sample: convenience label combining ethnicity and census pair for facet/filter operations
  mutate(Sample=case_when(Ethnicity=="Roma" & Censuses=="1992 - 2011" ~ "Roma '92-'11",
                          Ethnicity=="Roma" & Censuses=="2002 - 2011" ~ "Roma '02-'11",
                          Ethnicity=="Hungarian" & Censuses=="1992 - 2011" ~ "Hungarian '92-'11",
                          Ethnicity=="Hungarian" & Censuses=="2002 - 2011" ~ "Hungarian '02-'11",
                          Ethnicity=="Other" & Censuses=="1992 - 2011" ~ "Other '92-'11",
                          Ethnicity=="Other" & Censuses=="2002 - 2011" ~ "Other '02-'11"
                          ))




# --- Education Persistence Plots ---
# g1: Roma vs Hungarian household heads, 1992-2011 panel
# Error bars span the 5th-95th percentile of the bootstrap distribution (90% CI)
# Dodged so that Roma and Hungarian markers at the same education level are visually separated
g1<-ggplot(data=data_1_r %>%
             filter(Ethnicity %in% c("Roma","Hungarian")) %>%
             # mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian"))) %>%
             filter(Sample %in% c("Hungarian '92-'11","Roma '92-'11","Other '92-'11")),
           aes(x=Education,y=Percentage_med,group=Ethnicity,color=Ethnicity))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  scale_color_manual(values =mycolors) +
  labs(x="Education Level",y="Persistence in Declared Ethnicity")
g1

# g1_other: "Other" minority household heads, 1992-2011 panel (no color mapping needed, single series)
g1_other<-ggplot(data=data_1_r %>%
             filter(Ethnicity %in% c("Other")) %>%
             # mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian"))) %>%
             filter(Sample %in% c("Hungarian '92-'11","Roma '92-'11","Other '92-'11")),
           aes(x=Education,y=Percentage_med,group=Ethnicity))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  # scale_color_manual(values =mycolors) +
  labs(x="Education Level",y="Persistence in Declared Ethnicity")
g1_other

# g1_2002: Roma vs Hungarian household heads, 2002-2011 panel (robustness using shorter panel)
g1_2002<-ggplot(data=data_1_r %>%
             filter(Ethnicity %in% c("Roma","Hungarian")) %>%
             filter(Sample %in% c("Hungarian '02-'11","Roma '02-'11","Other '02-'11")),
           aes(x=Education,y=Percentage_med,color=Ethnicity,group=Ethnicity))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  scale_color_manual(values =mycolors) +
  labs(x="Education Level",y="Persistence in Declared Ethnicity")
g1_2002

# g1_other_2002: "Other" minority household heads, 2002-2011 panel
g1_other_2002<-ggplot(data=data_1_r %>%
                   filter(Ethnicity %in% c("Other")) %>%
                   # mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian"))) %>%
                   filter(Sample %in% c("Hungarian '02-'11","Roma '02-'11","Other '02-'11")),
                 aes(x=Education,y=Percentage_med,group=Ethnicity))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  #scale_color_manual(values =mycolors) +
  labs(x="Education Level",y="Persistence in Declared Ethnicity")
g1_other_2002

# (Commented-out individual pdf exports for the education plots; superseded by combined g3 below)
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_hh_heads.pdf",width=8,height=5)
# g1
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_2002_hh_heads.pdf",width=8,height=5)
# g1_2002
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_other_hh_heads.pdf",width=8,height=5)
# g1_other
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_other_2002_hh_heads.pdf",width=8,height=5)
# g1_other_2002
# dev.off()


# --- Helper Function: get_stats_ocup (mismatch-corrected persistence by occupation) ---
# Mirrors get_stats but groups by Occupation instead of Education.
# Note: GRUD filter is omitted here (applied externally before calling); NA occupation rows dropped.
# The mismatch correction logic is identical to get_stats.
#ocupation graphs----
get_stats_ocup<-function(data){
  result<-data %>%
    filter(!is.na(Occupation)) %>%
    filter(ETH_baseline==T) %>%
    right_join(town_sample,by="SIRSUP_baseline",relationship="many-to-many") %>%
    group_by(Occupation) %>%
    mutate(p_all=mean(ETH_2011==T,na.rm=T)) %>%
    group_by(mismatch,Occupation,p_all) %>%
    summarise(n=n(),p_eth_mismatch=mean(ETH_2011==T,na.rm=T), .groups = 'drop') %>%
    group_by(Occupation) %>%
    mutate(p_mismatch=n/sum(n)*2,n=sum(n)) %>%
    filter(mismatch==T) %>%
    mutate(p_real=(p_all-p_mismatch*p_eth_mismatch)/(1-p_mismatch)) %>%
    ungroup %>%
    select(Occupation,p_real)

  return(result)
}

# --- Bootstrap Loop: Occupation Persistence by Ethnicity ---
# Structure is identical to the education bootstrap above; uses get_stats_ocup instead of get_stats.
data_2_r_temp<-list()
for(i in 1:iter){
  set.seed(i)
  print(i)

  #roma
  town_sample<-get_town(towns02)
  stats_2002_temp_roma<-get_stats_ocup(data02 %>%
                                    mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92)
  stats_1992_temp_roma<-get_stats_ocup(data92 %>%
                                    mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #hu
  town_sample<-get_town(towns02_hu)
  stats_2002_temp_hun<-get_stats_ocup(data02 %>%
                                   mutate(ETH_baseline=HUN_baseline,ETH_2011=HUN_2011))
  town_sample<-get_town(towns92_hu)
  stats_1992_temp_hun<-get_stats_ocup(data92 %>%
                                   mutate(ETH_baseline=HUN_baseline,ETH_2011=HUN_2011))

  #ge
  town_sample<-get_town(towns02_ge)
  stats_2002_temp_ger<-get_stats_ocup(data02 %>%
                                   mutate(ETH_baseline=GER_baseline,ETH_2011=GER_2011))
  town_sample<-get_town(towns92_ge)
  stats_1992_temp_ger<-get_stats_ocup(data92 %>%
                                   mutate(ETH_baseline=GER_baseline,ETH_2011=GER_2011))

  #other
  town_sample<-get_town(towns02_oth)
  stats_2002_temp_oth<-get_stats_ocup(data02 %>%
                                   mutate(ETH_baseline=OTHER_baseline,ETH_2011=OTHER_2011))
  town_sample<-get_town(towns92_oth)
  stats_1992_temp_oth<-get_stats_ocup(data92 %>%
                                   mutate(ETH_baseline=OTHER_baseline,ETH_2011=OTHER_2011))

  #roma-hungarian
  town_sample<-get_town(towns02_rom_hu)
  stats_2002_temp_roma_hun<-get_stats_ocup(data02 %>%
                                        filter(LIM_baseline==11) %>%
                                        mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_hu)
  stats_1992_temp_roma_hun<-get_stats_ocup(data92 %>%
                                        filter(LIM_baseline==11) %>%
                                        mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-ro
  town_sample<-get_town(towns02_rom)
  stats_2002_temp_roma_ro<-get_stats_ocup(data02 %>%
                                       filter(LIM_baseline==10) %>%
                                       mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom)
  stats_1992_temp_roma_ro<-get_stats_ocup(data92 %>%
                                       filter(LIM_baseline==10) %>%
                                       mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-romani
  town_sample<-get_town(towns02_rom_romani)
  stats_2002_temp_roma_romani<-get_stats_ocup(data02 %>%
                                           filter(LIM_baseline==12) %>%
                                           mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_romani)
  stats_1992_temp_roma_romani<-get_stats_ocup(data92 %>%
                                           filter(LIM_baseline==12) %>%
                                           mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))


  stats_1992_temp<-stats_1992_temp_roma %>%
    inner_join(stats_1992_temp_hun,by=c("Occupation"),suffix=c("","_hun")) %>%
    inner_join(stats_1992_temp_ger,by=c("Occupation"),suffix=c("","_ger")) %>%
    inner_join(stats_1992_temp_oth,by=c("Occupation"),suffix=c("","_oth")) %>%
    inner_join(stats_1992_temp_roma_hun,by=c("Occupation"),suffix=c("","_rom_hu")) %>%
    inner_join(stats_1992_temp_roma_ro,by=c("Occupation"),suffix=c("","_rom_ro")) %>%
    inner_join(stats_1992_temp_roma_romani,by=c("Occupation"),suffix=c("_rom","_rom_romani")) %>%
    mutate(Censuses='1992 - 2011')

  stats_2002_temp<-stats_2002_temp_roma %>%
    inner_join(stats_2002_temp_hun,by=c("Occupation"),suffix=c("","_hun")) %>%
    inner_join(stats_2002_temp_ger,by=c("Occupation"),suffix=c("","_ger")) %>%
    inner_join(stats_2002_temp_oth,by=c("Occupation"),suffix=c("","_oth")) %>%
    inner_join(stats_2002_temp_roma_hun,by=c("Occupation"),suffix=c("","_rom_hu")) %>%
    inner_join(stats_2002_temp_roma_ro,by=c("Occupation"),suffix=c("","_rom_ro")) %>%
    inner_join(stats_2002_temp_roma_romani,by=c("Occupation"),suffix=c("_rom","_rom_romani")) %>%
    mutate(Censuses='2002 - 2011')



  data_2_r_temp[[i]]<-bind_rows(stats_1992_temp,stats_2002_temp)
}

# --- Aggregate Bootstrap Results: Occupation ---
# Same aggregation structure as for education; occupation axis ordered from least to most skilled
data_2_r<-bind_rows(data_2_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Occupation,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05),.names = "{col}_05")) %>%
  # Order occupation categories from least to most skilled for a coherent left-to-right progression
  mutate(Occupation=factor(Occupation,levels=c("Unemployed","Agriculture and Unskilled Labor","Skilled Labor",
                                               "Managers, Professionals, Clerks"))) %>%
  mutate(Ethnicity=case_when(Ethnicity=="p_real_rom" ~ "Roma",
                             Ethnicity=="p_real_ger" ~ "German",
                             Ethnicity=="p_real_hun" ~ "Hungarian",
                             Ethnicity=="p_real_oth" ~ "Other",
                             Ethnicity=="p_real_roma_hu" ~ "Roma - Hungarian",
                             Ethnicity=="p_real_roma_ro" ~ "Roma - Romanian",
                             Ethnicity=="p_real_roma_romani" ~ "Roma - Romani"
  )) %>%
  mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian","German","Other","Roma - Hungarian","Roma - Romanian","Roma - Romani"))) %>%
  mutate(Sample=case_when(Ethnicity=="Roma" & Censuses=="1992 - 2011" ~ "Roma '92-'11",
                          Ethnicity=="Roma" & Censuses=="2002 - 2011" ~ "Roma '02-'11",
                          Ethnicity=="Hungarian" & Censuses=="1992 - 2011" ~ "Hungarian '92-'11",
                          Ethnicity=="Hungarian" & Censuses=="2002 - 2011" ~ "Hungarian '02-'11",
                          Ethnicity=="Other" & Censuses=="1992 - 2011" ~ "Other '92-'11",
                          Ethnicity=="Other" & Censuses=="2002 - 2011" ~ "Other '02-'11"
  ))




# --- Occupation Persistence Plots ---
# g2: Roma vs Hungarian household heads by occupation, 1992-2011 panel
#occupation graphs----
g2<-ggplot(data=data_2_r %>%
             filter(Ethnicity %in% c("Roma","Hungarian")) %>%
             filter(Sample %in% c("Hungarian '92-'11","Roma '92-'11","Other '92-'11")),
           aes(x=Occupation,y=Percentage_med,color=Ethnicity,group=Ethnicity))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  scale_color_manual(values =mycolors) +
  labs(x="Occupation",y="Persistence in Declared Ethnicity")
g2

# g2_other: "Other" minority household heads by occupation, 1992-2011 panel
g2_other<-ggplot(data=data_2_r %>%
             filter(Ethnicity %in% c("Other")) %>%
             filter(Sample %in% c("Hungarian '92-'11","Roma '92-'11","Other '92-'11")),
           aes(x=Occupation,y=Percentage_med))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  # scale_color_manual(values =mycolors) +
  labs(x="Occupation",y="Persistence in Declared Ethnicity")
g2_other




# g2_2002: Roma vs Hungarian household heads by occupation, 2002-2011 panel
g2_2002<-ggplot(data=data_2_r %>%
             filter(Ethnicity %in% c("Roma","Hungarian")) %>%
             filter(Sample %in% c("Hungarian '02-'11","Roma '02-'11","Other '02-'11")),
           aes(x=Occupation,y=Percentage_med,color=Ethnicity,group=Ethnicity))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  scale_color_manual(values =mycolors) +
  labs(x="Occupation",y="Persistence in Declared Ethnicity")
g2_2002

# g2_other_2002: "Other" minority household heads by occupation, 2002-2011 panel
g2_other_2002<-ggplot(data=data_2_r %>%
                   filter(Ethnicity %in% c("Other")) %>%
                   filter(Sample %in% c("Hungarian '02-'11","Roma '02-'11","Other '02-'11")),
                 aes(x=Occupation,y=Percentage_med))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Percentage_05,
                    ymax=Percentage_95),
                width=0.15,
                size=1,
                position=position_dodge(width=0.3))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        # legend.justification = c("center"),
        plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
        legend.position=c(.15,.25),
        legend.title.align=0.5)+
  guides(size = "none")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  # scale_color_manual(values =mycolors) +
  labs(x="Occupation",y="Persistence in Declared Ethnicity")
g2_other_2002


# (Commented-out individual pdf exports for the occupation plots; superseded by combined g3 below)
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_hh_heads.pdf",width=8,height=5)
# g2
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_other_hh_heads.pdf",width=8,height=5)
# g2_other
# dev.off()
#
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_2002_hh_heads.pdf",width=8,height=5)
# g2_2002
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_other_2002_hh_heads.pdf",width=8,height=5)
# g2_other_2002
# dev.off()



# --- Final Combined Figure and Output ---
# g3: two-panel figure combining education (left) and occupation (right) persistence plots
# Legend is retained only on g1 (left panel); y-axis label suppressed on g2 to avoid redundancy
# Output: Figure A03.pdf — Appendix Figure A.3 of the paper
g3<-plot_grid(g1+theme(legend.position = c(.25,.15)), g2+theme(legend.position ="none")+labs(y=element_blank() ))
setwd(wd_output)
pdf("Figure A03.pdf",width=10,height=6)
g3
dev.off()

# (Commented-out alternative combined plots for other ethnic groups and the 2002 panel)
# g3_other<-plot_grid(g1_other+theme(legend.position = c(.25,.15)), g2_other+theme(legend.position ="none")+labs(y=element_blank() ))
# setwd(wd_output)
# pdf("02_Figure_binscatter_both_other_hh_heads.pdf",width=10,height=6)
# g3_other
# dev.off()
#
#
# g3_2002<-plot_grid(g1_2002+theme(legend.position = c(.25,.15)), g2_2002+theme(legend.position ="none")+labs(y=element_blank() ))
# setwd(wd_output)
# pdf("02_Figure_binscatter_both_2002_hh_heads.pdf",width=10,height=6)
# g3_2002
# dev.off()
#
# g3_other_2002<-plot_grid(g1_other_2002+theme(legend.position = c(.25,.15)), g2_other_2002+theme(legend.position ="none")+labs(y=element_blank() ))
# setwd(wd_output)
# pdf("02_Figure_binscatter_both_other_2002_hh_heads.pdf",width=10,height=6)
# g3_other_2002
# dev.off()


# Save underlying results for future use if necessary; saves time
# Saves the full list of bootstrap draws (before aggregation) so confidence intervals can be recomputed
# without re-running the bootstrap; output: results_educ_hh_heads.rds and results_ocup_hh_heads.rds
setwd(wd_data_results)
saveRDS(bind_rows(data_1_r_temp),"results_educ_hh_heads.rds")
saveRDS(bind_rows(data_2_r_temp),"results_ocup_hh_heads.rds")




