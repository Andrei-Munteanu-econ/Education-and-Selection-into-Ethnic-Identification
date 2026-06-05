#This script generates Figures 1 and Figures A.4, A.5 and A.6 of the Appendix

# --- Data Loading ---
#load data----
setwd(wd_data_linked)
# data_2002_2011: linked records spanning the 2002 and 2011 censuses (genderless cell_id matching)
data_2002_2011<-fread('data_2002_2011_unique_genderless.csv')
# data_1992_2011: linked records spanning the 1992 and 2011 censuses (genderless cell_id matching)
data_1992_2011<-fread('data_1992_2011_unique_genderless.csv')



setwd(wd_data_92_other)
# eth: 1992 mother-tongue / language file from the DBF format census extract; used for LIM_baseline merges
eth <- read.dbf("LIMBA92.DBF", as.is = F)
# iter: number of bootstrap replications for town-level resampling
iter<-200
# mycolors: consistent color mapping for Roma (red) and Hungarian (gray) series across all figures
mycolors<-c("Roma"="red","Hungarian"="gray60")

# --- Data Cleaning: 1992-2011 Panel ---
#clean data----
# data92: cleaned version of the 1992-2011 linked file; education, occupation, mismatch, and ethnicity flags
# are derived here so that get_stats() can operate on a standardised data frame
data92<-data_1992_2011 %>%
  # Bin continuous years_2011 into the five education categories used in the figure
  # The gaps in years (e.g., 5-7 omitted) reflect non-standard schooling durations in the Romanian system
  mutate(Education=case_when(years_2011==0 ~ "None",
                             years_2011 %in% 2:4 ~ "Primary School",
                             years_2011 %in% 8:10  ~ "Middle School",
                             years_2011 %in% 12:13 ~ "High School or Vocational",
                             years_2011 %in% 14:16 ~ "Post- Secondary")) %>%
  # Collapse 4-digit ISCO-style occupation code to 1-digit major group for the occupation analysis
  mutate(OCUP_agg_2011=floor(OCUP_2011/1000)) %>%
  mutate(Occupation=case_when(OCUP_agg_2011 %in% c(0) ~ "Unemployed",
                              OCUP_agg_2011 %in% c(1:2,4) ~ "Managers, Professionals, Clerks",
                              OCUP_agg_2011 %in% c(3,5,7:8) ~ "Skilled Labor",
                              OCUP_agg_2011 %in% c(6,9) ~ "Agriculture and Unskilled Labor")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) %>%
  # mismatch: sex differs between the baseline and 2011 records, flagging likely false census matches
  # used in get_stats() to apply the mismatch-correction formula
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  # HUN_1992 / HUN_2011: Hungarian-identity flag in each census year (ET code 11 in 1992; range 1101-1103 in 2011)
  mutate(HUN_1992=ifelse(ET_1992==11,T,F),
         HUN_2011=ifelse(ET_2011 %in% c(1101,1102,1103),T,F)) %>%
  # GER_1992 / GER_2011: German-identity flag (ET code 13 in 1992; range 1300-1399 in 2011)
  mutate(GER_1992=ifelse(ET_1992==13,T,F),
         GER_2011=ifelse(ET_2011 %in% c(1300:1399),T,F)) %>%
  # OTHER_1992 / OTHER_2011: all non-Romanian, non-Roma minority groups (ET codes 13-90 and equivalents)
  mutate(OTHER_1992=ifelse(ET_1992%in% 13:90,T,F),
         OTHER_2011=ifelse(ET_2011 %in% c(1300:9000),T,F))%>%
  # Standardise column names: replace "_1992" suffix with "_baseline" so get_stats() works on both panels
  rename_all(~gsub("_1992","_baseline",.))







# --- Data Cleaning: 2002-2011 Panel ---
# data02: cleaned version of the 2002-2011 linked file; same binning and flag logic as data92
# "_2002" suffixes are renamed to "_baseline" for compatibility with get_stats()
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
  # mismatch: sex differs between 2002 and 2011 records for this panel
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  mutate(HUN_2002=ifelse(ET_2002==11,T,F),
         HUN_2011=ifelse(ET_2011 %in% c(1101,1102,1103),T,F)) %>%
  mutate(GER_2002=ifelse(ET_2002==13,T,F),
         GER_2011=ifelse(ET_2011 %in% c(1300:1399),T,F)) %>%
  mutate(OTHER_2002=ifelse(ET_2002 %in% 13:90,T,F),
         OTHER_2011=ifelse(ET_2011 %in% c(1300:9000),T,F)) %>%
  rename_all(~gsub("_2002","_baseline",.))

# --- Town Sampling Frames for Bootstrap ---
#education----
# towns02 / towns92: full set of unique town IDs in each panel, used for with-replacement resampling
towns02<-unique(data02$SIRSUP_baseline)
towns92<-unique(data92$SIRSUP_baseline)

# Subset town vectors by baseline ethnicity so bootstrap resampling is done within the relevant
# geographic support for each ethnic group (avoids resampling towns with no members of that group)
towns02_hu<-unique(unlist(data02 %>% filter(ET_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_hu<-unique(unlist(data92 %>% filter(ET_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_ge<-unique(unlist(data02 %>% filter(ET_baseline==13) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_ge<-unique(unlist(data92 %>% filter(ET_baseline==13) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_oth<-unique(unlist(data02 %>% filter(ET_baseline %in% 14:90) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_oth<-unique(unlist(data92 %>% filter(ET_baseline %in% 14:90) %>% select(SIRSUP_baseline), use.names = FALSE))


towns02_rom<-unique(unlist(data02 %>% filter(ET_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom<-unique(unlist(data92 %>% filter(ET_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))

# Roma with Hungarian mother tongue (LIM_baseline==11): a sub-group used in Figures A.05/A.06
towns02_rom_hu<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_hu<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))

# Roma with Romani mother tongue (LIM_baseline==12)
towns02_rom_romani<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_romani<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))


# --- Helper Functions ---

# get_stats: for one bootstrap draw (town_sample already set in the calling loop),
# computes the mismatch-corrected ethnic persistence rate by Education bin.
# Mismatch correction formula: p_real = (p_all - p_mismatch * p_eth_mismatch) / (1 - p_mismatch)
#   p_all          = unconditional share still identifying as ETH in 2011
#   p_mismatch     = estimated share of matched records that are false matches (2 * fraction with sex mismatch)
#   p_eth_mismatch = share of false matches where ETH_2011 == TRUE (driven by the ethnic composition of the town)
# The factor of 2 in p_mismatch = n/sum(n)*2 assumes sex mismatches are symmetric and represent
# half of all false matches (each false match generates one wrong-sex observation).
get_stats<-function(data){
  result<-data %>%
    filter(ETH_baseline==T) %>%
    right_join(town_sample,by="SIRSUP_baseline",relationship="many-to-many") %>%
    group_by(Education) %>%
    mutate(p_all=mean(ETH_2011==T,na.rm=T)) %>%
    group_by(mismatch,Education,p_all) %>%
    summarise(n=n(),p_eth_mismatch=mean(ETH_2011==T,na.rm=T), .groups = 'drop') %>%
    group_by(Education) %>%
    mutate(p_mismatch=n/sum(n)*2,n=sum(n)) %>%
    filter(mismatch==T) %>%
    mutate(p_real=(p_all-p_mismatch*p_eth_mismatch)/(1-p_mismatch)) %>%
    ungroup %>%
    select(Education,p_real)

  return(result)
}

# get_town: draws a with-replacement bootstrap sample of towns (one row per town draw)
# The resulting data frame is used by right_join inside get_stats() so that towns drawn
# multiple times contribute proportionally more observations to the cell means.
get_town<-function(towns){
  town_sample<- data.frame(SIRSUP_baseline=
                               sample(x = towns,
                                      size = length(towns),
                                      replace = TRUE))



  return(town_sample)
}


# --- Bootstrap Loop: Education Analysis ---
# data_1_r_temp: list collecting one data frame per bootstrap iteration (200 total)
data_1_r_temp<-list()
for(i in 1:iter){
  # set.seed(i) inside the loop: each iteration is reproducible individually even if the loop
  # is re-run partially; different seeds across iterations ensure independent draws
  set.seed(i)

  #roma
  town_sample<-get_town(towns02)
  stats_2002_temp_roma<-get_stats(data02 %>%
    mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92)
  stats_1992_temp_roma<-get_stats(data92 %>%
    mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #hu
  town_sample<-get_town(towns02_hu)
  stats_2002_temp_hun<-get_stats(data02 %>%
    mutate(ETH_baseline=HUN_baseline,ETH_2011=HUN_2011))
  town_sample<-get_town(towns92_hu)
  stats_1992_temp_hun<-get_stats(data92 %>%
    mutate(ETH_baseline=HUN_baseline,ETH_2011=HUN_2011))

  #ge
  town_sample<-get_town(towns02_ge)
  stats_2002_temp_ger<-get_stats(data02 %>%
    mutate(ETH_baseline=GER_baseline,ETH_2011=GER_2011))
  town_sample<-get_town(towns92_ge)
  stats_1992_temp_ger<-get_stats(data92 %>%
    mutate(ETH_baseline=GER_baseline,ETH_2011=GER_2011))

  #other
  town_sample<-get_town(towns02_oth)
  stats_2002_temp_oth<-get_stats(data02 %>%
    mutate(ETH_baseline=OTHER_baseline,ETH_2011=OTHER_2011))
  town_sample<-get_town(towns92_oth)
  stats_1992_temp_oth<-get_stats(data92 %>%
    mutate(ETH_baseline=OTHER_baseline,ETH_2011=OTHER_2011))

  #roma-hungarian
  # Roma subgroup that declared Hungarian as mother tongue at baseline; isolated to examine
  # whether language-group composition drives the education-passing gradient for Roma
  town_sample<-get_town(towns02_rom_hu)
  stats_2002_temp_roma_hun<-get_stats(data02 %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_hu)
  stats_1992_temp_roma_hun<-get_stats(data92 %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-ro
  # Roma subgroup with Romanian mother tongue (LIM_baseline==10); the majority Roma language group
  town_sample<-get_town(towns02_rom)
  stats_2002_temp_roma_ro<-get_stats(data02 %>%
                                            filter(LIM_baseline==10) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom)
  stats_1992_temp_roma_ro<-get_stats(data92 %>%
                                            filter(LIM_baseline==10) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-romani
  # Roma subgroup with Romani mother tongue (LIM_baseline==12); most culturally distinct group,
  # expected to have lowest passing rates regardless of education
  town_sample<-get_town(towns02_rom_romani)
  stats_2002_temp_roma_romani<-get_stats(data02 %>%
                                                filter(LIM_baseline==12) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_romani)
  stats_1992_temp_roma_romani<-get_stats(data92 %>%
                                                filter(LIM_baseline==12) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  # Combine all ethnic groups for the 1992-2011 panel into a wide row keyed on Education
  stats_1992_temp<-stats_1992_temp_roma %>%
    inner_join(stats_1992_temp_hun,by=c("Education"),suffix=c("","_hun")) %>%
    inner_join(stats_1992_temp_ger,by=c("Education"),suffix=c("","_ger")) %>%
    inner_join(stats_1992_temp_oth,by=c("Education"),suffix=c("","_oth")) %>%
    inner_join(stats_1992_temp_roma_hun,by=c("Education"),suffix=c("","_rom_hu")) %>%
    inner_join(stats_1992_temp_roma_ro,by=c("Education"),suffix=c("","_rom_ro")) %>%
    inner_join(stats_1992_temp_roma_romani,by=c("Education"),suffix=c("_rom","_rom_romani")) %>%
    mutate(Censuses='1992 - 2011')

  # Combine all ethnic groups for the 2002-2011 panel
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

#load underlying results to save time if previously saved
# setwd(wd_data_results)
# data_1_r_temp<-readRDS("results_educ.rds")

# --- Summarise Bootstrap Draws: Education ---
# Pivot from wide (one column per ethnic group) to long, then collapse to median and
# 90% bootstrap confidence interval (5th-95th percentile) for each Education x Ethnicity x Censuses cell
data_1_r<-bind_rows(data_1_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Education,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05),.names = "{col}_05")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) %>%
  # Map internal column-name suffixes back to human-readable ethnicity labels for the legend
  mutate(Ethnicity=case_when(Ethnicity=="p_real_rom" ~ "Roma",
                             Ethnicity=="p_real_ger" ~ "German",
                             Ethnicity=="p_real_hun" ~ "Hungarian",
                             Ethnicity=="p_real_oth" ~ "Other",
                             Ethnicity=="p_real_roma_hu" ~ "Roma - Hungarian",
                             Ethnicity=="p_real_roma_ro" ~ "Roma - Romanian",
                             Ethnicity=="p_real_roma_romani" ~ "Roma - Romani"
                             )) %>%
  mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian","German","Other","Roma - Hungarian","Roma - Romanian","Roma - Romani"))) %>%
  # Sample: convenience label combining Ethnicity and Censuses for filtering in each figure panel
  mutate(Sample=case_when(Ethnicity=="Roma" & Censuses=="1992 - 2011" ~ "Roma '92-'11",
                          Ethnicity=="Roma" & Censuses=="2002 - 2011" ~ "Roma '02-'11",
                          Ethnicity=="Hungarian" & Censuses=="1992 - 2011" ~ "Hungarian '92-'11",
                          Ethnicity=="Hungarian" & Censuses=="2002 - 2011" ~ "Hungarian '02-'11",
                          Ethnicity=="Other" & Censuses=="1992 - 2011" ~ "Other '92-'11",
                          Ethnicity=="Other" & Censuses=="2002 - 2011" ~ "Other '02-'11"
                          ))




# --- Education Figures ---
#education graphs----
# g1: Figure 1 left panel — mismatch-corrected ethnic persistence by education, Roma vs Hungarian,
# 1992-2011 panel; error bars are bootstrap 90% CI; legend inside at bottom-left
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

# g1_other: education x persistence for the residual "Other" minority group, 1992-2011 panel;
# used in Figure A.04 as a placebo check (no strong passing incentive expected)
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

# g1_2002: education x persistence for Roma vs Hungarian, 2002-2011 panel (Figure A.05 left panel);
# the shorter inter-census window tests robustness of the education gradient
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

# g1_other_2002: "Other" minority group, 2002-2011 panel; right panel of Figure A.06
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

# setwd(wd_output)
# pdf("02_Figure_binscatter_educ.pdf",width=8,height=5)
# g1
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_2002.pdf",width=8,height=5)
# g1_2002
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_other.pdf",width=8,height=5)
# g1_other
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_other_2002.pdf",width=8,height=5)
# g1_other_2002
# dev.off()


# --- Bootstrap Loop: Occupation Analysis ---
# get_stats_ocup: identical mismatch-correction logic as get_stats() but stratified by Occupation
# rather than Education; used to show the passing-persistence gradient along socioeconomic status
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

# data_2_r_temp: list collecting occupation bootstrap draws (same 200-iteration structure as data_1_r_temp)
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



# #load underlying results to save time if previously saved
# setwd(wd_data_results)
# data_2_r_temp<-readRDS("results_ocup.rds")

# --- Summarise Bootstrap Draws: Occupation ---
# Same median / 90% CI collapse as for education, now keyed on Occupation x Ethnicity x Censuses
data_2_r<-bind_rows(data_2_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Occupation,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05),.names = "{col}_05")) %>%
  # Order occupation from lowest to highest socioeconomic status so the x-axis reads left to right
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




# --- Occupation Figures ---

#occupation graphs----
# g2: Figure 1 right panel — mismatch-corrected ethnic persistence by occupation, Roma vs Hungarian,
# 1992-2011 panel; paired with g1 to show both education and occupation gradients side-by-side
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

# g2_other: occupation x persistence for "Other" minorities, 1992-2011 panel; right panel of Figure A.04
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




# g2_2002: occupation x persistence, Roma vs Hungarian, 2002-2011 panel; right panel of Figure A.05
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

# g2_other_2002: occupation x persistence, "Other" minorities, 2002-2011 panel; right panel of Figure A.06
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




# --- Save Output Figures ---

#save Figure 1
# g3: combines education panel (g1, with legend) and occupation panel (g2, legend suppressed, y-label removed)
# Output: Figure 01.pdf
g3<-plot_grid(g1+theme(legend.position = c(.25,.15)), g2+theme(legend.position ="none")+labs(y=element_blank() ))
setwd(wd_output)
pdf("Figure 01.pdf",width=10,height=6)
g3
dev.off()

#save Figure A.04 of the Appendix
# g3_other: placebo figure for residual "Other" minorities, 1992-2011 panel; education + occupation
# Output: Figure A04.pdf
g3_other<-plot_grid(g1_other+theme(legend.position = c(.25,.15)), g2_other+theme(legend.position ="none")+labs(y=element_blank() ))
setwd(wd_output)
pdf("Figure A04.pdf",width=10,height=6)
g3_other
dev.off()

#Save Figure A.05 of the Appendix
# g3_2002: robustness check using 2002-2011 panel for Roma and Hungarians; education + occupation
# Output: Figure A05.pdf
g3_2002<-plot_grid(g1_2002+theme(legend.position = c(.25,.15)), g2_2002+theme(legend.position ="none")+labs(y=element_blank() ))
setwd(wd_output)
pdf("Figure A05.pdf",width=10,height=6)
g3_2002
dev.off()

#Save Figure A.06 of the Appendix
# g3_other_2002: placebo for "Other" minorities, 2002-2011 panel; education + occupation
# Output: Figure A06.pdf
g3_other_2002<-plot_grid(g1_other_2002+theme(legend.position = c(.25,.15)), g2_other_2002+theme(legend.position ="none")+labs(y=element_blank() ))
setwd(wd_output)
pdf("Figure A06.pdf",width=10,height=6)
g3_other_2002
dev.off()

# Save underlying results for future use if necessary; saves time
# setwd(wd_data_results)
# saveRDS(bind_rows(data_1_r_temp),"results_educ.rds")
# saveRDS(bind_rows(data_2_r_temp),"results_ocup.rds")




