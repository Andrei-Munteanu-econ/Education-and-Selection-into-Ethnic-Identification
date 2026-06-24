# =====================================================================
# Figure A02 — Gender-ratio-adjusted persistence scatterplots
# Produces:  output/Figure A02.pdf
#            (also saves data_figure_a02_1.rds and data_figure_a02_2.rds
#             with the underlying bootstrap results)
# Inputs:    data_2002_2011_unique_genderless_v2.csv,
#            data_1992_2011_unique_genderless_v2.csv (linked samples),
#            per-locality sex/ethnicity proportion .rds files (built by
#            locality_gender_ratios.R), LIMBA92.DBF (1992 language codes)
# Summary:   Estimates persistence in declared ethnicity by education and
#            by occupation for Roma vs. Hungarian, correcting for the
#            chance of a spurious cross-sex link using locality-level sex
#            ratios. Confidence bands come from a town-level bootstrap
#            (iter resamples, re-seeded each iteration). Left panel =
#            education, right panel = occupation.
# =====================================================================

# ---- Build per-locality sex/ethnicity proportions (helper script) ----
#compute gender ratios for each locality
setwd(wd_code)
setwd('./AA Figure A02 gender-adjusted scatterplot/')
source("locality_gender_ratios.R")

#load data----
setwd(wd_data_linked)
data_2002_2011<-fread('data_2002_2011_unique_genderless_v2.csv')
data_1992_2011<-fread('data_1992_2011_unique_genderless_v2.csv')

#load stats per locality
data_1992_v2<-readRDS("data_1992_sex_ethnicity_proportion_per_cell.rds")
data_2002_v2<-readRDS("data_2002_sex_ethnicity_proportion_per_cell.rds")
data_2011_92<-readRDS("data_2011_sex_ethnicity_proportion_per_cell_1992.rds") %>%
  mutate(Education = fct_recode(Education, "Postsec." = "Post- Secondary"))
data_2011_02<-readRDS("data_2011_sex_ethnicity_proportion_per_cell_2002.rds")  %>%
  mutate(Education = fct_recode(Education, "Postsec." = "Post- Secondary"))



setwd(wd_data_92_other)
eth <- read.dbf("LIMBA92.DBF", as.is = F)
iter<-200
mycolors<-c("Roma"="red","Hungarian"="gray60")

#clean data----
data92<-data_1992_2011 %>%
  mutate(Education=case_when(years_2011==0 ~ "None",
                             years_2011 %in% 2:4 ~ "Primary School",
                             years_2011 %in% 8:10  ~ "Middle School",
                             years_2011 %in% 12:13 ~ "High School or Vocational",
                             years_2011 %in% 14:16 ~ "Postsec.")) %>%
  mutate(OCUP_agg_2011=floor(OCUP_2011/1000)) %>%
  mutate(Occupation=case_when(OCUP_agg_2011 %in% c(0) ~ "Unemployed",
                              OCUP_agg_2011 %in% c(1:2,4) ~ "Managers, Professionals, Clerks",
                              OCUP_agg_2011 %in% c(3,5,7:8) ~ "Skilled Labor",
                              OCUP_agg_2011 %in% c(6,9) ~ "Agriculture and Unskilled Labor")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Postsec."))) %>% 
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  mutate(HUN_1992=ifelse(ET_1992==11,T,F),
         HUN_2011=ifelse(ET_2011 %in% c(1101,1102,1103),T,F)) %>%
  mutate(GER_1992=ifelse(ET_1992==13,T,F),
         GER_2011=ifelse(ET_2011 %in% c(1300:1399),T,F)) %>%
  mutate(OTHER_1992=ifelse(ET_1992%in% 13:90,T,F),
         OTHER_2011=ifelse(ET_2011 %in% c(1300:9000),T,F))%>%
  rename_all(~gsub("_1992","_baseline",.)) %>%
  #add town-level cell stats
  mutate(town_cell=sub("^([^-]+)-.*", "\\1", cell_id_genderless_baseline)) %>%
  left_join(data_1992_v2,by=c("town_cell")) %>%
  left_join(data_2011_92,by=c("town_cell","Education"),suffix=c("_baseline","_endline"))


  




data02<-data_2002_2011 %>%
  mutate(Education=case_when(years_2011==0 ~ "None",
                             years_2011 %in% 2:4 ~ "Primary School",
                             years_2011 %in% 8:10  ~ "Middle School",
                             years_2011 %in% 12:13 ~ "High School or Vocational",
                             years_2011 %in% 14:16 ~ "Postsec.")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Postsec."))) %>% 
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
  rename_all(~gsub("_2002","_baseline",.)) %>%
  #add town-level cell stats
  mutate(town_cell=sub("^([^-]+)-.*", "\\1", cell_id_genderless_baseline)) %>%
  left_join(data_2002_v2,by=c("town_cell")) %>%
  left_join(data_2011_02,by=c("town_cell","Education"),suffix=c("_baseline","_endline"))

#education----
towns02<-unique(data02$SIRSUP_baseline) 
towns92<-unique(data92$SIRSUP_baseline)

towns02_hu<-unique(unlist(data02 %>% filter(ET_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_hu<-unique(unlist(data92 %>% filter(ET_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_ge<-unique(unlist(data02 %>% filter(ET_baseline==13) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_ge<-unique(unlist(data92 %>% filter(ET_baseline==13) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_oth<-unique(unlist(data02 %>% filter(ET_baseline %in% 14:90) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_oth<-unique(unlist(data92 %>% filter(ET_baseline %in% 14:90) %>% select(SIRSUP_baseline), use.names = FALSE))


towns02_rom<-unique(unlist(data02 %>% filter(ET_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom<-unique(unlist(data92 %>% filter(ET_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_rom_hu<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_hu<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_rom_romani<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_romani<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))


get_stats<-function(data){
  result<-data %>%
    right_join(town_sample,by="SIRSUP_baseline",relationship="many-to-many") %>%
    select(Education,town_cell,p_ROMA_baseline,p_female_ROMA_baseline,p_female_ROMA_endline,SIRSUP_baseline,mismatch,ETH_baseline,ETH_2011,
           SEX_baseline,SEX_2011) %>%
    group_by(Education,town_cell) %>%
    mutate(p_eth_if_mismatch=mean(p_ROMA_baseline,na.rm=T)) %>% #prob of matching to a Roma at baseline if mismatched
    filter(ETH_baseline==T) %>% #keep only Roma at baseline
    summarise(p_data=mean(ETH_2011==T,na.rm=T), #get passing rates from the data
              p_female_ROMA_baseline=mean(p_female_ROMA_baseline,na.rm=T),
              p_female_ROMA_endline=mean(p_female_ROMA_endline,na.rm=T),
              p_same_sex_chance=p_female_ROMA_baseline*p_female_ROMA_endline+(1-p_female_ROMA_baseline)*(1-p_female_ROMA_endline),
              p_different_sex=sum(mismatch)/n(),
              p_eth_if_mismatch=mean(p_eth_if_mismatch),
              n=n(),
              # p_eth_if_mismatch=mean(p_ROMA_baseline,na.rm=T),
              .groups = 'drop') %>%
    mutate(p_mismatch_old=p_different_sex*2,
           mu_g=p_different_sex/p_same_sex_chance) %>%
    mutate(p_real=(p_data-mu_g*p_eth_if_mismatch)/(1-mu_g),
           p_old=(p_data-p_different_sex*2*p_eth_if_mismatch)/(1-p_different_sex*2),
           p_fake=(p_data)/(1-p_different_sex*4))
  
  result<-result %>% 
    group_by(Education) %>%
    summarise(p_real=weighted.mean(p_real[!is.infinite(p_real) & p_real<=1 & p_real>=0],n[!is.infinite(p_real) & p_real<=1 & p_real>=0],na.rm=T),
              p_fake=weighted.mean(p_fake[!is.infinite(p_fake) & p_fake<=1 & p_fake>=0],n[!is.infinite(p_fake) & p_fake<=1 & p_fake>=0],na.rm=T),
              p_old=weighted.mean(p_old[!is.infinite(p_old) & p_old<=1 & p_old>=0],n[!is.infinite(p_old) & p_old<=1 & p_old>=0],na.rm=T)
    ) %>%
    select(Education,p_real)

  return(result)
}




# ggplot(result, aes(x = p_different_sex)) +
#   geom_histogram(
#     aes(y = after_stat(count / sum(count))),
#     bins = 100
#   ) +
#   labs(
#     x = "p_different_sex",
#     y = "Relative frequency"
#   ) +
#   theme_minimal()
# 
# ggplot(result, aes(x = p_same_sex_chance)) +
#   geom_histogram(
#     aes(y = after_stat(count / sum(count))),
#     bins = 100
#   ) +
#   labs(
#     x = "p_same_sex_chance",
#     y = "Relative frequency"
#   ) +
#   theme_minimal()
# 
# 
# ggplot(result, aes(x = mu_g)) +
#   geom_histogram(
#     aes(y = after_stat(count / sum(count))),
#     bins = 100
#   ) +
#   labs(
#     x = "mu_g",
#     y = "Relative frequency"
#   ) +
#   theme_minimal()



get_town<-function(towns){
  town_sample<- data.frame(SIRSUP_baseline=
                               sample(x = towns,
                                      size = length(towns),
                                      replace = TRUE))
  
 
  
  return(town_sample)
}

data_1_r_temp<-list()
for(i in 1:iter){
  set.seed(i)
  print(i)
  
  #roma
  town_sample<-get_town(towns02)
  stats_2002_temp_roma<-get_stats(data02 %>%
    mutate(ETH_baseline=ROMA_baseline,
           ETH_2011=ROMA_2011)
    )
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
  town_sample<-get_town(towns02_rom_hu)
  stats_2002_temp_roma_hun<-get_stats(data02 %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_hu)
  stats_1992_temp_roma_hun<-get_stats(data92 %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-ro
  town_sample<-get_town(towns02_rom)
  stats_2002_temp_roma_ro<-get_stats(data02 %>%
                                            filter(LIM_baseline==10) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom)
  stats_1992_temp_roma_ro<-get_stats(data92 %>%
                                            filter(LIM_baseline==10) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011)) 
  
  #roma-romani
  town_sample<-get_town(towns02_rom_romani)
  stats_2002_temp_roma_romani<-get_stats(data02 %>%
                                                filter(LIM_baseline==12) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_romani)
  stats_1992_temp_roma_romani<-get_stats(data92 %>%
                                                filter(LIM_baseline==12) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  
  stats_1992_temp<-stats_1992_temp_roma %>%
    inner_join(stats_1992_temp_hun,by=c("Education"),suffix=c("","_hun")) %>%
    inner_join(stats_1992_temp_ger,by=c("Education"),suffix=c("","_ger")) %>%
    inner_join(stats_1992_temp_oth,by=c("Education"),suffix=c("","_oth")) %>%
    inner_join(stats_1992_temp_roma_hun,by=c("Education"),suffix=c("","_rom_hu")) %>%
    inner_join(stats_1992_temp_roma_ro,by=c("Education"),suffix=c("","_rom_ro")) %>%
    inner_join(stats_1992_temp_roma_romani,by=c("Education"),suffix=c("_rom","_rom_romani")) %>%
    mutate(Censuses='1992 - 2011')
  
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

data_1_r<-bind_rows(data_1_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Education,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05),.names = "{col}_05")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Postsec."))) %>%
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



setwd(wd_data_results)
saveRDS(data_1_r,"data_figure_a02_1.rds")




#education graphs----
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

data_2_r<-bind_rows(data_2_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Occupation,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05),.names = "{col}_05")) %>%
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



setwd(wd_data_results)
saveRDS(data_2_r,"data_figure_a02_2.rds")


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


g3<-plot_grid(g1+theme(legend.position = c(.25,.15)), g2+theme(legend.position ="none")+labs(y=element_blank() ))
setwd(wd_output)
pdf("Figure A02.pdf",width=10,height=6)
g3
dev.off()





