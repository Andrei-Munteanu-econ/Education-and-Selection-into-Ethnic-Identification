#This script generates Figure A.2 of the Appendix

# --- Setup: gender ratio helper and data loading ---

#compute gender ratios for each locality
# locality_gender_ratios.R builds town-level female share among Roma and non-Roma,
# which is needed to compute gender-adjusted mismatch rates (mu_g) below
setwd(wd_code)
setwd('./AA Figure A2/')
source("locality_gender_ratios.R")

#load data----
setwd(wd_data_linked)
# data_2002_2011: genderless-cell linked records spanning the 2002-2011 intercensus period
data_2002_2011<-fread('data_2002_2011_unique_genderless_v2.csv')
# data_1992_2011: genderless-cell linked records spanning the 1992-2011 intercensus period
data_1992_2011<-fread('data_1992_2011_unique_genderless_v2.csv')

#load stats per locality
# These .rds files contain the town-by-education-cell female shares used to compute
# gender-match probabilities when correcting for census linkage mismatch
data_1992_v2<-readRDS("data_1992_sex_ethnicity_proportion_per_cell.rds")
data_2002_v2<-readRDS("data_2002_sex_ethnicity_proportion_per_cell.rds")
# _1992 / _2002 suffix: the file refers to the baseline census used for the endline town cells
data_2011_92<-readRDS("data_2011_sex_ethnicity_proportion_per_cell_1992.rds")
data_2011_02<-readRDS("data_2011_sex_ethnicity_proportion_per_cell_2002.rds")

setwd(wd_data_92_other)
# eth: 1992 mother-tongue (LIM) database; loaded to support language-based Roma subgroup splits below
eth <- read.dbf("LIMBA92.DBF", as.is = F)
# iter: number of bootstrap replications; town-level resampling to capture within-locality correlation
iter<-200
# mycolors: consistent Roma/Hungarian color scheme used across Figure 1 and the appendix figures
mycolors<-c("Roma"="red","Hungarian"="gray60")

# --- Data cleaning: 1992-2011 linked sample ---
#clean data----
data92<-data_1992_2011 %>%
  # Bin continuous years_2011 into the five coarse education categories shown in the figure
  mutate(Education=case_when(years_2011==0 ~ "None",
                             years_2011 %in% 2:4 ~ "Primary School",
                             years_2011 %in% 8:10  ~ "Middle School",
                             years_2011 %in% 12:13 ~ "High School or Vocational",
                             years_2011 %in% 14:16 ~ "Post- Secondary")) %>%
  # Collapse 4-digit OCUP_2011 codes to 1-digit major groups for occupation panels
  mutate(OCUP_agg_2011=floor(OCUP_2011/1000)) %>%
  mutate(Occupation=case_when(OCUP_agg_2011 %in% c(0) ~ "Unemployed",
                              OCUP_agg_2011 %in% c(1:2,4) ~ "Managers, Professionals, Clerks",
                              OCUP_agg_2011 %in% c(3,5,7:8) ~ "Skilled Labor",
                              OCUP_agg_2011 %in% c(6,9) ~ "Agriculture and Unskilled Labor")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) %>%
  # mismatch: TRUE when the linked record pair reports different sexes; flags likely false matches
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  # Create ethnicity flags for each comparison group at baseline and endline
  # ET codes: 11 = Hungarian, 12 = Roma, 13 = German; 2011 codes use expanded 4-digit scheme
  mutate(HUN_1992=ifelse(ET_1992==11,T,F),
         HUN_2011=ifelse(ET_2011 %in% c(1101,1102,1103),T,F)) %>%
  mutate(GER_1992=ifelse(ET_1992==13,T,F),
         GER_2011=ifelse(ET_2011 %in% c(1300:1399),T,F)) %>%
  mutate(OTHER_1992=ifelse(ET_1992%in% 13:90,T,F),
         OTHER_2011=ifelse(ET_2011 %in% c(1300:9000),T,F))%>%
  # Rename baseline-year suffixes to _baseline so get_stats() can work identically on both panels
  rename_all(~gsub("_1992","_baseline",.)) %>%
  #add town-level cell stats
  # town_cell: locality portion of cell_id_genderless; used as the merge key with town-level stats
  mutate(town_cell=sub("^([^-]+)-.*", "\\1", cell_id_genderless_baseline)) %>%
  # Attach baseline female share (p_female_ROMA_baseline) for gender-adjusted mismatch correction
  left_join(data_1992_v2,by=c("town_cell")) %>%
  # Attach endline female share (p_female_ROMA_endline) by town-cell and education group
  left_join(data_2011_92,by=c("town_cell","Education"),suffix=c("_baseline","_endline"))







# --- Data cleaning: 2002-2011 linked sample (mirrors data92 logic above) ---
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
  rename_all(~gsub("_2002","_baseline",.)) %>%
  #add town-level cell stats
  mutate(town_cell=sub("^([^-]+)-.*", "\\1", cell_id_genderless_baseline)) %>%
  left_join(data_2002_v2,by=c("town_cell")) %>%
  left_join(data_2011_02,by=c("town_cell","Education"),suffix=c("_baseline","_endline"))

# --- Town-list construction: unique localities per ethnic group and census pair ---
#education----
# towns02 / towns92: full locality universe for the 2002-2011 and 1992-2011 panels respectively;
# used as the resampling frame in the bootstrap
towns02<-unique(data02$SIRSUP_baseline)
towns92<-unique(data92$SIRSUP_baseline)

# Locality lists restricted to towns that contain at least one Hungarian-identified individual;
# used so bootstrap resamples are drawn from the correct ethnic group's town distribution
towns02_hu<-unique(unlist(data02 %>% filter(ET_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_hu<-unique(unlist(data92 %>% filter(ET_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_ge<-unique(unlist(data02 %>% filter(ET_baseline==13) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_ge<-unique(unlist(data92 %>% filter(ET_baseline==13) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_oth<-unique(unlist(data02 %>% filter(ET_baseline %in% 14:90) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_oth<-unique(unlist(data92 %>% filter(ET_baseline %in% 14:90) %>% select(SIRSUP_baseline), use.names = FALSE))


towns02_rom<-unique(unlist(data02 %>% filter(ET_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom<-unique(unlist(data92 %>% filter(ET_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))

# Roma subgroups by baseline mother tongue (LIM): 11 = Hungarian, 12 = Romani; used to examine
# whether the education-passing gradient differs by linguistic assimilation
towns02_rom_hu<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_hu<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==11) %>% select(SIRSUP_baseline), use.names = FALSE))

towns02_rom_romani<-unique(unlist(data02 %>% filter(ET_baseline==12 & LIM_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))
towns92_rom_romani<-unique(unlist(data92 %>% filter(ET_baseline==12 & LIM_baseline==12) %>% select(SIRSUP_baseline), use.names = FALSE))


# --- get_stats(): gender-adjusted mismatch correction by education cell ---
# Computes the gender-corrected ethnic persistence rate (p_real) for each education bin,
# using the formula: p_real = (p_data - mu_g * p_eth_if_mismatch) / (1 - mu_g)
# where mu_g is the gender-adjusted mismatch probability (see Mismatch Correction note).
# The function operates on a bootstrap town_sample drawn by get_town() before each call.
get_stats<-function(data){
  result<-data %>%
    right_join(town_sample,by="SIRSUP_baseline",relationship="many-to-many") %>%
    select(Education,town_cell,p_ROMA_baseline,p_female_ROMA_baseline,p_female_ROMA_endline,SIRSUP_baseline,mismatch,ETH_baseline,ETH_2011,
           SEX_baseline,SEX_2011) %>%
    group_by(Education,town_cell) %>%
    # p_eth_if_mismatch: within (Education x town) cell, the fraction of baseline records that are Roma;
    # approximates P(baseline is Roma | false match), needed to evaluate how many false links
    # artificially inflate or deflate observed passing rates
    mutate(p_eth_if_mismatch=mean(p_ROMA_baseline,na.rm=T)) %>% #prob of matching to a Roma at baseline if mismatched
    filter(ETH_baseline==T) %>% #keep only Roma at baseline
    summarise(p_data=mean(ETH_2011==T,na.rm=T), #get passing rates from the data
              p_female_ROMA_baseline=mean(p_female_ROMA_baseline,na.rm=T),
              p_female_ROMA_endline=mean(p_female_ROMA_endline,na.rm=T),
              # p_same_sex_chance: probability that two randomly drawn Roma individuals (one from
              # baseline, one from endline) share the same sex; used to convert observed sex-mismatch
              # rate into an estimated false-match rate mu_g
              p_same_sex_chance=p_female_ROMA_baseline*p_female_ROMA_endline+(1-p_female_ROMA_baseline)*(1-p_female_ROMA_endline),
              # p_different_sex: share of matched pairs with opposite sex; raw signal of false matches
              p_different_sex=sum(mismatch)/n(),
              p_eth_if_mismatch=mean(p_eth_if_mismatch),
              n=n(),
              # p_eth_if_mismatch=mean(p_ROMA_baseline,na.rm=T),
              .groups = 'drop') %>%
    # p_mismatch_old: simple (non-gender-adjusted) mismatch rate estimator; kept for comparison
    mutate(p_mismatch_old=p_different_sex*2,
           # mu_g: gender-adjusted false-match rate; divides raw sex-mismatch rate by the
           # expected sex-mismatch probability under random pairing, giving P(false match)
           mu_g=p_different_sex/p_same_sex_chance) %>%
    # p_real: gender-adjusted corrected persistence rate (main estimand for this figure)
    # p_old: simpler correction that ignores gender composition (used as robustness check)
    # p_fake: alternative correction formula retained for diagnostic purposes
    mutate(p_real=(p_data-mu_g*p_eth_if_mismatch)/(1-mu_g),
           p_old=(p_data-p_different_sex*2*p_eth_if_mismatch)/(1-p_different_sex*2),
           p_fake=(p_data)/(1-p_different_sex*4))

  # Aggregate from town-education cells to education bins via weighted mean,
  # dropping cells where p_real is out of [0,1] (numerical artefacts from small cells)
  result<-result %>%
    group_by(Education) %>%
    summarise(p_real=weighted.mean(p_real[!is.infinite(p_real) & p_real<=1 & p_real>=0],n[!is.infinite(p_real) & p_real<=1 & p_real>=0],na.rm=T),
              p_fake=weighted.mean(p_fake[!is.infinite(p_fake) & p_fake<=1 & p_fake>=0],n[!is.infinite(p_fake) & p_fake<=1 & p_fake>=0],na.rm=T),
              p_old=weighted.mean(p_old[!is.infinite(p_old) & p_old<=1 & p_old>=0],n[!is.infinite(p_old) & p_old<=1 & p_old>=0],na.rm=T)
    ) %>%
    select(Education,p_real)

  return(result)
}




# Commented-out diagnostic histograms for p_different_sex, p_same_sex_chance, and mu_g;
# retained for future robustness checks
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



# get_town(): draws a bootstrap resample of towns with replacement.
# Resampling at the locality level accounts for within-town correlation
# in both ethnic composition and matching quality.
get_town<-function(towns){
  town_sample<- data.frame(SIRSUP_baseline=
                               sample(x = towns,
                                      size = length(towns),
                                      replace = TRUE))



  return(town_sample)
}

# --- Bootstrap loop: education panels ---
# Outer loop runs iter=200 bootstrap replications.
# set.seed(i) inside the loop ensures each replication is reproducible
# without fixing the global seed, so replications are independent.
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
  # LIM_baseline==11: Roma individuals whose declared mother tongue is Hungarian;
  # this subgroup is more assimilated and expected to show different passing patterns
  town_sample<-get_town(towns02_rom_hu)
  stats_2002_temp_roma_hun<-get_stats(data02 %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_hu)
  stats_1992_temp_roma_hun<-get_stats(data92 %>%
                                             filter(LIM_baseline==11) %>%
                                             mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-ro
  # LIM_baseline==10: Roma individuals with Romanian as mother tongue
  town_sample<-get_town(towns02_rom)
  stats_2002_temp_roma_ro<-get_stats(data02 %>%
                                            filter(LIM_baseline==10) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom)
  stats_1992_temp_roma_ro<-get_stats(data92 %>%
                                            filter(LIM_baseline==10) %>%
                                            mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  #roma-romani
  # LIM_baseline==12: Roma individuals with Romani as mother tongue;
  # least linguistically assimilated subgroup, expected lowest passing rate
  town_sample<-get_town(towns02_rom_romani)
  stats_2002_temp_roma_romani<-get_stats(data02 %>%
                                                filter(LIM_baseline==12) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))
  town_sample<-get_town(towns92_rom_romani)
  stats_1992_temp_roma_romani<-get_stats(data92 %>%
                                                filter(LIM_baseline==12) %>%
                                                mutate(ETH_baseline=ROMA_baseline,ETH_2011=ROMA_2011))

  # Combine all ethnic group results for the 1992-2011 panel into one wide row per education bin
  stats_1992_temp<-stats_1992_temp_roma %>%
    inner_join(stats_1992_temp_hun,by=c("Education"),suffix=c("","_hun")) %>%
    inner_join(stats_1992_temp_ger,by=c("Education"),suffix=c("","_ger")) %>%
    inner_join(stats_1992_temp_oth,by=c("Education"),suffix=c("","_oth")) %>%
    inner_join(stats_1992_temp_roma_hun,by=c("Education"),suffix=c("","_rom_hu")) %>%
    inner_join(stats_1992_temp_roma_ro,by=c("Education"),suffix=c("","_rom_ro")) %>%
    inner_join(stats_1992_temp_roma_romani,by=c("Education"),suffix=c("_rom","_rom_romani")) %>%
    mutate(Censuses='1992 - 2011')

  # Combine all ethnic group results for the 2002-2011 panel into one wide row per education bin
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

# Summarise bootstrap distribution: median (point estimate) and 5th/95th percentiles (90% CI)
# across all iter replications, grouped by census pair, education bin, and ethnic group
data_1_r<-bind_rows(data_1_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Education,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05),.names = "{col}_05")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) %>%
  # Decode the column-name suffixes from the wide join back to human-readable ethnicity labels
  mutate(Ethnicity=case_when(Ethnicity=="p_real_rom" ~ "Roma",
                             Ethnicity=="p_real_ger" ~ "German",
                             Ethnicity=="p_real_hun" ~ "Hungarian",
                             Ethnicity=="p_real_oth" ~ "Other",
                             Ethnicity=="p_real_roma_hu" ~ "Roma - Hungarian",
                             Ethnicity=="p_real_roma_ro" ~ "Roma - Romanian",
                             Ethnicity=="p_real_roma_romani" ~ "Roma - Romani"
                             )) %>%
  mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian","German","Other","Roma - Hungarian","Roma - Romanian","Roma - Romani"))) %>%
  # Sample: compound label combining ethnicity and census pair; used to filter subplots below
  mutate(Sample=case_when(Ethnicity=="Roma" & Censuses=="1992 - 2011" ~ "Roma '92-'11",
                          Ethnicity=="Roma" & Censuses=="2002 - 2011" ~ "Roma '02-'11",
                          Ethnicity=="Hungarian" & Censuses=="1992 - 2011" ~ "Hungarian '92-'11",
                          Ethnicity=="Hungarian" & Censuses=="2002 - 2011" ~ "Hungarian '02-'11",
                          Ethnicity=="Other" & Censuses=="1992 - 2011" ~ "Other '92-'11",
                          Ethnicity=="Other" & Censuses=="2002 - 2011" ~ "Other '02-'11"
                          ))



# Output: bootstrap-summarised education panel data saved for potential downstream use
setwd(wd_data_results)
saveRDS(data_1_r,"data_figure_a02_1.rds")




# --- Education panel plots ---
#education graphs----
# g1: 1992-2011 panel, Roma vs. Hungarian; shows how mismatch-corrected persistence
# rates vary across education bins for the two main groups
g1<-ggplot(data=data_1_r %>%
             filter(Ethnicity %in% c("Roma","Hungarian")) %>%
             # mutate(Ethnicity=factor(Ethnicity,levels=c("Roma","Hungarian"))) %>%
             filter(Sample %in% c("Hungarian '92-'11","Roma '92-'11","Other '92-'11")),
           aes(x=Education,y=Percentage_med,group=Ethnicity,color=Ethnicity))+
  geom_point(size=1.25,position=position_dodge(width=0.3))+
  # Error bars span the 5th-95th bootstrap percentile (90% CI)
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

# g1_other: same period, "Other" minority group only; no color mapping needed (single series)
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

# g1_2002: 2002-2011 panel counterpart to g1 for Roma and Hungarian
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

# g1_other_2002: 2002-2011 panel, "Other" group only
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

# Commented-out individual PDF exports for standalone education panels; superseded by combined g3
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_gender_ratio.pdf",width=8,height=5)
# g1
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_2002_gender_ratio.pdf",width=8,height=5)
# g1_2002
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_other_gender_ratio.pdf",width=8,height=5)
# g1_other
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_educ_other_2002_gender_ratio.pdf",width=8,height=5)
# g1_other_2002
# dev.off()


# --- Occupation panel: get_stats_ocup() ---
#ocupation graphs----
# get_stats_ocup(): applies the same mismatch correction logic as get_stats() but groups by
# Occupation rather than Education. Note: correction is pooled across education bins here,
# so it uses the simpler p_mismatch = p_different_sex * 2 formula rather than mu_g.
get_stats_ocup<-function(data){
  result<-data %>%
    filter(!is.na(Occupation)) %>%
    filter(ETH_baseline==T) %>%
    right_join(town_sample,by="SIRSUP_baseline",relationship="many-to-many") %>%
    group_by(Occupation) %>%
    # p_all: raw (uncorrected) ethnic persistence rate within each occupation group
    mutate(p_all=mean(ETH_2011==T,na.rm=T)) %>%
    group_by(mismatch,Occupation,p_all) %>%
    summarise(n=n(),p_eth_mismatch=mean(ETH_2011==T,na.rm=T), .groups = 'drop') %>%
    group_by(Occupation) %>%
    # p_mismatch: estimated share of false matches, derived from the sex-discordant pairs
    # (p_different_sex * 2, the simpler non-gender-adjusted formula)
    mutate(p_mismatch=n/sum(n)*2,n=sum(n)) %>%
    filter(mismatch==T) %>%
    # Apply the standard mismatch correction formula to each occupation bin
    mutate(p_real=(p_all-p_mismatch*p_eth_mismatch)/(1-p_mismatch)) %>%
    ungroup %>%
    select(Occupation,p_real)

  return(result)
}

# --- Bootstrap loop: occupation panels (identical structure to the education loop above) ---
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

# Summarise bootstrap distribution for the occupation panels
data_2_r<-bind_rows(data_2_r_temp) %>%
  pivot_longer(cols=p_real_rom:p_real_rom_romani,names_to="Ethnicity",values_to="Percentage") %>%
  group_by(Censuses,Occupation,Ethnicity) %>%
  summarise(across(Percentage, ~ median(., na.rm = TRUE),.names = "{col}_med"),
            across(Percentage, ~ quantile(., probs =0.95),.names = "{col}_95"),
            across(Percentage, ~ quantile(., probs =0.05),.names = "{col}_05")) %>%
  # Order occupation categories from lowest to highest socioeconomic status
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



# Output: bootstrap-summarised occupation panel data saved for potential downstream use
setwd(wd_data_results)
saveRDS(data_2_r,"data_figure_a02_2.rds")


# --- Occupation panel plots ---
#occupation graphs----
# g2: 1992-2011 panel, Roma vs. Hungarian; y-axis is mismatch-corrected persistence rate
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

# g2_other: 1992-2011 panel, "Other" group; single series so no color mapping
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




# g2_2002: 2002-2011 panel counterpart to g2
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

# g2_other_2002: 2002-2011 panel, "Other" group only
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


# Commented-out standalone occupation PDF exports; superseded by combined g3
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_gender_ratio.pdf",width=8,height=5)
# g2
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_other_gender_ratio.pdf",width=8,height=5)
# g2_other
# dev.off()
#
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_2002_gender_ratio.pdf",width=8,height=5)
# g2_2002
# dev.off()
#
# setwd(wd_output)
# pdf("02_Figure_binscatter_ocup_other_2002_gender_ratio.pdf",width=8,height=5)
# g2_other_2002
# dev.off()



# --- Final combined figure: education (left) + occupation (right), 1992-2011 panel ---
# g3: two-panel composite that constitutes the published Figure A.2.
# Left panel: education gradient; right panel: occupation gradient; legend moved to left panel only.
g3<-plot_grid(g1+theme(legend.position = c(.25,.15)), g2+theme(legend.position ="none")+labs(y=element_blank() ))
# Output: Figure A02.pdf -- Appendix Figure A.2 (gender-adjusted mismatch correction, 1992-2011)
setwd(wd_output)
pdf("Figure A02.pdf",width=10,height=6)
g3
dev.off()

# Commented-out alternative combined panels (other minority groups and 2002-2011 versions);
# retained for robustness documentation
# g3_other<-plot_grid(g1_other+theme(legend.position = c(.25,.15)), g2_other+theme(legend.position ="none")+labs(y=element_blank() ))
# setwd(wd_output)
# pdf("02_Figure_binscatter_both_other_gender_ratio.pdf",width=10,height=6)
# g3_other
# dev.off()
#
#
# g3_2002<-plot_grid(g1_2002+theme(legend.position = c(.25,.15)), g2_2002+theme(legend.position ="none")+labs(y=element_blank() ))
# setwd(wd_output)
# pdf("02_Figure_binscatter_both_2002_gender_ratio.pdf",width=10,height=6)
# g3_2002
# dev.off()
#
# g3_other_2002<-plot_grid(g1_other_2002+theme(legend.position = c(.25,.15)), g2_other_2002+theme(legend.position ="none")+labs(y=element_blank() ))
# setwd(wd_output)
# pdf("02_Figure_binscatter_both_other_2002_gender_ratio.pdf",width=10,height=6)
# g3_other_2002
# dev.off()




