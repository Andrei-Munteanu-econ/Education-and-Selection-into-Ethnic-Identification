# Produces Figures A.7 and A.8: Sample Representativeness (Roma and all individuals)

# --- Purpose ---
# Figures A.7 and A.8 assess whether the linked (panel) samples are representative
# of the full 2011 census population on key observable characteristics:
# education, locality population size, sex, and age.
# Figure A.8 covers all individuals; Figure A.7 covers Roma-identified individuals only.
# Representativeness is evaluated by comparing distributions across three samples:
#   (1) full 2011 census cross-section,
#   (2) 2002-2011 linked panel (individuals observed in both 2002 and 2011),
#   (3) 1992-2011 linked panel (individuals observed in both 1992 and 2011).
# If linked samples are representative, the bars for all three census labels should be similar.

# --- Commented-out code: loading 1992 and 2002 cross-sections (not used in final version) ---
# The 1992 and 2002 full cross-sections were loaded in an earlier draft but are
# not needed for these figures, which use the 2011 cross-section as the reference.
# setwd(wd_data_92)
# filename<-'data_1992_clean.csv'
# data_92<-read_sample(filename) %>%
#   select(ROMA,years,ET,AA,pop_SIRSUP_1992,SEX)
# data_92<-read_data(filename,data_92)
# data_92<-data_92 %>%
#   mutate(Census="1992")
#
# setwd(wd_data_02)
# filename<-'data_2002_clean.csv'
# data_02<-read_sample(filename) %>%
#   select(ROMA,years,ET,AA,pop_SIRSUP_2002,SEX)
# data_02<-read_data(filename,data_02)
# data_02<-data_02 %>%
#   mutate(Census="2002")

# --- Load 2011 cross-section (reference/benchmark population) ---
# This is the full 2011 census population used as the denominator for representativeness checks.
setwd(wd_data_11)
filename<-'data_2011_clean.csv'
# read_sample() loads the relevant subset of columns; read_data() reads the full file
# keeping only rows present in the sample; key variables: ROMA, years (schooling),
# ET (ethnicity code), AA (birth year), pop_SIRSUP_2011 (locality size), SEX
data_11<-read_sample(filename) %>%
  select(ROMA,years,ET,AA,pop_SIRSUP_2011,SEX)
data_11<-read_data(filename,data_11)
# Census label marks this as the 2011 full cross-section for faceting/legend later
data_11<-data_11 %>%
  mutate(Census="2011")

# --- Load 1992-2011 linked panel ---
# Individuals matched across both the 1992 and 2011 censuses; "unique" suffix
# indicates that duplicate matches have been removed (one-to-one linkage).
setwd(wd_data_linked)
filename<-'data_1992_2011_unique.csv'
# Retain 2011-wave attributes (ROMA_2011, years_2011, AA_2011, pop_SIRSUP_2011, SEX_2011)
# plus the 1992 Roma flag (ROMA_1992) and 1992 locality size for match-rate calculations.
data_92_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ROMA_1992,pop_SIRSUP_1992)
data_92_11<-read_data(filename,data_92_11)
# Rename to common column names so the panel can be row-bound with the 2011 cross-section.
# ROMA_1992 becomes ROMA_baseline (Roma status at baseline census) to distinguish it from
# the 2011 Roma flag used for outcome measurement.
data_92_11<-data_92_11 %>%
  mutate(Census="1992 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011,ROMA_baseline=ROMA_1992)

# --- Load 2002-2011 linked panel (first pass: two-census linkage) ---
setwd(wd_data_linked)
filename<-'data_2002_2011_unique.csv'
data_02_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ROMA_2002,pop_SIRSUP_2002)
data_02_11<-read_data(filename,data_02_11)
data_02_11<-data_02_11 %>%
  mutate(Census="2002 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011,ROMA_baseline=ROMA_2002)

# --- Overwrite with three-census linked panel (1992-2002-2011) ---
# This block supersedes the two-census 2002-2011 linkage above.
# The triple-linked file restricts to individuals present in all three censuses,
# which is the primary analysis sample. Using this ensures the representativeness
# figure reflects the actual sample used in the paper's regressions.
setwd(wd_data_linked)
filename<-'data_1992_2002_2011_unique.csv'
data_02_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ROMA_2002,pop_SIRSUP_2002)
data_02_11<-read_data(filename,data_02_11)
data_02_11<-data_02_11 %>%
  mutate(Census="2002 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011,ROMA_baseline=ROMA_2002)

# Harmonise the SEX column name across both linked panels so bind_rows() works correctly
data_02_11<-data_02_11 %>%
  rename(SEX=SEX_2011)
data_92_11<-data_92_11 %>%
  rename(SEX=SEX_2011)

# --- Stack all three samples into a single long data frame ---
# Census column distinguishes: "2011" (full cross-section), "2002 - 2011" (2-or-3-wave panel),
# "1992 - 2011" (long panel). Characteristics are always measured in 2011 to keep
# the comparison on the same scale across all three samples.
data<-bind_rows(data_11,data_02_11,data_92_11)

# --- Recode continuous variables into categorical bins for bar charts ---
# Education bins collapse the harmonised years-of-schooling variable into five
# broad categories that match conventional Romanian school stages.
# pop_SIRSUP_2011: locality population in 2011 — binned into six size classes
# to capture rural/small-town vs. urban gradient (key for match rates).
data<-data %>%
  mutate(Education=case_when(years==0 ~ "None",
                             years==4 ~ "Primary School",
                             years %in% 8:10  ~ "Middle School",
                             years %in% 12:13 ~ "High School or Vocational",
                             years %in% 14:16 ~ "Post- Secondary")) %>%
  mutate(pop=cut(pop_SIRSUP_2011,
                 breaks = c(0,1000,5000,10000,50000,200000,Inf),
                 labels = c("<1k","1-5k","5-10k", "10-50k", "50-200k",
                            ">200k"))) %>%
  # Fix the ordering of education levels for consistent display on x-axis
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))

# --- Compute within-sample proportions by locality size, for all individuals ---
# Each bar shows the share of individuals in a given pop bin, separately per Census label.
# Comparing bars across Census labels reveals whether linked-sample individuals
# are over-/under-represented in certain locality sizes relative to the 2011 universe.
stats_pop<-data %>%
  group_by(pop,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Locality Population (2011)")

# --- Compute within-sample proportions by education level, for all individuals ---
# NAs arise when years of schooling falls outside the coded bins; these are dropped.
stats_educ<-data %>%
  filter(!is.na(Education)) %>%
  group_by(Education,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Education (2011)")

# --- Compute within-sample proportions by sex, for all individuals ---
# SEX==1 is Male, SEX==2 is Female (LL coding in raw census; harmonised to SEX here).
stats_gender<-data %>%
  mutate(SEX=ifelse(SEX==1,"Male","Female")) %>%
  filter(!is.na(SEX)) %>%
  group_by(SEX,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="SEX") %>%
  # Explicit factor ordering ensures Males appear before Females on the x-axis
  mutate(SEX=factor(SEX,levels=c("Male","Female")))

# --- Compute within-sample proportions by age group, for all individuals ---
# AGE_baseline: age as of 2011 (used for consistency with the reference year).
# Subtract 1 because individuals born in year AA turn (2011-AA-1) by year-end on average.
# The 41+ bin absorbs all older individuals; the 10-20 bin corresponds to
# teenagers who would have been children in 1992 (not in the linked panel).
stats_age<-data %>%
  mutate(AGE_baseline=2011-AA-1) %>%
  mutate(AGE=case_when(AGE_baseline %in% 10:20 ~ "10-20",
                       AGE_baseline %in% 21:30 ~ "21-30",
                       AGE_baseline %in% 31:40 ~ "31-40",
                       AGE_baseline %in% 41:110 ~ "41+"
                       )) %>%
  filter(!is.na(AGE)) %>%
  group_by(AGE,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Age (2011)") %>%
  mutate(AGE=factor(AGE,levels=c("10-20","21-30","31-40","41+")))


# --- Combine all four summary statistics into a single long data frame for ggplot ---
# Each row represents one (category bin, Census) combination with a proportion value.
# All category-specific columns are unified into a single x column to allow faceting.
data_graph <- bind_rows(
  stats_educ %>% mutate(x = factor(Education, levels = levels(Education))) %>% select(-Education),
  stats_pop %>% mutate(x = factor(pop, levels = levels(pop))) %>% select(-pop),
  stats_age %>% mutate(x = factor(AGE, levels = levels(AGE))) %>% select(-AGE),
  stats_gender %>% mutate(x = factor(SEX, levels = levels(SEX))) %>% select(-SEX)
)

# --- Figure A.8: Bar chart of sample representativeness for ALL individuals ---
# Each panel (facet) corresponds to one characteristic (Education, Locality Pop, Age, Sex).
# Bars are side-by-side (dodge) by Census label.
# scales="free_x" allows each facet to show only the relevant categories on its x-axis.
g<-ggplot(data=data_graph,aes(
  x=x,
  y=proportion ,
  fill=Census))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~stat,scales="free_x")+
  #scale_x_continuous(breaks=c(1992,2002,2011))+
  #theme(axis.title.y=element_text("Town Population (1992)"))+
  theme(strip.text.x = element_text(size=14),
    axis.text.x = element_text(angle = 0, vjust = 0.5,size=14),
    axis.text.y = element_text(size=14),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=16),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    legend.justification = c("center"),
    legend.position = c("top"))+
  # theme(legend.key.size = unit(0.8, 'cm'),
  #       legend.title.align=0.5,
  #       legend.text = element_text(size=12),
  #       legend.title=element_text(size=12),
  #       plot.title = element_text(size = 15),
  #       legend.justification = c("center"),
  #       legend.position = c("top"),
  #       plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
  #       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  #       strip.text.x = element_text(size = 12))+
  guides(fill=guide_legend(title="Census"))+
  # Express y-axis as percentages rounded to nearest 1 pp
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  # Wrap long category labels to avoid overlap on x-axis (e.g. "High School or Vocational")
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  xlab(element_blank())+
  ylab("Proportion")
g

# Output: Figure A.8 — sample representativeness for all individuals
setwd(wd_output)
pdf("Figure A08.pdf",width=10,height=6)
g
dev.off()

#roma----
# --- Figures A.7: same representativeness check restricted to Roma-identified individuals ---
# Filtering to ROMA==T at each step ensures proportions are computed within the Roma
# subsample, so the comparison shows whether linked Roma individuals are representative
# of Roma in the full 2011 census on the same four characteristics.
stats_pop_roma<-data %>%
  filter(ROMA==T) %>%
  group_by(pop,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Locality Population")
stats_educ_roma<-data %>%
  filter(ROMA==T) %>%
  filter(!is.na(Education)) %>%
  group_by(Education,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Education")
stats_gender_roma<-data %>%
  filter(ROMA==T) %>%
  mutate(SEX=ifelse(SEX==1,"Male","Female")) %>%
  filter(!is.na(SEX)) %>%
  group_by(SEX,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="SEX") %>%
  mutate(SEX=factor(SEX,levels=c("Male","Female")))
stats_age_roma<-data %>%
  filter(ROMA==T) %>%
  mutate(AGE_baseline=2011-AA-1) %>%
  mutate(AGE=case_when(AGE_baseline %in% 10:20 ~ "10-20",
                       AGE_baseline %in% 21:30 ~ "21-30",
                       AGE_baseline %in% 31:40 ~ "31-40",
                       AGE_baseline %in% 41:110 ~ "41+"
  )) %>%
  filter(!is.na(AGE)) %>%
  group_by(AGE,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Age (2011)") %>%
  mutate(AGE=factor(AGE,levels=c("10-20","21-30","31-40","41+")))




# --- Combine Roma-only summary statistics for ggplot ---
data_graph <- bind_rows(
  stats_educ_roma %>% mutate(x = factor(Education, levels = levels(Education))) %>% select(-Education),
  stats_pop_roma %>% mutate(x = factor(pop, levels = levels(pop))) %>% select(-pop),
  stats_age_roma %>% mutate(x = factor(AGE, levels = levels(AGE))) %>% select(-AGE),
  stats_gender_roma %>% mutate(x = factor(SEX, levels = levels(SEX))) %>% select(-SEX)
)


# --- Figure A.7: Bar chart of sample representativeness for Roma-identified individuals ---
# Same layout as Figure A.8 but restricted to Roma (ROMA==T).
g<-ggplot(data=data_graph,aes(
  x=x,
  y=proportion ,
  fill=Census))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~stat,scales="free_x")+
  #scale_x_continuous(breaks=c(1992,2002,2011))+
  #theme(axis.title.y=element_text("Town Population (1992)"))+
  theme(strip.text.x = element_text(size=14),
        axis.text.x = element_text(angle = 0, vjust = 0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.justification = c("center"),
        legend.position = c("top"))+
  # theme(legend.key.size = unit(0.8, 'cm'),
  #       legend.title.align=0.5,
  #       legend.text = element_text(size=12),
  #       legend.title=element_text(size=12),
  #       plot.title = element_text(size = 15),
  #       legend.justification = c("center"),
  #       legend.position = c("top"),
  #       plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
  #       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  #       strip.text.x = element_text(size = 12))+
  guides(fill=guide_legend(title="Census"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  xlab(element_blank())+
  ylab("Proportion")
g

# Output: Figure A.7 — sample representativeness for Roma-identified individuals
setwd(wd_output)
pdf("Figure A07.pdf",width=10,height=6)
g
dev.off()

# see summary_stats_for_matching
# --- Commented-out match rate calculations (moved to summary_stats_for_matching script) ---
# These snippets compute the share of 2011 individuals (and Roma specifically) who
# were successfully linked to a 1992 record, overall and by locality size.
# Key figures cited in the paper: ~33% overall link rate, ~36% for Roma,
# rising to ~50% in small towns (pop <= 10,000) where cell-level matching is denser.
# ##match rate Roma (2011):
# #2011 individuals linked: 32.9%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991))/
#   nrow(data %>% filter(Census=="2011") %>% filter(AA<=1991))
#
# #2011 Roma linked: 36.0\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991) %>% filter(ROMA==T))/
#   nrow(data %>% filter(Census=="2011") %>% filter(AA<=1991) %>% filter(ROMA==T))
#
# #2011 ind. linked small towns: 53.3\%
# nrow(data %>% filter(Census=="1992 - 2011") %>%
#       filter(pop_SIRSUP_2011<=10000 & AA<=1991))/
#   nrow(data %>% filter(Census=="2011") %>%
#         filter(pop_SIRSUP_2011<=10000 & AA<=1991))
#
# #2011 Roma linked:  45.7\%
# nrow(data %>% filter(Census=="1992 - 2011") %>%
#       filter(pop_SIRSUP_2011<=10000 & AA<=1991) %>% filter(ROMA==T))/
#   nrow(data %>% filter(Census=="2011") %>%
#         filter(pop_SIRSUP_2011<=10000 & AA<=1991) %>% filter(ROMA==T))


# ##match rate Roma (1992):
# #24.0\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991))
# #27.5\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991) %>% filter(ROMA_baseline==T))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991) %>% filter(ROMA==T))
# #40.3%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000) )
# #38.2\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000) %>% filter(ROMA_baseline==T))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000) %>% filter(ROMA==T))
#
#
# #not too old:
# #49.7%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000) )
# #41.7\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000) %>% filter(ROMA_baseline==T))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000) %>% filter(ROMA==T))
