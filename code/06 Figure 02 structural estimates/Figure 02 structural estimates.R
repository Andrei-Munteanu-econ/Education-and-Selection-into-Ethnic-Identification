# Produces Figure 2: Estimated Roma Population Accounting for Heterogeneity

# --- Section 1: Load Structural Estimation Results ---

# Switch to the directory holding the structural model .rds output files
setwd(wd_data_structural)

# results1: bootstrap estimates (100 iterations) from the Normal cost-heterogeneity model.
# Each row is one bootstrap draw x education cell; columns include pr (Roma share
# implied by the model), d0/d1/d2 (scaling factors), and pr_obs (raw observed share).
results1<-readRDS("results_parallel.rds")
results1<-results1 %>%
  # Map numeric education-years (id) to readable labels matching the harmonised EDUC factor.
  # id values correspond to years_1992 / years_2011 bins: 0=no schooling, 4=primary,
  # 8-10=middle school (gym), 12-13=High School or Vocational, 14-16=post-secondary/higher.
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School or Vocational",
                             id %in% 14:16 ~ "Postsec.")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School or Vocational","Postsec."))) %>%
  # Retain id as a numeric variable for ordering and merging on education years
  mutate(educ=as.numeric(as.character(id))) %>%
  # Tag this block of rows as the Normal distributional assumption for cost heterogeneity
  mutate(Model="Normal")

# results2: bootstrap estimates from the Uniform cost-heterogeneity model
results2<-readRDS("results_uniform.rds")
results2<-results2 %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School or Vocational",
                             id %in% 14:16 ~ "Postsec.")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School or Vocational","Postsec."))) %>%
  mutate(educ=as.numeric(as.character(id))) %>%
  # Tag this block of rows as the Uniform distributional assumption
  mutate(Model="Uniform")

# results3: bootstrap estimates from the Lognormal cost-heterogeneity model
results3<-readRDS("results_parallel_lognormal.rds")
results3<-results3 %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School or Vocational",
                             id %in% 14:16 ~ "Postsec.")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School or Vocational","Postsec."))) %>%
  mutate(educ=as.numeric(as.character(id))) %>%
  # Tag this block of rows as the Lognormal distributional assumption
  mutate(Model="Lognormal")

# Stack all three model results into one long data frame for joint summarisation
results_all<-bind_rows(results1,results2,results3)


# --- Section 2: Summarise Bootstrap Distributions by Education x Model ---

# Collapse bootstrap draws to point estimates (mean) and 90% confidence bands
# (5th and 95th percentiles) for each structural quantity, within each
# education-cell x distributional-assumption combination.
results_sum<-results_all %>%
  group_by(id,Education,educ,Model) %>%
  summarise(pr_05=quantile(pr, probs = 0.05),pr_95=quantile(pr, probs = 0.95),pr=mean(pr,na.rm=T),
            d0_05=quantile(d0, probs = 0.05),d0_95=quantile(d0, probs = 0.95),d0=mean(d0,na.rm=T),
            d1_05=quantile(d1, probs = 0.05),d1_95=quantile(d1, probs = 0.95),d1=mean(d1,na.rm=T),
            d2_05=quantile(d2, probs = 0.05),d2_95=quantile(d2, probs = 0.95),d2=mean(d2,na.rm=T),
            # sigma_05=quantile(sigma, probs = 0.05),sigma_95=quantile(sigma, probs = 0.95),sigma=mean(sigma,na.rm=T),
            # mu_05=quantile(mu, probs = 0.05),mu_95=quantile(mu, probs = 0.95),mu_1=mean(mu,na.rm=T)
            # pr_obs: average observed Roma share in the cell (constant across bootstrap draws)
            pr_obs=mean(pr_obs),
            n=mean(n)
  )  %>%
  # d2 (and its CI bounds) is defined as the ratio of the model-implied true Roma share
  # to the reported share: d2 = pr_obs / pr.  A value > 1 means the structural model
  # estimates more Roma heritage than self-reports suggest (i.e., some Roma are passing).
  # CI inverts pr quantiles: dividing by pr_95 gives the lower bound of d2,
  # and dividing by pr_05 gives the upper bound.
  mutate(d2_05=pr_obs/pr_95,d2_95=pr_obs/pr_05,d2=pr_obs/pr) %>%
  arrange(educ)

# Append a "Reported" row for each education cell with d2 = 1, representing the
# null benchmark where reported rates equal true rates (no passing / no correction).
results_sum<-rbindlist(list(results_sum,data.frame(Education=unique(results_sum$Education),d2=1,Model="Reported")),fill=T) %>%
  # Order factor levels so "Reported" appears first in legend and plot
  mutate(Model=factor(Model,
                      levels=c("Reported","Lognormal","Normal","Uniform")))


# --- Section 3: Panel A — Ratio of Estimated to Reported Roma by Education (g1) ---

# g1: for each education group, plots the ratio (model-implied true Roma share) /
# (reported Roma share), i.e. 1/d2.  A ratio > 1 means the model infers more Roma
# heritage than self-reports indicate.  Separate series for each distributional assumption.
g1<-ggplot(results_sum,aes(x=Education,y=1/d2,group=Model,color=Model))+
  geom_point(position=position_dodge(width=0.5))+
  # 90% bootstrap confidence intervals from the education-cell-level bootstrap
  geom_errorbar(aes(ymin=1/d2_05,ymax=1/d2_95),width=0.1,position=position_dodge(width=0.5))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.title=element_text(size=14,hjust = 0.5),
        # legend.position=c(0.2,0.75),
        legend.position="none",
        plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=9))+
  xlab("Educational Attainment")+
  ylab("Roma Heritage (Estimated)/Reported")+
  # scale_x_continuous(breaks=0:10)+
  scale_y_continuous(breaks=seq(from = 0, to = 20, by = 1),
                     minor_breaks = seq(from = 0, to = 20, by = 1)
                     # labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
                     # labels = scales::percent,
  )+
  # Wrap long education labels so they fit on the x-axis without rotation
  scale_x_discrete(labels = function(x) str_wrap(x, width =8))+
  expand_limits(y=0)+
  labs(color=str_wrap("Model:",width=8))

g1


# --- Section 4: Load 2011 Census Data for Population-Level Extrapolation ---

# Switch to the directory containing the cleaned 2011 census file
setwd(wd_data_11)
filename<-"data_2011_clean.csv"

# Read only the columns needed for this extrapolation to keep memory usage manageable
data_all_2011<-read_sample(filename) %>%
  dplyr::select(EDUC,cell_id_1992,cell_id_2002,ROMA,AA,SCU)

# Read the full data and correct EDUC for young adults (age 18-25 in 2011) whose
# SCU code suggests continued schooling that was not yet reflected in EDUC:
# SCU <= 65 maps to post-secondary enrolment; SCU <= 92 maps to general High School or Vocational.
# This correction prevents misclassification of those who have not yet completed their
# terminal degree at the time of the census.
data_all_2011<-read_data(filename,data_all_2011) %>%
  mutate(EDUC=case_when((2011-AA) %in% 18:25 & SCU<=65 & SCU>0 ~ "Postsec",
                        (2011-AA) %in% 18:25 & SCU<=92 & SCU>0 ~ "General HS",
                        TRUE ~ EDUC)) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))


# Restrict to valid education records (exclude "Sub 10" which signals
# implausible or missing schooling) and to birth cohorts born by 1992,
# so individuals were at least 19 at the time of the 2011 census and
# thus plausibly subject to the same passing decision modelled for the
# 1992-2011 cohort.
data_all_2011<-data_all_2011 %>%
  filter(EDUC!="Sub 10" & AA<=1992) %>%
  # Collapse the EDUC factor into the same coarsened numeric bins (years of schooling)
  # used in the structural model to allow merging with results_sum
  mutate(educ=case_when(EDUC=="No formal" ~ 0,
                        EDUC=="Primary" ~ 4,
                        EDUC=="Gym" ~ 8,
                        EDUC %in% c("Higher Long","Higher Short") ~ 16,
                        EDUC=="Sub 10" ~ NA_real_,
                        TRUE ~ 12))

# Diagnostic: mean education years by Roma status in 2011 —
# used to check whether Roma and non-Roma populations differ in education composition
educ_2011_eth<-data_all_2011 %>%
  # filter(EDUC!="Sub 10" & AA<=1992) %>%
  group_by(ROMA) %>%
  summarise(educ=mean(educ,na.rm=T))


# --- Section 5: Merge Structural Scaling Factors into 2011 Census Records ---

# Join the structural model's d2 scaling factors (and CI bounds) to every 2011
# individual by matching on the coarsened education bin (educ).
# This allows us to inflate the observed Roma count in each cell by 1/d2 to
# obtain the model-implied true Roma headcount.
data_all_2011_sum<-data_all_2011 %>%
  # filter( EDUC!="Sub 10") %>%
  # filter(AA<=1992) %>%
  left_join(results_sum %>%
              dplyr::select(id,educ,Model,Education,matches("d2"),matches("pass")))

# Aggregate to education x model cells to get observed and estimated Roma counts
data_all_2011_sum2<-data_all_2011_sum %>%
  group_by(educ,Model,Education,d2_05,d2_95,d2) %>%
  summarise(n=n(),n_ROMA=sum(ROMA==T)) %>%
  # pr = estimated true Roma share (1/d2); pr bounds follow from d2 CI bounds
  mutate(pr=1/d2,pr_high=1/d2_95,pr_low=1/d2_05) %>%
  # n_mid: central estimate of true Roma count (reported Roma / d2);
  # n_low/n_high: 90% CI bounds on the true Roma count
  mutate(n_mid=n_ROMA/d2,n_low=n_ROMA/d2_95,n_high=n_ROMA/d2_05)

# Reshape to long format so both "Reported" and "Estimated" counts appear as series
# in the same ggplot aesthetic
data_all_2011_sum_long<-data_all_2011_sum2 %>%
  pivot_longer(cols=c(n_mid,n_ROMA)) %>%
  # Confidence interval bands only apply to the estimated (structural) series;
  # set them to NA for the raw reported count
  mutate(n_low=ifelse(name=="n_ROMA",NA,n_low),
         n_high=ifelse(name=="n_ROMA",NA,n_high),
         name=ifelse(name=="n_ROMA","Reported","Estimated"))

# Keep only the estimated series for structural models and one copy of the
# reported series (arbitrarily taken from the Uniform model rows, since the
# reported count is the same regardless of model)
data_all_2011_sum_long<-data_all_2011_sum_long %>%
  filter(name=="Estimated" |(name=="Reported" & Model=="Uniform"))

# Compute national totals across education cells for each series,
# to be embedded in legend labels (e.g. "Normal (660k)")
totals<-data_all_2011_sum_long %>%
  group_by(name,Model) %>%
  summarise(n=sum(value)) %>%
  rename(totals=n)

# Merge totals back and build legend labels that include the population total
data_all_2011_sum_long<-data_all_2011_sum_long %>%
  left_join(totals) %>%
  mutate(name=case_when(name=="Reported" ~
                          paste0("Reported (",
                                 f_dec(totals/1000), "k)"),
                        name=="Estimated" ~
                          paste0(Model," (",
                                 f_dec(totals/1000), "k)")
  ))

# Fix factor level order for legend consistency with the paper's presentation order
data_all_2011_sum_long<-data_all_2011_sum_long %>%
  mutate(name=factor(name,
                     levels=c("Reported (384k)","Lognormal (617k)","Normal (660k)","Uniform (610k)")))


# --- Section 6: Panel B — Absolute Estimated Roma Counts by Education (g2) ---

# g2: for each education group, shows the absolute number of individuals with
# Roma heritage under each distributional assumption, alongside the reported count.
# The gap between estimated and reported series quantifies the scale of ethnic passing.
g2<-ggplot(data_all_2011_sum_long,aes(x=Education,y=value,color=name))+
  geom_point(position=position_dodge(width=0.5))+
  # 90% bootstrap CI bands for the structural estimates; reported count has no CI
  geom_errorbar(aes(ymin=n_low,ymax=n_high),width=0.1,position=position_dodge(width=0.5))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.title=element_text(size=9,hjust = 0.5),
        legend.text=element_text(size=9),
        legend.position=c(.78,.8),
        # legend.position = element_blank(),
        legend.title.align=0.5)+
  xlab("Educational Attainment")+
  ylab("Estimated Roma Heritage (Thousands)")+
  # scale_x_continuous(breaks=0:10)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  # Display y-axis in thousands (scale = 1e-3) with no decimals
  scale_y_continuous(breaks=seq(from = 0, to = 1000000, by = 20000),
                     labels=unit_format(unit = "", scale = 1e-3,accuracy=1.0),
  )+
  labs(color=str_wrap("Estimate:",width=8))
g2

# --- Output: Save Figure 02 (two-panel, 10x5 inches) ---
# Left panel (g1): ratio of estimated-to-reported Roma share by education and model.
# Right panel (g2): absolute estimated Roma heritage counts by education and model.
setwd(wd_output)
pdf("Figure 02.pdf",width=10,height=5)
grid.arrange(g1, g2,  ncol=2,widths = c(1/2,1/2))
dev.off()



