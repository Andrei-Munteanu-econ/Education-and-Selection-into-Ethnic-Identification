# =====================================================================
# Compares the structural estimates across heterogeneity distributions.
# Produces:  05_Figure_structural_comparison.pdf
# Inputs:    results_parallel.rds (Normal), results_uniform.rds (Uniform),
#            results_parallel_lognormal.rds (Lognormal)
# Summary:   Pools the bootstrap estimates from the three distributions, labels
#            education groups, summarises each group's actual/observed Roma ratio
#            (pr_obs / pr) with 5th-95th-percentile bootstrap bands, and plots them
#            by education with one series per distribution.
# =====================================================================

# ---- Load and label estimates per distribution ----
# Normal model results
setwd(wd_data_structural)
results1<-readRDS("results_parallel.rds")
results1<-results1 %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  mutate(educ=as.numeric(as.character(id))) %>%
  mutate(Model="Normal")

# Uniform model results
results2<-readRDS("results_uniform.rds")
results2<-results2 %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  mutate(educ=as.numeric(as.character(id))) %>%
  mutate(Model="Uniform")

# Lognormal model results
results3<-readRDS("results_parallel_lognormal.rds")
results3<-results3 %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  mutate(educ=as.numeric(as.character(id))) %>%
  mutate(Model="Lognormal")
# Stack all three distributions into one long data frame
results_all<-bind_rows(results1,results2,results3)

# ---- Summarise the actual/observed Roma ratio with bootstrap bands ----
# Average the parameters across bootstrap replicates and take 5th/95th percentiles
# as the band; d2 here is redefined as observed/latent (pr_obs/pr), so 1/d2 is the
# undercount ratio of actual to observed Roma plotted below.
results_sum<-results_all %>%
  group_by(id,Education,educ,Model) %>%
  summarise(pr_05=quantile(pr, probs = 0.05),pr_95=quantile(pr, probs = 0.95),pr=mean(pr,na.rm=T),
            d0_05=quantile(d0, probs = 0.05),d0_95=quantile(d0, probs = 0.95),d0=mean(d0,na.rm=T),
            d1_05=quantile(d1, probs = 0.05),d1_95=quantile(d1, probs = 0.95),d1=mean(d1,na.rm=T),
            d2_05=quantile(d2, probs = 0.05),d2_95=quantile(d2, probs = 0.95),d2=mean(d2,na.rm=T),
            # sigma_05=quantile(sigma, probs = 0.05),sigma_95=quantile(sigma, probs = 0.95),sigma=mean(sigma,na.rm=T),
            # mu_05=quantile(mu, probs = 0.05),mu_95=quantile(mu, probs = 0.95),mu_1=mean(mu,na.rm=T)
            pr_obs=mean(pr_obs),
            n=mean(n)
  )  %>%
  mutate(d2_05=pr_obs/pr_95,d2_95=pr_obs/pr_05,d2=pr_obs/pr) %>%
  arrange(educ)

# ---- Build the comparison figure ----
# Actual/observed Roma (1/d2) by education, one coloured series per distribution,
# with bootstrap error bars; points dodged so the three distributions are visible.
g1b<-ggplot(results_sum,aes(x=Education,y=1/d2,group=Model,color=Model))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=1/d2_05,ymax=1/d2_95),width=0.1,position=position_dodge(width=0.5))+
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),  
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.title=element_text(size=14),
        legend.position=c(0.2,0.8),
        plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=14))+
  xlab("Educational Attainment")+
  ylab("Actual/Observed Roma")+
  # scale_x_continuous(breaks=0:10)+
  scale_y_continuous(breaks=seq(from = 0, to = 20, by = 1),
                     minor_breaks = seq(from = 0, to = 20, by = 1)
                     # labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
                     # labels = scales::percent,
  )+
  scale_x_discrete(labels = function(x) str_wrap(x, width =8))+
  expand_limits(y=0)+
  labs(color=str_wrap("Distribution",width=8))

g1b


# ---- Save figure ----
setwd(wd_output)
pdf("05_Figure_structural_comparison.pdf",width=8,height=5)
g1b
dev.off() 