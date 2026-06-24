# =====================================================================
# Appendix Table A01 — Cross-wave linkage counts and inconsistent-sex rates
# Produces:  output/Table A01.tex
# Inputs:    data_2002_2011_unique_genderless.csv, data_1992_2011_unique_genderless.csv
#            (gender-blind linked census panels)
# Summary:   For each linked panel (1992->2011, 2002->2011) and each subgroup
#            (All, Roma, Educated = >8 years), reports the share of links whose
#            recorded sex is inconsistent across waves (Panel A, a linkage-quality
#            proxy) and the total number of unique matches (Panel B). Emits a
#            two-panel LaTeX tabular.
# =====================================================================

# ---- Load gender-blind linked census panels ----
# Match rates estimated using "genderless" matches so sex can serve as a check.
setwd(wd_data_linked)
data_2002_2011<-fread('data_2002_2011_unique_genderless.csv')
data_1992_2011<-fread('data_1992_2011_unique_genderless.csv')

tab<-data.frame("All"=c(nrow(data_1992_2011),nrow(data_2002_2011)),
                "Roma"=c(nrow(data_1992_2011 %>% filter(ROMA_1992==T)),
                         nrow(data_2002_2011 %>% filter(ROMA_2002==T))),
                "Educated"=c(nrow(data_1992_2011 %>% filter(years_2011>8)),
                         nrow(data_2002_2011 %>% filter(years_2011>8)))
                )

#Stats
stats_2002<-data_2002_2011 %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  group_by(mismatch) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

stats_1992<-data_1992_2011 %>%
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  group_by(mismatch) %>%
  summarise(n=n())%>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

#Stats Roma
stats_2002_roma<-data_2002_2011 %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  filter(ROMA_2002==T) %>%
  group_by(mismatch) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

stats_1992_roma<-data_1992_2011 %>%
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  filter(ROMA_1992==T) %>%
  group_by(mismatch) %>%
  summarise(n=n())%>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

#Stats Educ
stats_2002_educ<-data_2002_2011 %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  filter(years_2011>8) %>%
  group_by(mismatch) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

stats_1992_educ<-data_1992_2011 %>%
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  filter(years_2011>8) %>%
  group_by(mismatch) %>%
  summarise(n=n())%>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

#Stats Educ Roma
stats_2002_test<-data_2002_2011 %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  filter(years_2002>=4 & ROMA_2002==T) %>%
  group_by(mismatch) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

stats_1992_test<-data_1992_2011 %>%
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  filter(years_1992>=4 & ROMA_1992==T) %>%
  group_by(mismatch) %>%
  summarise(n=n())%>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

# ---- assemble the two panels ------------------------------------------------
# Panel A: match counts ; Panel B: gender-mismatch rates (%)
# Both share columns All / Roma / Educated and rows 1992-2011 / 2002-2011.

row_lab <- c("1992--2011", "2002--2011")

tab <- data.frame(
  All      = c(nrow(data_1992_2011), nrow(data_2002_2011)),
  Roma     = c(nrow(data_1992_2011 %>% filter(ROMA_1992 == T)),
               nrow(data_2002_2011 %>% filter(ROMA_2002 == T))),
  Educated = c(nrow(data_1992_2011 %>% filter(years_2011 > 8)),
               nrow(data_2002_2011 %>% filter(years_2011 > 8)))
)

stats <- data.frame(
  All      = c(stats_1992$p,      stats_2002$p)      * 100,
  Roma     = c(stats_1992_roma$p, stats_2002_roma$p) * 100,
  Educated = c(stats_1992_educ$p, stats_2002_educ$p) * 100
)

# formatters
# fmt_n   <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
# fmt_pct <- function(x) sprintf("%.1f", x)
# 
# panelA_body <- vapply(seq_len(nrow(tab)), function(i) {
#   sprintf("%s & %s & %s & %s \\\\",
#           row_lab[i], fmt_n(tab$All[i]), fmt_n(tab$Roma[i]), fmt_n(tab$Educated[i]))
# }, character(1))
# 
# panelB_body <- vapply(seq_len(nrow(stats)), function(i) {
#   sprintf("%s & %s & %s & %s \\\\",
#           row_lab[i], fmt_pct(stats$All[i]), fmt_pct(stats$Roma[i]), fmt_pct(stats$Educated[i]))
# }, character(1))
# 
# panel_title <- function(txt) sprintf("\\multicolumn{4}{c}{%s} \\\\", txt)

fmt_n   <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) sprintf("%.1f", x)

out <- c(
  "\\begin{tabular}{lcccc}",
  "\\hline",
  "& Linkage & All & Roma & Educated \\\\",
  "\\hline",
  sprintf("\\multirow{2}{*}{Inconsistent Sex (\\%%)} & 1992 - 2011 & %s & %s & %s \\\\",
          fmt_pct(stats$All[1]), fmt_pct(stats$Roma[1]), fmt_pct(stats$Educated[1])),
  sprintf("& 2002 - 2011 & %s & %s & %s \\\\",
          fmt_pct(stats$All[2]), fmt_pct(stats$Roma[2]), fmt_pct(stats$Educated[2])),
  "\\midrule",
  sprintf("\\multirow{2}{*}{Total Unique Matches} & 1992 - 2011 & %s & %s & %s \\\\",
          fmt_n(tab$All[1]), fmt_n(tab$Roma[1]), fmt_n(tab$Educated[1])),
  sprintf("& 2002 - 2011 & %s & %s & %s \\\\",
          fmt_n(tab$All[2]), fmt_n(tab$Roma[2]), fmt_n(tab$Educated[2])),
  "\\hline",
  "\\end{tabular}"
)

setwd(wd_output)
writeLines(out, "Table A01.tex")
