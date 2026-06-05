# Produces Table A.1: Census Linkages
# --- Load Linked Census Data ---
#estimate match rates using genderless matches----
# Genderless matching cells (SIRSUP + AA + ZZ, without LL) are used to avoid
# mechanical gender-ratio bias that arises when sex is part of the cell definition.
setwd(wd_data_linked)
# data_2002_2011: linked records for individuals matched across 2002 and 2011 censuses
data_2002_2011<-fread('data_2002_2011_unique_genderless.csv')
# data_1992_2011: linked records for individuals matched across 1992 and 2011 censuses
data_1992_2011<-fread('data_1992_2011_unique_genderless.csv')

# tab: match counts by subgroup, used in Panel B of the table
# Rows = linkage pair (1992-2011, 2002-2011); columns = All / Roma / Educated (years_2011 > 8)
tab<-data.frame("All"=c(nrow(data_1992_2011),nrow(data_2002_2011)),
                "Roma"=c(nrow(data_1992_2011 %>% filter(ROMA_1992==T)),
                         nrow(data_2002_2011 %>% filter(ROMA_2002==T))),
                # years_2011 > 8 corresponds to more than primary schooling (above Gym level)
                "Educated"=c(nrow(data_1992_2011 %>% filter(years_2011>8)),
                         nrow(data_2002_2011 %>% filter(years_2011>8)))
                )

# --- Compute Gender-Mismatch Rates (Full Sample) ---
# Strategy: records linked via genderless cells that disagree on sex across census waves
# are almost certainly false matches. The share of such records (p) estimates p_mismatch.
#Stats
# stats_2002: sex-mismatch rate in the 2002-2011 linkage, full sample
stats_2002<-data_2002_2011 %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  group_by(mismatch) %>%
  summarise(n=n()) %>%
  # p = share of all matched records where reported sex differs across waves
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

# stats_1992: sex-mismatch rate in the 1992-2011 linkage, full sample
stats_1992<-data_1992_2011 %>%
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  group_by(mismatch) %>%
  summarise(n=n())%>%
  mutate(p=n/sum(n),n=sum(n)) %>%
  filter(mismatch==T)

# --- Compute Gender-Mismatch Rates (Roma Subsample) ---
# Mismatch rates among Roma-identified individuals; used to check whether
# Roma records are differentially prone to false matches relative to the full sample.
#Stats Roma
stats_2002_roma<-data_2002_2011 %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  # restrict to Roma-identified individuals in the earlier census year
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

# --- Compute Gender-Mismatch Rates (Educated Subsample) ---
# Mismatch rates among individuals with years_2011 > 8 (above primary/gym threshold).
# Relevant because the IV analysis focuses on educated Roma; higher mismatch here
# would bias the main estimates more severely.
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

# --- Compute Gender-Mismatch Rates (Educated Roma Subsample) ---
# The most policy-relevant subgroup: Roma individuals with at least primary education
# (years >= 4) in the earlier census. Used as a robustness / diagnostic check.
#Stats Educ Roma
stats_2002_test<-data_2002_2011 %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  # years_2002 >= 4: at least completed primary education in 2002
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

# stats<-data.frame(All=c(stats_1992$p,stats_2002$p)*100,
#                   Roma=c(stats_1992_roma$p,stats_2002_roma$p)*100,
#                   `Educated`=c(stats_1992_educ$p,stats_2002_educ$p)*100)
#
#
# setwd(wd_output)
# fileConn<-file("03_table_3_match_rates.txt")
# writeLines(print(xtable(stats, type = "latex",digits=1),
#                  include.rownames=FALSE,
#                  format.args=list(big.mark = ","), suffix = "%")
#            , fileConn)
# close(fileConn)
#
# setwd(wd_output)
# fileConn<-file("03_table_3_match_numbers.txt")
# writeLines(print(xtable(tab, type = "latex",digits=1),
#                  include.rownames=FALSE,
#                  format.args=list(big.mark = ","), suffix = "%")
#            , fileConn)
# close(fileConn)
#
#
#
#
#
#

# ---- assemble the two panels ------------------------------------------------
# Panel A: match counts ; Panel B: gender-mismatch rates (%)
# Both share columns All / Roma / Educated and rows 1992-2011 / 2002-2011.

# row_lab: LaTeX-escaped labels for the two census linkage pairs
row_lab <- c("1992--2011", "2002--2011")

# tab: rebuilt (cleaner spacing) version of the match-count data frame above
tab <- data.frame(
  All      = c(nrow(data_1992_2011), nrow(data_2002_2011)),
  Roma     = c(nrow(data_1992_2011 %>% filter(ROMA_1992 == T)),
               nrow(data_2002_2011 %>% filter(ROMA_2002 == T))),
  Educated = c(nrow(data_1992_2011 %>% filter(years_2011 > 8)),
               nrow(data_2002_2011 %>% filter(years_2011 > 8)))
)

# stats: gender-mismatch rates (%) by subgroup, multiplied by 100 for display
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

# --- Format Helpers ---
# fmt_n: integer counts formatted with thousands separator for readability
fmt_n   <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
# fmt_pct: one decimal place percentage string (e.g. "2.3")
fmt_pct <- function(x) sprintf("%.1f", x)

# --- Assemble LaTeX Table Body ---
# Hand-built tabular environment so that \multirow spans both linkage rows
# within each panel, matching the journal's two-panel layout exactly.
out <- c(
  "\\begin{tabular}{lcccc}",
  "\\hline",
  "& Linkage & All & Roma & Educated \\\\",
  "\\hline",
  # Panel A: gender-mismatch rate (%) — proxy for the false-match rate p_mismatch
  sprintf("\\multirow{2}{*}{Inconsistent Sex (\\%%)} & 1992 - 2011 & %s & %s & %s \\\\",
          fmt_pct(stats$All[1]), fmt_pct(stats$Roma[1]), fmt_pct(stats$Educated[1])),
  sprintf("& 2002 - 2011 & %s & %s & %s \\\\",
          fmt_pct(stats$All[2]), fmt_pct(stats$Roma[2]), fmt_pct(stats$Educated[2])),
  "\\midrule",
  # Panel B: total unique matched records — denominator for all pass-rate calculations
  sprintf("\\multirow{2}{*}{Total Unique Matches} & 1992 - 2011 & %s & %s & %s \\\\",
          fmt_n(tab$All[1]), fmt_n(tab$Roma[1]), fmt_n(tab$Educated[1])),
  sprintf("& 2002 - 2011 & %s & %s & %s \\\\",
          fmt_n(tab$All[2]), fmt_n(tab$Roma[2]), fmt_n(tab$Educated[2])),
  "\\hline",
  "\\end{tabular}"
)

# --- Write Output ---
# Output: output/Table A01.tex — Table A.1 in the paper appendix
setwd(wd_output)
writeLines(out, "Table A01.tex")
