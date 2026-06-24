# =====================================================================
# Appendix Table A13 — Survey ethnic identification and education (2x2 cross-tab)
# Produces:  output/Table A13.tex
# Inputs:    survey_no_priming.csv (no-priming arm), survey_priming.csv (priming arm)
#            (anonymized survey waves, built by 00_anonymize_survey.R)
# Summary:   Among attentive survey respondents, cross-tabulates two beliefs:
#            (a) whether Roma who change their ethnic identification are more
#            educated (from II8), and (b) whether educated Roma change their
#            identification more (from II10). The 2x2 table of counts (with
#            margins) shows whether respondents' two views are internally
#            consistent yet divergent. Emits a LaTeX tabular of counts.
# =====================================================================

# ---- Load and pool the two survey arms ----
# II8/II10 exist only in the priming wave; bind_rows fills NA for no-priming rows.
setwd(wd_data_survey_processed)
data1 <- fread("survey_no_priming.csv") %>% mutate(priming = "No Priming")
data2 <- fread("survey_priming.csv")    %>% mutate(priming = "Priming")
data_master <- bind_rows(data1, data2)

# ---- Keep only attentive respondents ----------------------------------------
# Drop respondents flagged as not paying attention (II1 == 89); II15 == 3 marks
# those who pass the attention check.
data_master <- data_master %>%
 filter(II1 != 89)

# ---- Build the two belief indicators (NA outside {1,2}) ---------------------
# passing_more_educated: from II8, =1 if the respondent thinks Roma who change
#   their ethnic identification are more educated (II8 == 2).
# educated_pass_more: from II10, =1 if the respondent thinks educated Roma
#   change their identification more often (II10 == 1).
# Responses other than 1 or 2 are set to NA.
data_master <- data_master %>%
  mutate(
    passing_more_educated = if_else(II8  %in% c(1, 2), as.integer(II8  == 2), NA_integer_),
    educated_pass_more    = if_else(II10 %in% c(1, 2), as.integer(II10 == 1), NA_integer_)
  )

# ---- Two-way frequency table ------------------------------------------------
tw_data <- data_master %>%
  filter(!is.na(educated_pass_more), !is.na(passing_more_educated))
tw <- table(tw_data$educated_pass_more, tw_data$passing_more_educated)
print(tw)
cat("\nRow percentages:\n")
print(round(100 * prop.table(tw, margin = 1), 1))

# ---- Emit LaTeX tabular (counts, like esttab cells("b") unstack) ------------
# rows    = educated_pass_more (0 = not more, 1 = more-educated pass more)
# columns = passing_more_educated (0 = not more, 1 = passing more educated)
row_lab <- c("0" = "No",
             "1" = "Yes")
col_lab <- c("0" = "No",
             "1" = "Yes")

row_tot <- margin.table(tw, 1)
col_tot <- margin.table(tw, 2)
grand   <- sum(tw)
fmtc    <- function(x) format(x, big.mark = ",")

rn <- rownames(tw)

# row-dimension title sits in the leftmost column header, not rotated
body <- vapply(rn, function(r) {
  sprintf("%s & %s & %s \\\\",
          row_lab[r], paste(fmtc(tw[r, ]), collapse = " & "), fmtc(row_tot[r]))
}, character(1))

total_row <- sprintf("Total & %s & %s \\\\",
                     paste(fmtc(col_tot), collapse = " & "), fmtc(grand))

tab <- c(
  "\\begin{tabular}{lcc c}",
  "\\toprule",
  "& \\multicolumn{2}{c}{\\makecell{Do Educated Roma Change\\\\Their Ethnic Identification?}} & \\\\",
  "\\cmidrule(lr){2-3}",
  "\\makecell[l]{Are Roma Who Change Their Ethnic\\\\Identification More Educated?} & No & Yes & Total \\\\",
  "\\midrule",
  body,
  "\\midrule",
  total_row,
  "\\bottomrule",
  "\\end{tabular}"
)

if (!dir.exists(wd_output)) dir.create(wd_output, recursive = TRUE)
writeLines(tab, file.path(wd_output, "Table A13.tex"))