# Produces Table A.13: Respondent Views Internally Consistent but Divergent

# --- Load Survey Data ---
# The survey was administered in two versions that differ only by ethnic-priming treatment;
# both files are loaded and stacked. The priming variable records which treatment arm
# each respondent belongs to, enabling heterogeneity checks if needed.
#load survey data
setwd(wd_data_survey)
# data1: no-priming arm; respondents answered without a prior question activating Roma identity
data1 <- read_sav("baza_Link1.sav") %>% mutate(priming = "No Priming")
# data2: priming arm; respondents first answered a question designed to activate Roma salience
data2 <- read_sav("baza_Link2.sav") %>% mutate(priming = "Priming")
# Pool both survey arms into a single data frame for joint cross-tabulation
data_master <- bind_rows(data1, data2)

# ---- Keep only attentive respondents ----------------------------------------
# II1 == 89  -> "not paying attention" flag (dropped)
# II15 == 3  -> passes the attention check (kept)
# Dropping inattentive respondents ensures the two survey items reflect genuine beliefs
data_master <- data_master %>%
 filter(II1 != 89)

# ---- Build the two indicators (NA outside {1,2}) ----------------------------
# II8:  "Are Roma who change their ethnic identification more educated than average Roma?"
#        1 = No, 2 = Yes  ->  passing_more_educated = 1 if respondent says Yes
# II10: "Do more-educated Roma change their ethnic identification more often?"
#        1 = Yes, 2 = No  ->  educated_pass_more = 1 if respondent says Yes
# Responses outside {1,2} (e.g., "don't know", refused) are treated as missing
# to avoid conflating non-response with a substantive answer.
# Internal consistency check: a respondent who answers Yes to both is coherent;
# divergence between the two items reveals directional asymmetry in beliefs.
data_master <- data_master %>%
  mutate(
    passing_more_educated = if_else(II8  %in% c(1, 2), as.integer(II8  == 2), NA_integer_),
    educated_pass_more    = if_else(II10 %in% c(1, 2), as.integer(II10 == 1), NA_integer_)
  )

# ---- Two-way frequency table ------------------------------------------------
# Restrict to respondents with valid answers on both items so the joint distribution
# is well-defined; this is the analytic sample for Table A.13.
tw_data <- data_master %>%
  filter(!is.na(educated_pass_more), !is.na(passing_more_educated))
# tw: rows = educated_pass_more, columns = passing_more_educated
tw <- table(tw_data$educated_pass_more, tw_data$passing_more_educated)
print(tw)
cat("\nRow percentages:\n")
# Row percentages show, conditional on believing educated Roma pass more (or not),
# what share also believes those who pass are more educated
print(round(100 * prop.table(tw, margin = 1), 1))

# ---- Emit LaTeX tabular (counts, like esttab cells("b") unstack) ------------
# rows    = educated_pass_more (0 = not more, 1 = more-educated pass more)
# columns = passing_more_educated (0 = not more, 1 = passing more educated)
# Human-readable labels for the binary indicators (0 = No, 1 = Yes)
row_lab <- c("0" = "No",
             "1" = "Yes")
col_lab <- c("0" = "No",
             "1" = "Yes")

# Marginal totals and grand total used for the "Total" row and column
row_tot <- margin.table(tw, 1)
col_tot <- margin.table(tw, 2)
grand   <- sum(tw)
# fmtc: helper that formats integers with thousands-separator commas for readability
fmtc    <- function(x) format(x, big.mark = ",")

rn <- rownames(tw)

# row-dimension title sits in the leftmost column header, not rotated
# Construct one LaTeX data row per level of educated_pass_more
body <- vapply(rn, function(r) {
  sprintf("%s & %s & %s \\\\",
          row_lab[r], paste(fmtc(tw[r, ]), collapse = " & "), fmtc(row_tot[r]))
}, character(1))

# Marginal totals row at the bottom of the tabular body
total_row <- sprintf("Total & %s & %s \\\\",
                     paste(fmtc(col_tot), collapse = " & "), fmtc(grand))

# Assemble the full LaTeX tabular environment; column headers use \makecell for line breaks
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

# Output: writes Table A13.tex to the designated output directory
if (!dir.exists(wd_output)) dir.create(wd_output, recursive = TRUE)
writeLines(tab, file.path(wd_output, "Table A13.tex"))
