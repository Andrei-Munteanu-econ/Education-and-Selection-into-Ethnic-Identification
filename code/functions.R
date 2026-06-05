# --- Data I/O Helpers ---

# Quick probe of a large raw census file: read only the first 10 000 rows so
# that downstream code can inspect column names and infer classes cheaply.
read_sample<-function(filename){
  data<-fread(paste0(filename),nrows=10000,fill=TRUE)
  return(data)
}

# Full read using the column set and classes already determined from read_sample().
# Restricting to `select=colnames` avoids loading census columns that are never used;
# 36 threads matches the server core count for this project.
read_data<-function(filename,sample){

  # Inherit column classes from the sample so fread coerces types consistently
  classes<-sapply(sample, class)
  #classes<-gsub("integer","factor",classes)
  # Inherit column names from the sample to select only the needed subset
  colnames<-colnames(sample)

  setDTthreads(threads=36)
  data<-fread(paste0(filename),colClasses = classes,select=colnames,verbose=F)
  return(data)
}

# --- Survey Summary Helpers (legacy / exploratory) ---

#summarize survey variables
#summary
# Produces a grouped skim table split by priming condition (used in earlier survey analysis).
summarise_c<-function(str){
  summary<-data_master %>%
    select(any_of(str),priming) %>%
    group_by(priming) %>%
    skim %>%
    select(-skim_type,-skim_variable) %>%
    filter(complete_rate!=0) %>%
    select(-numeric.p0,-numeric.p25,	-numeric.p50,	-numeric.p75,	-numeric.p100) %>%
    rename(mean=numeric.mean,sd=numeric.sd) %>%
    mutate(complete_rate=complete_rate*100)



  summary<-kable(summary,digits=2) %>%
    kable_styling("striped", full_width = F)

  return(summary)
}
# Summarises item non-response (blank open-text answers) by priming condition.
summarise_long<-function(str){
  summary<-data_master %>%
    select(any_of(str),priming) %>%
    group_by(priming) %>%
    summarise(`No Answer`=eval_tidy(parse_expr(paste0("sum(",str,"=='')/n()"))))

  summary<-kable(summary,digits=2) %>%
    kable_styling("striped", full_width = F)


  return(summary)
}
# Summarises a discrete (labelled) survey variable by priming condition,
# running a two-proportion z-test (prop.test) to flag statistically significant
# differences in response shares across the two conditions.
summarise_d<-function(str){
  x<-data_master[[str]]
  # Retrieve value labels stored as a named integer vector attribute (haven/labelled format)
  label<-stack(attr(x, 'labels')) %>%
    rename(vars=values,answer=ind)
  # label<-label %>%
  #   mutate(answer=factor(labs,levels=unique(label[order(label$vars),"labs"]), ordered=TRUE))

  summary<-data_master %>%
    select(matches("priming") | matches(str)) %>%
    left_join(label,by=setNames("vars", str)) %>%
    group_by(answer,priming) %>%
    summarise(p=n()) %>%
    group_by(priming) %>%
    # mutate(p=p/sum(p)) %>%
    pivot_wider(names_from=priming,id_cols=answer,values_from = p) %>%
    rowwise() %>%
    mutate(NoPriming=replace_na(NoPriming,0),
           Priming=replace_na(Priming,0)) %>%
    # Two-sided proportion test comparing primed vs. unprimed response share for each answer category
    mutate(p=sapply(list(prop.test(c(NoPriming,Priming),c(1000,1000))),"[[","p.value")) %>%
    ungroup %>%
    mutate(Priming=Priming/sum(Priming)*100,NoPriming=NoPriming/sum(NoPriming)*100)

  summary<-kable(summary,digits=2) %>%
    kable_styling("striped", full_width = F)

  return(summary)
}

# --- Number Formatting Helpers ---

#formatting
# f_big: formats large integers with thousands separator (e.g. population counts in descriptive tables)
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
# f_dec: rounds to integer and trims whitespace (used for inline-text counts and percentages)
f_dec<-function(x) format(round(x,0), scientific=FALSE, nsmall=0,, trim = TRUE)


# --- modelsummary / fixest Integration ---

#modelsummary options
options(modelsummary_format_numeric_latex = "plain") #latex output
# glance_custom.fixest: appends the mean of the dependent variable as a GOF row in
# modelsummary() tables. Reconstructing DV as fitted + residual is necessary because
# fixest does not store the raw outcome vector directly on the model object.
glance_custom.fixest <- function(x, ...) { #add mean of dependent variable to regression tables
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  # Recover the dependent variable mean from the OLS identity: y = yhat + e
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}


# --- LaTeX Table Post-processing ---

# Inserts a column-number row "(1) & (2) & ..." immediately above the first \midrule
# of a modelsummary-generated .tex file. This matches journal conventions where
# each regression column is numbered for cross-referencing in the text.
add_column_numbers <- function(tex_file) {
  lines <- readLines(tex_file)
  # Count data columns from the tabular spec (count c's after the l)
  tab_line <- lines[grep("\\\\begin\\{tabular", lines)[1]]
  col_spec <- regmatches(tab_line, regexpr("\\{[lcr]+\\}", tab_line))
  # n_cols: number of data columns (excludes the left-hand row-label column)
  n_cols <- nchar(gsub("[^cr]", "", col_spec))  # count c and r columns
  if (n_cols == 0) return(invisible(NULL))
  # Find the first \midrule — column numbers go just before it
  midrule_idx <- which(grepl("^\\\\midrule", lines))[1]
  if (is.na(midrule_idx)) return(invisible(NULL))
  # Build column number row
  nums <- paste0("(", seq_len(n_cols), ")", collapse = " & ")
  num_row <- paste0("  & ", nums, "\\\\")
  # Insert before the first \midrule
  lines <- append(lines, num_row, after = midrule_idx - 1)
  # Output: overwrites the .tex file in place with the column-number row added
  writeLines(lines, tex_file)
}

