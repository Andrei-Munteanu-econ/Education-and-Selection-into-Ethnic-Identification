# =====================================================================
# Shared utility library for the replication package.
# Produces:  sourced by other scripts (defines functions, sets modelsummary options)
# Inputs:    none directly; functions take filenames / data frames as arguments
# Summary:   Provides data-reading helpers (read_sample / read_data), survey
#            summary builders (summarise_c / summarise_long / summarise_d),
#            number-formatting helpers, a modelsummary glance method that adds
#            the dependent-variable mean, and a helper that inserts column
#            numbers into a saved LaTeX table.
# =====================================================================

# ---- Data reading helpers ----

# Read only the first 10,000 rows of a CSV to infer column types / a template
read_sample<-function(filename){
  data<-fread(paste0(filename),nrows=10000,fill=TRUE)
  return(data)
}

# Read the full CSV, reusing the column classes and column selection from a sample template
read_data<-function(filename,sample){

  classes<-sapply(sample, class)
  #classes<-gsub("integer","factor",classes)
  colnames<-colnames(sample)

  setDTthreads(threads=36)
  data<-fread(paste0(filename),colClasses = classes,select=colnames,verbose=F)
  return(data)
}

# ---- Survey summary builders ----

# Summarise continuous survey variables by priming arm: skim-based means/SDs and
# completion rates, returned as a styled kable
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
# Summarise the share of blank ("No Answer") responses for a variable by priming arm
summarise_long<-function(str){
  summary<-data_master %>%
    select(any_of(str),priming) %>%
    group_by(priming) %>%
    summarise(`No Answer`=eval_tidy(parse_expr(paste0("sum(",str,"=='')/n()"))))
  
  summary<-kable(summary,digits=2) %>%
    kable_styling("striped", full_width = F) 
  
  
  return(summary)
}
# Summarise a discrete/labelled survey variable: response-share distribution by
# priming arm plus a two-sample proportion test comparing arms, as a styled kable
summarise_d<-function(str){
  x<-data_master[[str]]
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
    mutate(p=sapply(list(prop.test(c(NoPriming,Priming),c(1000,1000))),"[[","p.value")) %>%
    ungroup %>%
    mutate(Priming=Priming/sum(Priming)*100,NoPriming=NoPriming/sum(NoPriming)*100)
  
  summary<-kable(summary,digits=2) %>%
    kable_styling("striped", full_width = F) 
  
  return(summary)
}

# ---- Number-formatting helpers ----

# Format a number with thousands separators and one decimal place
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
# Format a number rounded to a whole integer, trimmed of leading/trailing spaces
f_dec<-function(x) format(round(x,0), scientific=FALSE, nsmall=0,, trim = TRUE)


# ---- modelsummary options and custom glance ----

# Use plain (non-scientific) numeric formatting in LaTeX output
options(modelsummary_format_numeric_latex = "plain") #latex output
# Custom glance method: adds a "Mean of DV" goodness-of-fit row to fixest regression tables
glance_custom.fixest <- function(x, ...) { #add mean of dependent variable to regression tables
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}


# ---- LaTeX table post-processing ----

# Insert a "(1) (2) ..." column-number row just above the first \midrule of a
# saved LaTeX table, counting data columns from the tabular spec
add_column_numbers <- function(tex_file) {
  lines <- readLines(tex_file)
  # Count data columns from the tabular spec (count c's after the l)
  tab_line <- lines[grep("\\\\begin\\{tabular", lines)[1]]
  col_spec <- regmatches(tab_line, regexpr("\\{[lcr]+\\}", tab_line))
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
  writeLines(lines, tex_file)
}

