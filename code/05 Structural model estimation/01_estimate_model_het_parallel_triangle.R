# Structural model bootstrap estimation — Triangle distribution assumption; saves results to data/processed/results/

# --- Initialisation ---

# Container for bootstrap results (populated inside the parallel loop)
results<-list()
# Number of bootstrap iterations per education group
iter<-100
# Constraint matrix for constrOptim: rows 1-4 enforce params <= 1, rows 5-8 enforce params >= 0
# Together these keep the four mixture-weight parameters in [0, 1]
ui<-rbind(-diag(4), diag(4))
# Right-hand side of the linear inequality constraints ui %*% theta >= ci
ci<-c(-1,- 1,-1,-1,0,0,0,0)
# Isolated environment used to pass objects cleanly to worker processes
env <- new.env()

# --- Parallel backend setup ---
#parallel
# Number of parallel workers; kept below total cores to leave headroom for the OS
clusters<-25
# Working directory on workers must point to where structural data files are stored
setwd(wd_data_structural)
cores=detectCores()
# outfile captures worker-side messages/warnings for debugging without cluttering the console
cl <- makeCluster(clusters,outfile="outfile_triangle.txt") #not to overload your computer
registerDoParallel(cl)


# --- Build iteration grid ---




####run loops


# Full factorial of bootstrap draws (j) crossed with education-group IDs (i);
# each row is one unit of parallel work dispatched to a worker
comb <- expand.grid(j=1:iter, i=unique(data_sum$id) )



# Workers source the model likelihood from wd_code, so reset wd before launching
setwd(wd_code)
start_time <- Sys.time()
# Distribute all (education-group, bootstrap-iteration) combinations across workers;
# each call to f(i, j, "triangle") draws a bootstrap sample for group i, iteration j,
# and maximises the mixture-model log-likelihood under the Triangle cost distribution
results_all <-foreach::foreach (k=1:nrow(comb),.combine=bind_rows,.packages=c("tidyverse","triangle")) %dopar% {
  # i: education-group identifier (indexes rows of data_sum)
  i<-comb$i[k]
  # j: bootstrap iteration index (controls set.seed inside f for reproducibility)
  j<-comb$j[k]

  # Estimate model for this group-iteration cell; "triangle" selects the Triangle
  # distributional assumption for the unobserved utility cost of Roma identification
  f(i,j,"triangle")
}
end_time <- Sys.time()
# Elapsed wall-clock time reported to console for performance monitoring
end_time - start_time


# --- Shutdown and save ---
#stop cluster
stopCluster(cl)

# Switch back to structural data directory before saving
setwd(wd_data_structural)
# Output: full bootstrap results matrix (all education groups x 100 iterations) under Triangle assumption
saveRDS(results_all,"results_parallel_triangle.rds")
