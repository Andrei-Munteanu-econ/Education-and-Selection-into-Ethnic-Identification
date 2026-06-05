# Structural model bootstrap estimation — Normal distribution assumption; saves results to data/processed/results/

# --- Initialisation ---

# Container for bootstrap results (populated inside the parallel loop)
results<-list()
# iter: number of bootstrap iterations per education group
iter<-100
# ui, ci: linear inequality constraints passed to constrOptim() inside f().
# Together they enforce [0,1] bounds on the five probability parameters and
# a lower bound of 0 on the cost-distribution mean (param 5 >= 0).
ui<-rbind(-diag(5), diag(5))
ci<-c(-1,- 1,-1,-1,-2,0,0,0,0,0)
# Separate environment used to isolate objects exported to worker nodes
env <- new.env()

# --- Parallel backend setup ---
#parallel
# clusters: number of parallel workers; tune to available hardware
clusters<-20
# Switch to the structural-model data directory before spawning workers
# so that each worker inherits the correct working directory
setwd(wd_data_structural)
cores=detectCores()
# outfile="outfile.txt" redirects worker stdout/stderr for debugging
cl <- makeCluster(clusters,outfile="outfile.txt") #not to overload your computer
registerDoParallel(cl)


# --- Build the iteration grid ---



####run loops

# comb: full factorial grid of (bootstrap draw j, education-group id i);
# each row is one unit of work dispatched to a parallel worker
comb <- expand.grid(j=1:iter, i=unique(data_sum$id) )


# Switch back to the code directory so workers resolve source() calls correctly
setwd(wd_code)
# Record wall-clock start time to benchmark total parallel runtime
start_time <- Sys.time()
# Dispatch all (i, j) combinations in parallel; each call to f() fits the
# mixture model for education group i on bootstrap resample j under the
# Normal cost-heterogeneity assumption and returns a single-row tibble of estimates
results_all <-foreach::foreach (k=1:nrow(comb),.combine=bind_rows,.packages="tidyverse") %dopar% {
  # i: education-group identifier drawn from data_sum$id
  i<-comb$i[k]
  # j: bootstrap iteration index (1..iter); f() uses set.seed(j) for reproducibility
  j<-comb$j[k]
  # f() is defined in a sourced helper script; arguments are (group id, bootstrap seed, distribution family)
  f(i,j,"normal")
}
end_time <- Sys.time()
# Print elapsed time for the full parallel run
end_time - start_time


#stop cluster
stopCluster(cl)

# --- Save results ---
setwd(wd_data_structural)
# Output: all bootstrap estimates (one row per group × iteration) under the Normal assumption
saveRDS(results_all,"results_parallel.rds")
