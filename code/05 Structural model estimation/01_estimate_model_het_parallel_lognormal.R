# Structural model bootstrap estimation — Lognormal distribution assumption; saves results to data/processed/results/

# --- Initialisation ---

# Container for per-iteration results (populated inside the parallel loop)
results<-list()
# iter: number of bootstrap resamples per education group
iter<-100
# ui, ci: linear inequality constraints passed to constrOptim() inside f().
# The first 5 rows enforce parameters <= their upper bounds; the last 5 enforce >= their lower bounds.
# Bounds correspond to the 5 structural parameters: (sigma, beta coefficients, cost parameters).
ui<-rbind(-diag(5), diag(5))
# ci: right-hand side of the constraint ui %*% theta >= ci (i.e., params bounded in [-1,0],[0,-4],[-4,-4],[-4,0])
ci<-c(-1,0,0,0,-3,0,-4,-4,-4,0)
# Scratch environment used to pass objects into the parallel workers without polluting global scope
env <- new.env()

# --- Parallel cluster setup ---
#parallel
# clusters: initial placeholder; overwritten below based on available cores
clusters<-20
# wd_data_structural: project-level path constant defined upstream (e.g., in 00_setup.R)
setwd(wd_data_structural)
cores=detectCores()
# Reserve 4 cores for the OS and other processes to avoid machine saturation
clusters=cores-4
# outfile captures worker stdout/stderr for debugging without blocking the master process
cl <- makeCluster(clusters,outfile="outfile_lognormal.txt") #not to overload your computer
registerDoParallel(cl)


# --- Bootstrap grid construction ---




####run loops


# comb: full factorial grid of (bootstrap iteration j, education group id i).
# Each row is one unit of work dispatched to a parallel worker.
# data_sum$id contains the distinct education-group identifiers (defined upstream).
comb <- expand.grid(j=1:iter, i=unique(data_sum$id) )



# Switch to code directory so worker processes can source helper scripts if needed
setwd(wd_code)
# --- Parallel bootstrap loop ---
start_time <- Sys.time()
# foreach iterates over every (i, j) combination in comb; bind_rows stacks the per-row results.
# .packages ensures tidyverse is loaded on each worker (f() uses dplyr/tidyr internals).
results_all <-foreach::foreach (k=1:nrow(comb),.combine=bind_rows,.packages="tidyverse") %dopar% {
  # i: education group identifier for this task
  i<-comb$i[k]
  # j: bootstrap iteration index for this task; used as seed inside f() for reproducibility
  j<-comb$j[k]

  # f(): core structural estimation function — fits the mixture model for education group i,
  # bootstrap resample j, under a Lognormal distribution for the cost heterogeneity parameter.
  # S, S1, S2 are pre-computed sufficient statistics / data objects passed from the master environment.
  f(i,j,"lognormal",S,S1,S2)
}
end_time <- Sys.time()
# Print elapsed wall-clock time as a diagnostic for the parallel run
end_time - start_time


# --- Save results ---
#stop cluster
stopCluster(cl)

# Return to structural data directory before writing output
setwd(wd_data_structural)
# Output: full bootstrap results table (one row per (group, iteration)) under the Lognormal assumption
saveRDS(results_all,"results_parallel_lognormal.rds")
