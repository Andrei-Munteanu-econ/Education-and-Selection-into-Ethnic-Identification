# Structural model bootstrap estimation — Uniform distribution assumption; saves results to data/processed/results/

# --- Initialisation ---

# results: placeholder list (not used for accumulation here; final output is results_all via foreach)
results<-list()
# iter: number of bootstrap replications per education-group cell
iter<-100
# ui, ci: linear inequality constraints passed to constrOptim() inside f().
# The first 5 rows enforce parameters >= lower bounds; the next 5 enforce <= upper bounds.
# Columns correspond to the 5 structural parameters being estimated.
ui<-rbind(-diag(5), diag(5))
# ci: right-hand side of the constraint ui %*% theta >= ci.
# Lower bounds: all params >= 0 (first four) and the second distributional param >= 0 (fifth slot mapped to -2 as placeholder).
# Upper bounds: first four params <= 1; last param unconstrained above 0.
ci<-c(-1,- 1,-1,-1,-2,0,0,0,0,0)
# env: scratch environment used inside helper functions to avoid namespace collisions
env <- new.env()

# --- Parallel backend setup ---
#parallel
# clusters: number of parallel workers; set below the physical core count to avoid overloading
clusters<-20
# Change working directory to the structural data folder so relative file paths inside workers resolve correctly
setwd(wd_data_structural)
cores=detectCores()
# Spin up the parallel socket cluster; outfile captures worker-side output for debugging
cl <- makeCluster(clusters,outfile="outfile.txt") #not to overload your computer
registerDoParallel(cl)


# --- Bootstrap grid construction ---




####run loops


# comb: full factorial grid of (bootstrap iteration j, education-group id i).
# Each row defines one unit of work dispatched to a worker.
# data_sum$id contains the unique education-group identifiers over which the structural model is estimated.
comb <- expand.grid(j=1:iter, i=unique(data_sum$id) )



# Switch back to the code directory so sourced helper scripts are found by workers
setwd(wd_code)
# --- Parallel bootstrap execution (Uniform distribution) ---
start_time <- Sys.time()
# Dispatch all (i, j) combinations in parallel.
# f(i, j, "uniform") fits the structural mixture model for education group i,
# bootstrap draw j, assuming costs are drawn from a Uniform distribution.
# bind_rows combines one-row result tibbles returned by each worker into a flat data frame.
results_all <-foreach::foreach (k=1:nrow(comb),.combine=bind_rows,.packages="tidyverse") %dopar% {
  i<-comb$i[k]
  j<-comb$j[k]

  f(i,j,"uniform")
}
end_time <- Sys.time()
# Print elapsed wall-clock time for the full parallel run
end_time - start_time


# --- Shutdown and save ---
#stop cluster
stopCluster(cl)

# Switch to the structural data folder to write output
setwd(wd_data_structural)
# Output: bootstrap estimates under the Uniform cost distribution, one row per (education group, bootstrap draw)
saveRDS(results_all,"results_uniform.rds")
