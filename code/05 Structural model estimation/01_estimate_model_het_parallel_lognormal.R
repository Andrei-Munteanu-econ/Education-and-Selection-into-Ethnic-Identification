# =====================================================================
# Bootstrap estimation of the structural model under LOGNORMAL heterogeneity.
# Produces:  results_parallel_lognormal.rds (data frame of estimates over
#            education groups x bootstrap replicates)
# Inputs:    objects defined by 00_main_structural.R (data, data_sum, f(),
#            model_lognormal, S/S1/S2); run after sourcing that script
# Summary:   Runs f(i, j, "lognormal") in parallel over every education group i and
#            100 bootstrap replicates j, then saves the combined results.
# =====================================================================

# ---- Estimation settings and parameter constraints ----
results<-list()
iter<-100                       # bootstrap replicates per education group
# Box constraints for constrOptim: 5 parameters (pr, d0, d1, d2, sigma).
# Bounds differ from the Normal/Uniform runs because the lognormal d-means live on
# the log scale (so d0/d1/d2 are unbounded below; ci allows them down to -4).
ui<-rbind(-diag(5), diag(5))
ci<-c(-1,0,0,0,-3,0,-4,-4,-4,0)
env <- new.env()

# ---- Set up parallel backend ----
#parallel
clusters<-20
setwd(wd_data_structural)
cores=detectCores()
clusters=cores-4                # leave 4 cores free
cl <- makeCluster(clusters,outfile="outfile_lognormal.txt") #not to overload your computer
registerDoParallel(cl)







# ---- Run estimation over all (group, replicate) pairs in parallel ----
####run loops

# Every combination of bootstrap replicate j and education group i
comb <- expand.grid(j=1:iter, i=unique(data_sum$id) )



setwd(wd_code)
start_time <- Sys.time()
# Estimate the LOGNORMAL model for each (i, j) in parallel and row-bind the results
results_all <-foreach::foreach (k=1:nrow(comb),.combine=bind_rows,.packages="tidyverse") %dopar% {
  i<-comb$i[k]
  j<-comb$j[k]

  f(i,j,"lognormal",S,S1,S2)
}
end_time <- Sys.time()
end_time - start_time


# ---- Tear down cluster and save ----
#stop cluster
stopCluster(cl)

setwd(wd_data_structural)
saveRDS(results_all,"results_parallel_lognormal.rds")
