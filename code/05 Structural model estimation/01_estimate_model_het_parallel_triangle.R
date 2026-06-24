# =====================================================================
# Bootstrap estimation of the structural model under TRIANGLE heterogeneity.
# Produces:  results_parallel_triangle.rds (data frame of estimates over
#            education groups x bootstrap replicates)
# Inputs:    objects defined by 00_main_structural.R (data, data_sum, f(),
#            model_triangle, S/S1/S2); run after sourcing that script
# Summary:   Runs f(i, j, "triangle") in parallel over every education group i and
#            100 bootstrap replicates j, then saves the combined results.
# =====================================================================

# ---- Estimation settings and parameter constraints ----
results<-list()
iter<-100                       # bootstrap replicates per education group
# Box constraints for constrOptim: only 4 parameters here (pr, d0, d1, d2) because
# the triangular spread is tied to the round means, so there is no separate sigma.
ui<-rbind(-diag(4), diag(4))
ci<-c(-1,- 1,-1,-1,0,0,0,0)
env <- new.env()

# ---- Set up parallel backend ----
#parallel
clusters<-25
setwd(wd_data_structural)
cores=detectCores()
cl <- makeCluster(clusters,outfile="outfile_triangle.txt") #not to overload your computer
registerDoParallel(cl)







# ---- Run estimation over all (group, replicate) pairs in parallel ----
####run loops

# Every combination of bootstrap replicate j and education group i
comb <- expand.grid(j=1:iter, i=unique(data_sum$id) )



setwd(wd_code)
start_time <- Sys.time()
# Estimate the TRIANGLE model for each (i, j) in parallel and row-bind the results
results_all <-foreach::foreach (k=1:nrow(comb),.combine=bind_rows,.packages=c("tidyverse","triangle")) %dopar% {
  i<-comb$i[k]
  j<-comb$j[k]

  f(i,j,"triangle")
}
end_time <- Sys.time()
end_time - start_time


# ---- Tear down cluster and save ----
#stop cluster
stopCluster(cl)

setwd(wd_data_structural)
saveRDS(results_all,"results_parallel_triangle.rds")
