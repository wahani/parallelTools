mclapply <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = 1L,
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE, 
                     packageToLoad = "", sourceFile = "") {
  
  require(parallel)
  if(.Platform$OS.type == "windows") {
    cl <- makeCluster(mc.cores)
    clusterEvalQ(cl, expression(lapply(packageToLoad, library)))
    clusterFunction <- if(mc.preschedule) clusterApply else clusterApplyLB
    result <- clusterFunction(cl = cl, x = X, fun = FUN, ...=...)
    stopCluster(cl)
    return(result)
  } else {
    return(parallel::mclapply(X = X, FUN = FUN, ...=..., mc.preschedule = mc.preschedule, 
                              mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores,
                              mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive))
  }
}

MClapply(lapply(10^1:20, rnorm), function(x) {
  Sys.sleep(10)
  mean(x)
}, mc.cores = detectCores())