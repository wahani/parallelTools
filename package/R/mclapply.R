#' mclapply - Wrapper for parallel::mclapply/clusterApply
#' 
#' @details Arguments are as for parallel::mclapply
#' @export
mclapply <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = 1L,
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE, 
                     packageToLoad = "", sourceFile = "") {
  if(.Platform$OS.type == "windows") {
    clusterFunction <- if(mc.preschedule) clusterApply else clusterApplyLB
    cl <- makeCluster(mc.cores)
    if(all(packageToLoad != "")) {
      clusterExport(cl, "packageToLoad", envir = environment())
      clusterEvalQ(cl, lapply(packageToLoad, require, character.only = TRUE))
    }
    if(sourceFile != "") {
      clusterExport(cl, "sourceFile", envir = environment())
      clusterEvalQ(cl, source(sourceFile, echo=FALSE))
    }
    result <- clusterFunction(cl = cl, x = X, fun = FUN, ...=...)
    stopCluster(cl)
    return(result)
  } else {
    return(parallel::mclapply(X = X, FUN = FUN, ...=..., mc.preschedule = mc.preschedule, 
                              mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores,
                              mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive))
  }
}