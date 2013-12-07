#' Parallel version of lapply
#' 
#' @description This function masks the function mclapply from package parallel. The aim is
#' to use multicore functionality under windows and linux using the same function call.
#' 
#' @inheritParams parallel::mclapply
#' @param mc.preschedule see \code{\link[parallel]{mclapply}} for Linux. Windows: If TRUE \code{\link{clusterApply}}
#' will be used, if FALSE \code{\link{clusterApplyLB}} is used
#' @param mc.cores number of cores to be used.
#' 
#' @details The function will detect th operating system via \code{.Platform$OS.type}. If the platform is equal
#' to "windows", \code{mc.cores} clusters will be initialized using \code{\link{makeCluster}}. Packages and sourcefiles
#' will be loaded onto each cluster using \code{\link{clusterEvalQ}}. Then either \code{\link{clusterApply}}
#' or \code{\link{clusterApplyLB}} is called with the arguments \code{x = X, fun = FUN, ... = ...} - the arguments \code{mc.set.seed},
#' \code{mc.silent}, \code{mc.cleanup} and \code{mc.allow.recursive} will be ignored under windows. The function will end with 
#' \code{\link{stopCluster}} to end all started sessions and return the resulting list.
#' 
#' If \code{.Platform$OS.type} is not equal to windows the function \code{\link[parallel]{mclapply}} will be called, passing
#' all arguments but \code{packageToLoad} and \code{sourceFile} which will be ignored.
#' 
#' This function is written to use multicore functionality in developing mainly for linux but with the ability to test
#' the code in windows. The function is written such that it is not necessary to use the package 
#' running linux since in this case it is a wrapper for \code{\link[parallel]{mclapply}}.
#' 
#' @return See the documentation for \code{\link{clusterApply}} when running Windows or
#' \code{\link[parallel]{mclapply}} otherwise.
#' 
#' @seealso \code{\link{clusterApply}}, \code{\link[parallel]{mclapply}}
#' @author Sebastian Warnholz
#' @examples
#' # Examples from parallel::mclapply and parallel::clusterApply:
#' simplify2array(mclapply(rep(4, 5), rnorm, mc.cores = 2))
#' mclapply(1:2, get("+"), 3, mc.cores = 2)
#' 
#' ## Not run: ## Set up each worker, relevant for Windows, see the examples
#' # for clusterApply for further details:
#' setPTOption(packageToLoad = "parallel") # load the package 'parallel' on each worker.
#' setPTOption(sourceFile = "workerSetupScript.R") # run the script on each worker.
#' ## End(Not run)
#' 
#' @export
mclapply <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = 1L,
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE) {
  
  # For Windows
  if(.Platform$OS.type == "windows") {
    
    clusterFunction <- if(mc.preschedule) clusterApply else clusterApplyLB
    cl <- makeCluster(mc.cores)
    on.exit(stopCluster(cl))
    ptOptions <- getPTOption()
    packageToLoad <- if(is.null(ptOptions)) "" else ptOptions$parallelToolsPTL
    sourceFile <- if(is.null(ptOptions)) "" else ptOptions$parallelToolsSF
    
    # load packages
    if(all(packageToLoad != "")) {
      clusterExport(cl, "packageToLoad", envir = environment())
      clusterEvalQ(cl, lapply(packageToLoad, require, character.only = TRUE))
    }
    
    # source
    if(sourceFile != "") {
      clusterExport(cl, "sourceFile", envir = environment())
      clusterEvalQ(cl, source(sourceFile, echo=FALSE))
    }
    
    return(clusterFunction(cl = cl, x = X, fun = FUN, ...=...))
        
  } else {
    
    # For else
    return(parallel::mclapply(X = X, FUN = FUN, ...=..., mc.preschedule = mc.preschedule, 
                              mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores,
                              mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive))
  }
}