#' parallel version of lapply windows/linux
#' 
#' @description This function masks the function mclapply from package parallel. The aim is
#' to use multicore functionality under windows and linux. For the description of the function
#' arguments see: \link[parallel]{mclapply} in the package parallel
#' 
#' @inheritParams parallel::mclapply
#' @param mc.preschedule see \code{\link[parallel]{mclapply}} for Linux. Windows: If TRUE \code{\link{clusterApply}}
#' will be used, if FALSE \code{\link{clusterApplyLB}} is used
#' @param mc.cores number of cores to be used.
#' 
#' @details The function will detect th operating system via \code{.Platform$OS.type}. If the platform is equal
#' to "windows", \code{mc.cores} clusters will be initialized using \code{\link{makeCluster}}. Packages and sourcefiles
#' will be loaded onto each cluster using \code{\link{clusterEvalQ}}. Then either \code{\link{clusterApply}}
#' or \code{\link{clusterApplyLB}} is called with the arguments \code{x = X, fun = FUN, ... = ...} - \code{mc.set.seed},
#' \code{mc.silent}, \code{mc.cleanup} and \code{mc.allow.recursive} will be ignored under windows. The function will end with 
#' \code{\link{stopCluster}} to end all started sessions and return the resulting list.
#' 
#' If \code{.Platform$OS.type} is not equal to windows the function \code{\link[parallel]{mclapply}} will be called, passing
#' all arguments but \code{packageToLoad} and \code{sourceFile} which will be ignored.
#' 
#' This function is written to use multicore functionality in developing mainly for linux but with the ability to test
#' the code in windows. The side effects of the applied parallelization for windows I do not know. Check the running 
#' processes regularly.
#' 
#' @seealso Everything in the package "parallel": \link{clusterApply} and \link[parallel]{mclapply} and \link{mcmapply}
#' @author Sebastian Warnholz
#' @examples 
#' library(Matrix)
#' 
#' setPTOption("Matrix")
#' getPTOption()
#' 
#' genData <- function(n) {
#'  # Generate some Data
#'  dat <- data.frame()
#'  for (i in 1:n) {
#'    dat <- rbind(dat, data.frame(colA = letters[sample(26)], colB = letters[sample(26)]))
#'  }
#'  dat
#' }
#'
#' slowFunction <- function(dat) {
#'  # A slow function
#'  dat$ind <- logical(nrow(dat))
#'  
#'    # call something from some package:
#'    Matrix(rnorm(10), 2)

#'  for (i in 1:nrow(dat)) {
#'    dat$ind[i] <- grepl(dat$colA[i], dat$colB[i])
#'  }
#'  
#'  dat
#' }
#'
#' nCores <- detectCores() - 1 # Detects the number of cores
#' dat <- genData(1000) # generate some Date
#' dat$sample <- rep_len(1:nCores, length.out = nrow(dat)) # ID for splitting the data
#' dat$id <- 1:nrow(dat) # ID for observations
#' dataList <- split(dat, dat$sample) # split the dataSet for processing the tasks parallel
#'
#' system.time(result1 <- slowFunction(dat)) # Function without mc
#'
#' # Function with mc - should work for all platforms
#' system.time(result2 <- do.call("rbind", mclapply(X = dataList, FUN = slowFunction, mc.cores = nCores)))
#'
#' # Better than mc is vecorization (in this case) - no multicore required:
#' system.time(result3 <- mapply(grepl, as.list(dat$colA), as.list(dat$colB)))
#'
#' #All the same
#' all((result1$ind == result2$ind[order(result2$id)]) & result1$ind == result3)
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
    
    result <- clusterFunction(cl = cl, x = X, fun = FUN, ...=...)
    
    return(result)
    
  } else {
    
    # For else
    return(parallel::mclapply(X = X, FUN = FUN, ...=..., mc.preschedule = mc.preschedule, 
                              mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores,
                              mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive))
  }
}