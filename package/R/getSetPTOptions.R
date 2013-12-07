#' get/set packages/sourceFiles to be loaded on workers
#' @description get and set the packages and source files which will be loaded on to each worker. 
#' Only relevant in Windows when the function applied in \code{\link[parallelTools]{mclapply}} depends
#' on some package or objects which can be sourced.
#' 
#' @param packageToLoad these two arguments are only relevant using Windows. packageToLoad is a
#' character vector with the names of the packages which should be loaded using \code{\link{require}} on to each
#' worker. In addition with \code{sourceFile} a R-Script can be specified which will be sourced on each worker.
#' @param sourceFile see \code{packageToLoad}
#' 
#' @details The functions will set or get the 'parallelToolsPTL' or 'parallelToolsSF'
#' fields in \code{\link{.Options}}. Running in windows, the function \code{\link[parallelTools]{mclapply}}
#' will check if these fields contain any package names or source files and if so, will load
#' them onto each worker where the cluster is created by \code{\link{makeCluster}}.
#' @seealso \link[parallelTools]{mclapply}, \code{\link{makeCluster}}
#' @author Sebastian Warnholz
#' @rdname getSetPTOptions
#' @export
getPTOption <- function() {
  ptOptions <- .Options[grepl("parallelTools", names(.Options))]
  if(length(ptOptions) == 0) {
    cat("No parrallelTools options set\n")
    invisible(NULL)
  } else {
    cat("packages that will be loaded on each cluster:", 
        if(ptOptions$parallelToolsPTL == "") "\"nothing\"" else ptOptions$parallelToolsPTL,
        "\nsource files sourced on each cluster:", 
        if(ptOptions$parallelToolsSF == "") "\"nothing\"" else ptOptions$parallelToolsSF)
    invisible(ptOptions)
  }
}

#' @rdname getSetPTOptions
#' @export
setPTOption <- function(packageToLoad = "", sourceFile = "") {
  options(parallelToolsPTL = packageToLoad, parallelToolsSF = sourceFile)
}
