#' get/set packages/sourceFiles to be loaded for Windows
#' @description get and set the packages and source files which will be loaded on to each cluster. 
#' Only relevant in Windows when the function applied in \code{\link[parallelTools]{mclapply}} depends
#' on some package or objects which can be sourced.
#' 
#' @inheritParams parallelTools::mclapply.0.1
#' 
#' @seealso \link[parallelTools]{mclapply}
#' @author Sebastian Warnholz
#' @rdname getSetPTOOptions
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

#' @rdname getSetPTOOptions
#' @export
setPTOption <- function(packageToLoad = "", sourceFile = "") {
  options(parallelToolsPTL = packageToLoad, parallelToolsSF = sourceFile)
}
