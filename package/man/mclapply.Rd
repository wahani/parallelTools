\name{mclapply}
\alias{mclapply}
\title{mclapply.0.1}
\usage{
  mclapply(X, FUN, ..., mc.preschedule = TRUE,
    mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = 1L,
    mc.cleanup = TRUE, mc.allow.recursive = TRUE,
    packageToLoad = "", sourceFile = "")

  mclapply(X, FUN, ..., mc.preschedule = TRUE,
    mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = 1L,
    mc.cleanup = TRUE, mc.allow.recursive = TRUE,
    packageToLoad = "", sourceFile = "")
}
\arguments{
  \item{mc.preschedule}{see
  \code{\link[parallel]{mclapply}} for Linux. Windows: If
  TRUE \code{\link{clusterApply}} will be used, if FALSE
  \code{\link{clusterApplyLB}} is used}

  \item{packageToLoad}{these two arguments are only
  relevant using Windows. packageToLoad is a character
  vector with the names of the packages which should be
  loaded using \code{\link{require}} on to each cluster. In
  addition with sourceFile a R-Script can be specified
  which will be sourced on each cluster.}

  \item{sourceFile}{see \code{packageToLoad}}

  \item{mc.cores}{number of cores to be used.}

  \item{mc.preschedule}{see
  \code{\link[parallel]{mclapply}} for Linux. Windows: If
  TRUE \code{\link{clusterApply}} will be used, if FALSE
  \code{\link{clusterApplyLB}} is used}

  \item{mc.cores}{number of cores to be used.}

  \item{X}{a vector (atomic or list) or an expressions
  vector.  Other objects (including classed objects) will
  be coerced by \code{\link{as.list}}.}

  \item{FUN}{the function to be applied to
  (\code{mclapply}) each element of \code{X} or
  (\code{mcmapply}) in parallel to \code{\dots}.}

  \item{...}{For \code{mclapply}, optional arguments to
  \code{FUN}.  For \code{mcmapply} and \code{mcMap}, vector
  or list inputs: see \code{\link{mapply}}.}

  \item{mc.set.seed}{See \code{\link{mcparallel}}.}

  \item{mc.silent}{if set to \code{TRUE} then all output on
  \file{stdout} will be suppressed for all parallel
  processes forked (\file{stderr} is not affected).}

  \item{mc.cleanup}{if set to \code{TRUE} then all children
  that have been forked by this function will be killed (by
  sending \code{SIGTERM}) before this function returns.
  Under normal circumstances \code{mclapply} waits for the
  children to deliver results, so this option usually has
  only effect when \code{mclapply} is interrupted. If set
  to \code{FALSE} then child processes are collected, but
  not forcefully terminated.  As a special case this
  argument can be set to the number of the signal that
  should be used to kill the children instead of
  \code{SIGTERM}.}

  \item{mc.allow.recursive}{Unless true, calling
  \code{mclapply} in a child process will use the child and
  not fork again.}
}
\description{
  Old version of parallelTools::mclapply. This function
  masks the function mclapply from package parallel. The
  aim is to use multicore functionality under windows and
  linux. For the description of the function arguments see:
  \link[parallel]{mclapply} in the package parallel

  This function masks the function mclapply from package
  parallel. The aim is to use multicore functionality under
  windows and linux. For the description of the function
  arguments see: \link[parallel]{mclapply} in the package
  parallel
}
\details{
  The function will detect th operating system via
  \code{.Platform$OS.type}. If the platform is equal to
  "windows", \code{mc.cores} clusters will be initialized
  using \code{\link{makeCluster}}. Packages and sourcefiles
  will be loaded onto each cluster using
  \code{\link{clusterEvalQ}}. Then either
  \code{\link{clusterApply}} or
  \code{\link{clusterApplyLB}} is called with the arguments
  \code{x = X, fun = FUN, ... = ...} - \code{mc.set.seed},
  \code{mc.silent}, \code{mc.cleanup} and
  \code{mc.allow.recursive} will be ignored under windows.
  The function will end with \code{\link{stopCluster}} to
  end all started sessions and return the resulting list.

  If \code{.Platform$OS.type} is not equal to windows the
  function \code{\link[parallel]{mclapply}} will be called,
  passing all arguments but \code{packageToLoad} and
  \code{sourceFile} which will be ignored.

  This function is written to use multicore functionality
  in developing mainly for linux but with the ability to
  test the code in windows. The side effects of the applied
  parallelization for windows I do not know. Check the
  running processes regularly.

  The function will detect th operating system via
  \code{.Platform$OS.type}. If the platform is equal to
  "windows", \code{mc.cores} clusters will be initialized
  using \code{\link{makeCluster}}. Packages and sourcefiles
  will be loaded onto each cluster using
  \code{\link{clusterEvalQ}}. Then either
  \code{\link{clusterApply}} or
  \code{\link{clusterApplyLB}} is called with the arguments
  \code{x = X, fun = FUN, ... = ...} - \code{mc.set.seed},
  \code{mc.silent}, \code{mc.cleanup} and
  \code{mc.allow.recursive} will be ignored under windows.
  The function will end with \code{\link{stopCluster}} to
  end all started sessions and return the resulting list.

  If \code{.Platform$OS.type} is not equal to windows the
  function \code{\link[parallel]{mclapply}} will be called,
  passing all arguments but \code{packageToLoad} and
  \code{sourceFile} which will be ignored.

  This function is written to use multicore functionality
  in developing mainly for linux but with the ability to
  test the code in windows. The side effects of the applied
  parallelization for windows I do not know. Check the
  running processes regularly.
}
\examples{
genData <- function(n) {
 # Generate some Data
 dat <- data.frame()
 for (i in 1:n) {
   dat <- rbind(dat, data.frame(colA = letters[sample(26)], colB = letters[sample(26)]))
 }
 dat
}

slowFunction <- function(dat) {
 # A slow function
 dat$ind <- logical(nrow(dat))

 for (i in 1:nrow(dat)) {
   dat$ind[i] <- grepl(dat$colA[i], dat$colB[i])
 }

 dat
}

nCores <- detectCores() - 1 # Detects the number of cores
dat <- genData(1000) # generate some Date
dat$sample <- rep_len(1:nCores, length.out = nrow(dat)) # ID for splitting the data
dat$id <- 1:nrow(dat) # ID for observations
dataList <- split(dat, dat$sample) # split the dataSet for processing the tasks parallel

system.time(result1 <- slowFunction(dat)) # Function without mc

# Function with mc - should work for all platforms
system.time(result2 <- do.call("rbind", mclapply(X = dataList, FUN = slowFunction, mc.cores = nCores)))

# Better than mc is vecorization (in this case) - no multicore required:
system.time(result3 <- mapply(grepl, as.list(dat$colA), as.list(dat$colB)))

#All the same
all((result1$ind == result2$ind[order(result2$id)]) & result1$ind == result3)
genData <- function(n) {
 # Generate some Data
 dat <- data.frame()
 for (i in 1:n) {
   dat <- rbind(dat, data.frame(colA = letters[sample(26)], colB = letters[sample(26)]))
 }
 dat
}

slowFunction <- function(dat) {
 # A slow function
 dat$ind <- logical(nrow(dat))

 for (i in 1:nrow(dat)) {
   dat$ind[i] <- grepl(dat$colA[i], dat$colB[i])
 }

 dat
}

nCores <- detectCores() - 1 # Detects the number of cores
dat <- genData(1000) # generate some Date
dat$sample <- rep_len(1:nCores, length.out = nrow(dat)) # ID for splitting the data
dat$id <- 1:nrow(dat) # ID for observations
dataList <- split(dat, dat$sample) # split the dataSet for processing the tasks parallel

system.time(result1 <- slowFunction(dat)) # Function without mc

# Function with mc - should work for all platforms
system.time(result2 <- do.call("rbind", mclapply(X = dataList, FUN = slowFunction, mc.cores = nCores)))

# Better than mc is vecorization (in this case) - no multicore required:
system.time(result3 <- mapply(grepl, as.list(dat$colA), as.list(dat$colB)))

#All the same
all((result1$ind == result2$ind[order(result2$id)]) & result1$ind == result3)
}
\author{
  Sebastian Warnholz

  Sebastian Warnholz
}
\seealso{
  Everything in the package "parallel": \link{clusterApply}
  and \link[parallel]{mclapply} and \link{mcmapply}

  Everything in the package "parallel": \link{clusterApply}
  and \link[parallel]{mclapply} and \link{mcmapply}
}

