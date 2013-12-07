\name{mclapply}
\alias{mclapply}
\title{Parallel version of lapply}
\usage{
  mclapply(X, FUN, ..., mc.preschedule = TRUE,
    mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = 1L,
    mc.cleanup = TRUE, mc.allow.recursive = TRUE)
}
\arguments{
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
\value{
  See the documentation for \code{\link{clusterApply}} when
  running Windows or \code{\link[parallel]{mclapply}}
  otherwise.
}
\description{
  This function masks the function mclapply from package
  parallel. The aim is to use multicore functionality under
  windows and linux using the same function call.
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
  \code{x = X, fun = FUN, ... = ...} - the arguments
  \code{mc.set.seed}, \code{mc.silent}, \code{mc.cleanup}
  and \code{mc.allow.recursive} will be ignored under
  windows. The function will end with
  \code{\link{stopCluster}} to end all started sessions and
  return the resulting list.

  If \code{.Platform$OS.type} is not equal to windows the
  function \code{\link[parallel]{mclapply}} will be called,
  passing all arguments but \code{packageToLoad} and
  \code{sourceFile} which will be ignored.

  This function is written to use multicore functionality
  in developing mainly for linux but with the ability to
  test the code in windows. The function is written such
  that it is not necessary to use the package running linux
  since in this case it is a wrapper for
  \code{\link[parallel]{mclapply}}.
}
\examples{
# Examples from parallel::mclapply and parallel::clusterApply:
simplify2array(mclapply(rep(4, 5), rnorm, mc.cores = 2))
mclapply(1:2, get("+"), 3, mc.cores = 2)

## Not run: ## Set up each worker, relevant for Windows, see the examples
# for clusterApply for further details:
setPTOption(packageToLoad = "parallel") # load the package 'parallel' on each worker.
setPTOption(sourceFile = "workerSetupScript.R") # run the script on each worker.
## End(Not run)
}
\author{
  Sebastian Warnholz
}
\seealso{
  \code{\link{clusterApply}},
  \code{\link[parallel]{mclapply}}
}

