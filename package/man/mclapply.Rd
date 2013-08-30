\name{mclapply}
\alias{mclapply}
\title{mclapply - Wrapper for parallel::mclapply/clusterApply}
\usage{
  mclapply(X, FUN, ..., mc.preschedule = TRUE,
    mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = 1L,
    mc.cleanup = TRUE, mc.allow.recursive = TRUE,
    packageToLoad = "", sourceFile = "")
}
\description{
  mclapply - Wrapper for parallel::mclapply/clusterApply
}
\details{
  Arguments are as for parallel::mclapply
}

