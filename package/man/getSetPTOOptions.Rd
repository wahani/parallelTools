\name{getPTOption}
\alias{getPTOption}
\alias{setPTOption}
\title{get/set packages/sourceFiles to be loaded for Windows}
\usage{
  getPTOption()

  setPTOption(packageToLoad = "", sourceFile = "")
}
\arguments{
  \item{packageToLoad}{these two arguments are only
  relevant using Windows. packageToLoad is a character
  vector with the names of the packages which should be
  loaded using \code{\link{require}} on to each cluster. In
  addition with sourceFile a R-Script can be specified
  which will be sourced on each cluster.}

  \item{sourceFile}{see \code{packageToLoad}}
}
\description{
  get and set the packages and source files which will be
  loaded on to each cluster. Only relevant in Windows when
  the function applied in
  \code{\link[parallelTools]{mclapply}} depends on some
  package or objects which can be sourced.
}
\author{
  Sebastian Warnholz
}
\seealso{
  \link[parallelTools]{mclapply}
}

