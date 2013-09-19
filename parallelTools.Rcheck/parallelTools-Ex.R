pkgname <- "parallelTools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('parallelTools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("mclapply")
### * mclapply

flush(stderr()); flush(stdout())

### Name: mclapply
### Title: mclapply - Wrapper for parallel::mclapply/clusterApply
### Aliases: mclapply

### ** Examples

genData <- function(n) {
  dat <- data.frame()
  for (i in 1:n) {
    dat <- rbind(dat, data.frame(colA = letters[sample(26)], colB = letters[sample(26)]))
  }
  dat
}

slowFunction <- function(dat) {
  dat$ind <- logical(nrow(dat))

  for (i in 1:nrow(dat)) {
    dat$ind[i] <- grepl(dat$colA[i], dat$colB[i])
  }

  dat
}

nCores <- detectCores() - 1
dat <- genData(1000)
dat$sample <- gl(nCores, 1)
dataList <- split(dat, dat$sample)

system.time(result1 <- slowFunction(dat))
system.time(result2 <- mclapply(X = dataList, FUN = slowFunction, mc.cores = nCores))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
