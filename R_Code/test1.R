# getPTOption <- function() {
#   ptOptions <- .Options[grepl("parallelTools", names(.Options))]
#   if(length(ptOptions) == 0) {
#     cat("No parrallelTools options set\n")
#     invisible(NULL)
#   } else {
#     cat("packages that will be loaded on each cluster:", 
#         if(ptOptions$parallelToolsPTL == "") "\"nothing\"" else ptOptions$parallelToolsPTL,
#         "\nsource files sourced on each cluster:", 
#         if(ptOptions$parallelToolsSF == "") "\"nothing\"" else ptOptions$parallelToolsSF)
#     invisible(ptOptions)
#   }
# }
# 
# setPTOption <- function(packageToLoad = "", sourceFile = "") {
#   options(parallelToolsPTL = packageToLoad, parallelToolsSF = sourceFile)
# }

setPTOption("testthat")
getPTOption()








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
