if(.Platform$OS.type == "windows") {
  require(devtools)
  install_github(repo="parallelTools", username = "wahani", subdir = "package")
}
require(parallelTools)
mclapply(1:10, 
         function(x) {
           data(oats)
           summary(oats)
         }, mc.cores = detectCores(), packageToLoad = c("MASS", "parallel"),
         sourceFile = "R_Code/MClapply.R")


