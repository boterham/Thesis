rm(list = ls())
try(.libPaths("/home/bos053/R/lib/"))
.libPaths()
# Capacity Settings
foldSize <- 1000 # Size of each training fold for genetic optimization
pop.size <- 20
max.trees <- 1000
min.trees <- 1
iterations <- 100 # Number of evolution iterations before a final model is chosen
sampSize <- 2000 # Size of each training fold for the final model
cores <- 10
getwd()

filenames <- c("Scripts/s1_functions.R",
               "Scripts/s2_preprocessing.R",
               "Scripts/s3_optimization.R",
               "Scripts/s4_validation.R",
               "Scripts/s5_prediction.R",
               "Scripts/s6_analysis.R")

provinces <- data.frame(name=c("Test","SonLa","LaiChau","DienBien","HoaBinh"),
                        lastTrain=c(9,9,12,13,12),
                        firstPred=c(10,10,13,14,13))

for (i in 1:nrow(provinces)){
  dir <- paste0(getwd(),"/Provinces/",provinces$name[i],"/")
  lastTrain <- provinces$lastTrain[i]
  firstPred <- lastTrain + 1
  try(sapply(filenames, source))
}

prov.list <- list()
for (i in 1:nrow(provinces)){
  results <- paste0(getwd(),"/Provinces/",provinces$name[i],"/prediction/results.RDa")
  if(file.exists(results)){
    load(results)
    df$province <- provinces$name[i]
    prov.list[[i]] <- df
    rm(df)
  }
}
results <- do.call("rbind",prov.list)
results$forest.diff <- results$forest.p/results$forest.r
p <- ggplot(results, aes(colour=factor(province),year,forest.diff)) +
  geom_line(size=1)
p



