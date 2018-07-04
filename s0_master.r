rm(list = ls())
# try(.libPaths("/home/bos053/R/lib/"))
.libPaths()
# Capacity Settings
foldSize <- 200 # Size of each training fold for genetic optimization
pop.size <- 20
max.trees <- 100
min.trees <- 1
iterations <- 10 # Number of evolution iterations before a final model is chosen
sampSize <- 500 # Size of each training fold for the final model
cores <- 3


filenames <- c("Scripts/s1_functions.R",
               "Scripts/s2_preprocessing.r",
               "Scripts/s3_optimization.r",
               "Scripts/s4_validation.r",
               "Scripts/s5_prediction.r",
               "Scripts/s6_analysis.r")
provinces <- data.frame(name=c("SonLa","LaiChau","DienBien","HoaBinh"),
                        lastTrain=c(9,12,13,12),
                        firstPred=c(10,13,14,13))
# for (i in 1:nrow(provinces)){
#   dir <- paste0(getwd(),"/Provinces/",provinces$name[i],"/")
#   lastTrain <- provinces$lastTrain[i]
#   firstPred <- lastTrain + 1
#   try(sapply(filenames, source))
# }

dir <- paste0(getwd(),"/Provinces/LaiChau/")
lastTrain <- 12
firstPred <- 13
sapply(filenames,source)


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



