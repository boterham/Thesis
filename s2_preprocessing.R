print(paste("Preprocessing",dir,"started at",Sys.time()))


createFolds(firstTrain,lastTrain,foldSize,dir)
print("Created Folds")

yearList <- list()
iter <- 0

for (i in firstTrain:(lastTrain-1)){
  iter <- (iter+1)
  s <- loadYear(i,dir)
  defor <- s$deforestation
  yearList[[iter]] <- sampleRandom(defor,sampSize,na.rm=T,sp=T) %>%
    extract(x=s) %>%
    as.data.frame()
  print(paste(""))
}
train <- do.call("rbind", yearList) %>% na.omit()
valid <- loadYear(lastTrain,dir)
trainPath <- paste0(dir,"finalValidation/train.Rdata")
validPath <- paste0(dir,"finalValidation/valid.Rdata")
save(train,file=trainPath)
save(valid,file=validPath)

print("Created final validation data")
rm(train,valid,trainPath,validPath,s,defor,yearList,iter)
gc()
yearList <- list()
iter <- 0

for (i in firstTrain:lastTrain){
  iter <- (iter+1)
  s <- loadYear(i,dir)
  defor <- s$deforestation
  yearList[[iter]] <- sampleRandom(defor,sampSize,na.rm=T,sp=T) %>%
    extract(x=s) %>%
    as.data.frame()
  print(paste(""))
}
train <- do.call("rbind", yearList) %>% na.omit()
trainPath <- paste0(dir,"prediction/train.Rdata")
save(train,file=trainPath)

rm(train,valid,trainPath,validPath,s,defor,yearList,iter)
gc()
print(paste("Preprocessing completed at",Sys.time()))
