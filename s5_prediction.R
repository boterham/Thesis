print(paste("Prediction of",dir,"started at",Sys.time()))

r.real <- list()
r.pred <- list()

# Years before PFES
fold <- paste0(dir,"annualFolders/variables_",firstTrain,"/fold.tif") %>% 
  raster()
for (i in firstTrain:lastTrain){
  # print(paste("Year",i+2000))
  deforestation <- paste0(dir,"annualFolders/variables_",i,"/deforestation.tif") %>%
    raster()
  deforestation <- mask(deforestation,fold)
  realName <- paste0(dir,"prediction/real_",i,".tif")
  writeRaster(deforestation,filename = realName,overwrite=T)
  r.real[[i]] <- freq(deforestation)
  r.pred[[i]] <- freq(deforestation)
  defmain <- paste0("real",i)
  plot(deforestation,main=defmain)
  rm(deforestation)
  gc()
}
rm(fold)
gc()

# First Year after PFES
s <- loadYear(firstPred,dir)
pred <- predict(object=s,model)
s$deforestation <- mask(s$deforestation,pred)
saveDeforestation(s$deforestation,pred,firstPred,dir)
r.real[[firstPred]] <- freq(s$deforestation)
r.pred[[firstPred]] <- freq(pred)
realmain <- paste0("real",firstPred)
plot(s$deforestation,main=realmain)
predmain <- paste0("pred",firstPred)
plot(pred,main=predmain)

# Other years after PFES
for (i in (firstPred+1):lastPred){
  # print(paste("Year:",i+2000))
  names(pred) <- "deforestation"
  s <- loadYearPred(i,dir)
  fc <- pred
  fc[fc == 1] <- NA
  focals <- stack(pred) %>%
    addFocal(list=c(3,11)) %>%
    dropLayer(i=1)
  s <- stack(s,focals)
  pred <- predict(object=s,model)
  pred <- mask(pred,fc)
  s$deforestation <- mask(s$deforestation,s$fold)
  saveDeforestation(s$deforestation,pred,i,dir)
  r.real[[i]] <- freq(s$deforestation)
  r.pred[[i]] <- freq(pred)
  realmain <- paste0("real",i)
  plot(s$deforestation,main=realmain)
  predmain <- paste0("pred",i)
  plot(pred,main=predmain)
  rm(focals,s)
  gc()
}

df <- data.frame(year=1:lastPred,forest.r=NA,deforested.r=NA,forest.p=NA,deforested.p=NA)
for (i in firstTrain:lastPred){
  # print(length(r.pred[[i]]))
  # print(nrow(r.pred[[i]]))
  df$forest.r[i] <- (r.real[[i]][1,2])*0.0729
  df$deforested.r[i] <- (r.real[[i]][2,2])*0.0729
  df$forest.p[i] <- (r.pred[[i]][1,2])*0.0729
  if(nrow(r.pred[[i]]) == 2){df$deforested.p[i] <- 0}
  if(nrow(r.pred[[i]]) == 3){df$deforested.p[i] <- (r.pred[[i]][2,2]*0.0729)}
}
df <- df[complete.cases(df),]
result.dir <- paste0(dir,"prediction/results.RDa")
save(df,file = result.dir)

rm(r.real,r.pred,pred,realName,defMain,s,realmain,predmain,df,result.dir)
gc()
print(paste("Prediction completed at",Sys.time()))