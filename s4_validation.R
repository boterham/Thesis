print(paste("Validation of",dir,"started at",Sys.time()))

set.seed(1)
evolve.dir <- paste0(dir,"evolution/")
query <- paste0("pop_all_*")
pop.all <- list.files(path=evolve.dir,pattern=glob2rx(as.character(query))) %>% max
load(paste0(evolve.dir,pop.all))

trainPath <- paste0(dir,"finalValidation/train.Rdata")
validPath <- paste0(dir,"finalValidation/valid.Rdata")
load(trainPath)
load(validPath)

hp <- pop.all[pop.all$k == max(pop.all$k),]
hp <- hp[hp$ntree == min(hp$ntree),]

forestPath <- paste0(dir,"finalValidation/model.Rdata")
cutoff.dec <- hp$cutoff/100
cutoff.input <- c(cutoff.dec,(1-cutoff.dec))
sampsize.input <- c(round(nrow(train[train$deforestation == 1,])/2),
                    round(nrow(train[train$deforestation == 1,])*(hp$sampsize/100)))
model <- randomForest(
  as.factor(deforestation)~.-fold,
  data=train,
  ntree = hp$ntree,
  mtry = hp$mtry,
  nodesize = hp$nodesize,
  cutoff = cutoff.input,
  strata = as.factor(train$deforestation),
  sampsize = sampsize.input, 
  importance = TRUE) 
save(model,file=forestPath)


pred <- predict(object = valid,model)
real <- mask(valid$deforestation,mask=valid$fold)
t <- squareTable(values(pred),values(real))
k <- kappa(t[1,1],t[2,1],t[1,2],t[2,2])
a <- (t[1,1]+t[2,2])/sum(t)
pr <- (t[2,1]+t[2,2])/(t[1,2]+t[2,1])
pred.info <- paste("Pred acc:",round(a,digits = 2),"k:",round(k,digits=2),"pr:",round(pr,digits=2))
par(mfrow=c(1,2))
plot(real,main="Real")
plot(pred,main=pred.info)

print(paste("Validation completed at",Sys.time()))