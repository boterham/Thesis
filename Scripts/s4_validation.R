print(paste("Validation of",dir,"started at",Sys.time()))

set.seed(1)
evolve.dir <- paste0(dir,"evolution/")
pop.all <- paste0(evolve.dir,"pop_all_",iterations,".Rda")
load(pop.all)

trainPath <- paste0(dir,"finalValidation/train.Rdata")
validPath <- paste0(dir,"finalValidation/valid.Rdata")
load(trainPath)
load(validPath)

hp <- pop.all[pop.all$score == max(pop.all$score),]
hp <- hp[hp$ntree == min(hp$ntree),]
hp <- hp[1,]

forestPath <- paste0(dir,"finalValidation/model.Rdata")
cutoff.dec <- hp$cutoff/100
cutoff.input <- c(cutoff.dec,(1-cutoff.dec))
print(cutoff.input)
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
validation.df <- data.frame(tn=t[1,1],tp=t[2,2],fp=t[2,1],fn=t[1,2],k,a,pr)
valid.df.path <- paste0(dir,"finalValidation/results.RDa")
save(validation.df,file = valid.df.path)

print(paste("Validation completed at",Sys.time()))
