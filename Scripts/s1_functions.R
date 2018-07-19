print("Initializing...")

require(magrittr)
require(beepr)
require(raster)
require(plotrix)
require(reshape2)
require(randomForest)
require(doMC)
require(foreach)
require(parallel)
require(ggplot2) 
require(rasterVis)
require(rgdal)


# Other settings
options(digits = 4)
try(memory.limit())
detectCores()
try(registerDoSEQ())
try(registerDoMC(cores))
firstTrain <- 4
lastPred <- 15 

# Functions for preprocessing
loadYear <- function(year,dir){
  directory.year <- paste0(dir,"annualFolders/variables_",year)
  fileList <- list.files(path=directory.year,pattern=glob2rx("*.tif"))
  rasterList <- list()
  ex <- data.frame(n=1:length(fileList),xmin=NA,xmax=NA,ymin=NA,ymax=NA)
  for (i in 1:length(fileList)){
    filePath <- paste0(directory.year,"/",fileList[[i]])
    fileName <- fileList[[i]]
    r <- raster(filePath)
    fileName <- unlist(strsplit(fileName,split=""))
    varName <- paste(fileName[1:(length(fileName)-4)],collapse="")
    r@data@names <- as.character(varName)
    rasterList <- c(rasterList,r)
    ex$xmin[i] <- bbox(rasterList[[i]])[1,1]
    ex$xmax[i] <- bbox(rasterList[[i]])[1,2]
    ex$ymin[i] <- bbox(rasterList[[i]])[2,1]
    ex$ymax[i] <- bbox(rasterList[[i]])[2,2]
  }
  newExtent <- c(max(ex$xmin),min(ex$xmax),max(ex$ymin),min(ex$ymax))
  for (i in 1:length(rasterList)){
    rasterList[[i]] <- crop(rasterList[[i]],newExtent)
    extent(rasterList[[i]]) <- extent(rasterList[[1]])
  }
  s <- stack(rasterList)
  prev.defor.path <- paste0(dir,"annualFolders/variables_",year-1,"/deforestation.tif",sep="")
  prev.defor <- raster(prev.defor.path) %>%
    stack() %>%
    addFocal(list=c(3,11)) %>%
    dropLayer(i=1)
  s <- stack(s,prev.defor)
  s$deforestation <- as.factor(s$deforestation)
  s$landcover <- as.factor(s$landcover)
  s$comtype <- as.factor(s$comtype)
  s$distype <- as.factor(s$distype)
  s$protarea <- as.factor(s$protarea)
  return(s)
}
focus <- function(r){
  nr <- 1+r*2
  nc <- 1+r*2
  img <- matrix(1, ncol=nc, nrow=nr)
  img <- melt(id.var=1:nrow(img), img)
  names(img) <- c("rows","cols","z")
  center=c(median(1:nr), median(1:nc))
  img$z[sqrt((img$rows - center[1])^2 + (img$cols - center[2])^2) > r] = 0
  m <- matrix(img$z,ncol = nc)
  return(m)
}
addFocal <- function(s,list){
  s.focal <- stack()
  iter = 0
  for (i in list){
    iter = iter + 1
    w.m <- focus(i)
    focal.i <- focal(s$deforestation,w.m,pad=T,padValue=NA)
    names(focal.i) <- paste0("focal.deforestation.",i,sep="")
    values(focal.i)[is.na(values(focal.i))] = 0
    s.focal <- stack(s.focal,focal.i)
    gc()
  }
  s.new <- stack(s,s.focal)
  return(s.new)
} 
createFolds <- function(first,last,fold.size,dir){
  set.seed(1)
  iter <- 0
  train.list <- list()
  
  for (i in first:(last-1)){
    iter <- iter + 1
    s <- loadYear(i,dir)
    samp <- sampleStratified(s$fold,size=fold.size,na.rm=T,xy=T,sp=T) %>% 
      extract(x=s) %>%
      as.data.frame()
    train.list[[iter]] <- samp
    print(paste("Sampled year",i))
  }
  df.train <- do.call("rbind", train.list)
  for (f in 1:10){
    train.fold <- df.train[df.train$fold != f,]
    file.path <- paste0(dir,"folds/trainFold_",f,".Rda",sep="")
    save(train.fold,file = file.path)
    valid.fold <- loadYear(last,dir)
    valid.fold <-  sampleStratified(valid.fold$fold,size=fold.size*10,na.rm=T,xy=T,sp=T) %>% 
      extract(x=valid.fold) %>%
      as.data.frame()
    valid.fold <- valid.fold[valid.fold$fold == f,]
    file.path <- paste0(dir,"folds/validFold_",f,".Rda",sep="")
    save(valid.fold,file = file.path)
    print(paste("Saved training and validation folds",f))
  }
}

# Functions for hyperparameter optimization and validation
load.fold <- function(fold,dir){
  # print(paste("load.fold:",fold))
  loadCommand <- paste0(dir,"trainFold_",fold,".Rda",sep="")
  load(loadCommand)
  loadCommand <- paste0(dir,"validFold_",fold,".Rda",sep="")
  load(loadCommand)
  for (i in 1:ncol(train.fold)){
    if (is.factor(train.fold[,i])){
      valid.fold[,i] <- factor(valid.fold[,i], levels =  levels(train.fold[,i]))
    }
  }
  valid.fold <- valid.fold[complete.cases(valid.fold),]
  # print("   complete")
  return(list(train.fold,valid.fold))
}
rf.fold <- function(x,fold,seed,dir,verbose=F){
  # print(paste("rf.fold",fold,"ntree",x$ntree,"mtry",x$mtry,"sampsize",x$sampsize))
  # print(paste("rf.fold",fold))
  fold.l <- load.fold(fold,dir)
  # print("1")
  train <- fold.l[[1]]
  # print("2")
  train <- train[complete.cases(train),]
  # print("3")
  valid <- fold.l[[2]]
  # print("4")
  valid <- valid[complete.cases(valid),]
  # print("5")
  cutoff.dec <- x$cutoff/100
  # print("6")
  cutoff.input <- c(cutoff.dec,(1-cutoff.dec))
  # print("7")
  sampsize.input <- c(round(nrow(train[train$deforestation == 1,])/2),
                      round(nrow(train[train$deforestation == 1,])*(x$sampsize/100)))
  # stel nrow = 500 en sampsize = 50
  #  input[[1]] <- 250
  #  input[[2]] <- 500*(50/100=0.5)=250
  
  # 
  # print(paste("sampsize.input:",sampsize.input))
  predictions <- rep(0,times=nrow(valid))
  # print("9")
  set.seed(seed)
  # print("10")
  try(model <- randomForest(
    as.factor(deforestation)~.-fold,
    data=train,
    ntree = x$ntree,
    mtry = x$mtry,
    nodesize = x$nodesize,
    cutoff = cutoff.input,
    strata = as.factor(train$deforestation),
    sampsize = sampsize.input, 
    importance = TRUE))
  # print("11")
  try(predictions <- predict(model,newdata=valid))
  # print("12")
  confusion <- squareTable(predictions,valid$deforestation)
  print(paste(confusion[1,1],confusion[1,2],confusion[2,1],confusion[2,2]))
  # print("13")
  TN <- confusion[1,1]
  FP <- confusion[1,2]
  FN <- confusion[2,1]
  TP <- confusion[2,2]
  TPR <- TP/(TP+FN)
  if(is.nan(TPR)){TPR <- 0
  print("TPR is NaN")}
  FPR <- FP/(TN+FP)
  ACC <- (TP+TN)/(TP+TN+FP+FN)
  k <- kappa(TN,FP,FN,TP)
  if(TP+FN == 0){PR <- 0}else{PR <- (TP+FP)/(TP+FN)}
  # print("14")
  results <- data.frame(TN,FP,FN,TP,k,TPR,FPR,ACC,PR)
  # print(results)
  # print("   complete")
  return(results)
}
rf.validate <- function(x,seeds,dir,folds=1:10,verbose=F){
  # print(paste("rf.validate"))
  foreach(fold.cv = folds, .packages = "randomForest") %:%
    foreach(seed.cv = seeds) %dopar% rf.fold(x,fold.cv,seed.cv,dir)
}
rf.cv <- function(x,seeds,dir,folds=1:10,verbose=F){
  # print("rf.cv")
  # print(x)
  results <- unlist(rf.validate(x,seeds,dir,folds,verbose),recursive = F)
  results <- do.call("rbind", results)
  # print(results)
  # print("rf.cv complete")
  return(results)
}
rf.calc <- function(x,seeds,dir,folds=1:10,verbose=F){
  # print(x)
  for (i in 1:nrow(x)){
    # print(x[i,])
    cv <- rf.cv(x[i,],seeds,dir,folds,verbose)
    x$k[i] <- mean(cv$k)
    x$tpr[i] <- mean(cv$TPR)
    x$fpr[i] <- mean(cv$FPR)
    x$acc[i] <- mean(cv$ACC)
    x$pr[i] <- mean(cv$PR)
    # print(x[i,])
    x$score[i] <- score(x$k[i],x$pr[i])
    # print(x[i,])
    print(paste(i,"ntree:",x$ntree[i],"mtry:",x$mtry[i],"nodesize:",x$nodesize[i],"sampsize",x$sampsize[i],"cutoff",x$cutoff[i],"kappa:",round(mean(cv$k),digits=4),"accuracy:",round(mean(cv$ACC),digits=4),"tpr:",round(mean(cv$TPR),digits=4),"fpr:",round(mean(cv$FPR),digits=4),"pr:",round(mean(cv$PR),digits=4),"score:",round(x$score[i],digits=4)))
  }
  # print(x)
  gc()
  
  return(x)
}
range <- function(max.ntree,min.ntree,verbose=F){
  # create a dataframe with two rows: minimum and maximum per hyperparameter
  ntree <- c(min.ntree,max.ntree)
  mtry <- c(7,20)
  cutoff <- c(1,99)
  sampsize <- c(1,100)
  nodesize <- c(1,5)
  df <- data.frame(ntree,mtry,cutoff,sampsize,nodesize)
  return(df)
}
choose <- function(min,max,verbose=F){
  range <- min:max
  choice <- unlist(sample(range,size=1))
  return(choice)
}
spawn <- function(amount,max.ntree=10,min.ntree=1,verbose=F){
  # create a dataframe with random hyperparameter combinations
  range <- range(max.ntree,min.ntree)
  combinations <- range[0,]
  for (i in 1:amount){
    set.seed(i)
    for (j in 1:ncol(range)){
      combinations[i,j] <- choose(range[1,j],range[2,j])
    }
  }
  combinations$iteration <- 0
  return(combinations)
}
select <- function(pop,best=0.4,lucky=0.1,verbose=F){
  pop.size <- nrow(pop)
  # pop <- pop[order(pop$k,decreasing = TRUE ),]
  pop <- pop[order(pop$score,decreasing = TRUE ),]
  pop.best <- pop[1:round(pop.size*best),]
  pop.lucky <- pop[sample(((pop.size*best)+1):pop.size,size=round(pop.size*lucky)),]
  parents <- rbind(pop.best,pop.lucky)
  return(parents)
}
evolve <- function(pop,iteration,pct.clones=0.5,dir,verbose=F,seeds=1){
  parents <- select(pop,verbose=F)
  n.offspring <- nrow(pop)-nrow(parents)
  n.clones <- round(n.offspring*pct.clones)
  clones <- parents[0,]
  for (i in 1:nrow(parents)){
    n.clones.i <- (nrow(parents)*pct.clones)/i
    if (n.clones.i >= 1){
      n.clones.i <- round(n.clones.i)
      if(nrow(clones) < n.clones){
        clones.i <- clone(parents[i,],n.clones.i,iteration,dir,verbose)
        clones <- rbind(clones,clones.i)
      }
    }
  }
  n.children <- nrow(pop)-nrow(parents)-nrow(clones)
  if(n.children > 0){
    children <- breed(parents,n.children,iteration,dir)
    pop <- rbind(parents,clones,children)
  }else{pop <- rbind(parents,clones)}
  return(pop)
}
mutate <- function(t1,t2=NA,min=1,max=NA,max.mutate=5,verbose=F){
  mutate.range <- c(-max.mutate:-1,1:max.mutate)
  mutation <- unlist(sample(mutate.range,size = 1))
  if (is.na(t2)){
    t.i <- t1
  }else{t.i <- choose(t1,t2)}
  t.i <- round(t.i+mutation)
  if (t.i < min){t.i <- min}
  if (!is.na(max)){if (t.i > max){t.i <- max}}
  return(t.i)
}
breed <- function(parents,n.children,iteration,dir,verbose=F,seeds=1){
  children <- data.frame(ntree=1:n.children,mtry=NA,cutoff=NA,sampsize=NA,nodesize=NA)
  for (i in 1:n.children){
    set.seed((iteration+i))
    parents.i <- sample(1:nrow(parents),size=2,replace=F)
    parents.i <- parents[parents.i,]
    set.seed((iteration+i))
    children$ntree[i] <- mutate(parents.i$ntree[1],parents.i$ntree[2],max.mutate=20)
    children$mtry[i] <- mutate(parents.i$mtry[1],parents.i$mtry[2],max=25,max.mutate=5)
    children$cutoff[i] <- mutate(parents.i$cutoff[1],parents.i$cutoff[2],max=99,max.mutate = 5)
    children$sampsize[i] <- mutate(parents.i$sampsize[1],parents.i$sampsize[2],max=99,max.mutate = 5)
    children$nodesize[i] <- mutate(parents.i$nodesize[1],parents.i$nodesize[2],max=100,max.mutate = 5)
  }
  children <- rf.calc(children,seeds,dir)
  children$iteration <- iteration
  return(children)
}
clone <- function(parent,n.clones,iteration,dir,verbose=F,seeds=1){
  clones <- data.frame(ntree=1:n.clones,mtry=NA,cutoff=NA,sampsize=NA,nodesize=NA)
  for (i in 1:n.clones){
    set.seed((iteration+i))
    clones$ntree[i] <- mutate(parent$ntree,max.mutate = 20)
    clones$mtry[i] <- mutate(parent$mtry,max=25,max.mutate = 2)
    clones$cutoff[i] <- mutate(parent$cutoff,max=99,max.mutate = 2)
    clones$sampsize[i] <- mutate(parent$sampsize,max=99,max.mutate = 2)
    clones$nodesize[i] <- mutate(parent$nodesize,max=100,max.mutate = 2)
  }
  clones <- unique(clones)
  clones <- rf.calc(clones,seeds,dir)
  if(nrow(clones) < n.clones){
    for (i in ((nrow(clones)+1):n.clones)){
      clones[i,] <- parent[1,]}}
  clones$iteration <- iteration
  return(clones)
}
evo.plot <- function(pop.all,pop,record,dir){
  jpegName <- paste0(dir,"evolution/evoPlot.jpg")
  jpeg(jpegName)
  plot(pop.all$iteration,pop.all$score,col="gray")
  points(pop$iteration,pop$score,col="black")
  points(record$iteration,record$score.mean,col="red")
  dev.off()
  plot(pop.all$iteration,pop.all$score,col="gray")
  points(pop$iteration,pop$score,col="black")
  points(record$iteration,record$score.mean,col="red")
}
record.update <- function(pop,record=NA){
  if(is.na(record)){record <- data.frame(iteration=0,score.mean=mean(pop$score),score.max=max(pop$score),k.mean=mean(pop$k),k.max=max(pop$k),acc.mean=mean(pop$acc),acc.max=max(pop$acc),pr.mean=mean(pop$pr))}else{
    record.i <-data.frame(iteration=max(pop$iteration),score.mean=mean(pop$score),score.max=max(pop$score),k.mean=mean(pop$k),k.max=max(pop$k),acc.mean=mean(pop$acc),acc.max=max(pop$acc),pr.mean=mean(pop$pr))
    record <- rbind(record,record.i)
  }
  return(record)
}

# Functions for predicting
loadForest <- function(dir){
  forestPath <- paste0(dir,"finalValidation/model.Rdata")
  load(forestPath)
  return(model)
}
saveDeforestation <- function(real,prediction,year,dir){
  names(real) <- paste0("real_",year)
  savePath <- paste0(dir,"prediction/real_",year,".tif")
  writeRaster(real,filename = savePath, overwrite=T)
  names(prediction) <- paste0("predicted_",year)
  savePath <- paste0(dir,"prediction/predicted_",year,".tif")
  writeRaster(prediction,filename = savePath, overwrite=T)
}
loadYearPred <- function(year,dir){
  directory.year <- paste0(dir,"annualFolders/variables_",year)
  fileList <- list.files(path=directory.year,pattern=glob2rx("*.tif"))
  print(fileList)
  rasterList <- list()
  ex <- data.frame(n=1:length(fileList),xmin=NA,xmax=NA,ymin=NA,ymax=NA)
  for (i in 1:length(fileList)){
    filePath <- paste0(directory.year,"/",fileList[[i]])
    fileName <- fileList[[i]]
    r <- raster(filePath)
    fileName <- unlist(strsplit(fileName,split=""))
    varName <- paste(fileName[1:(length(fileName)-4)],collapse="")
    r@data@names <- as.character(varName)
    rasterList <- c(rasterList,r)
    ex$xmin[i] <- bbox(rasterList[[i]])[1,1]
    ex$xmax[i] <- bbox(rasterList[[i]])[1,2]
    ex$ymin[i] <- bbox(rasterList[[i]])[2,1]
    ex$ymax[i] <- bbox(rasterList[[i]])[2,2]
  }
  newExtent <- c(max(ex$xmin),min(ex$xmax),max(ex$ymin),min(ex$ymax))
  for (i in 1:length(rasterList)){
    rasterList[[i]] <- crop(rasterList[[i]],newExtent)
    extent(rasterList[[i]]) <- extent(rasterList[[1]])
  }
  s <- stack(rasterList)
  return(s)
}

# Other functions
default.mtry <- function(dir){
  path <- paste0(dir,"trainFold_1.Rda",sep="")
  load(path)
  default.mtry <- floor(sqrt(ncol(train.fold)-1))
  return(default.mtry)
}
squareTable <- function(x,y) {
  x <- factor(x)
  y <- factor(y)
  commonLevels <- sort(unique(c(levels(x), levels(y))))
  pred <- factor(x, levels = commonLevels)
  real <- factor(y, levels = commonLevels)
  table(pred,real)
}
kappa <- function(TN,FP,FN,TP){
  TN <- as.numeric(TN)
  FP <- as.numeric(FP)
  FN <- as.numeric(FN)
  TP <- as.numeric(TP)
  real.S <- TN+TP
  max.S <- TN+FP+FN+TP
  random.S <- (((TN+FP)*(TN+FN))/max.S) + (((FN+TP)*(FP+TP))/max.S)
  kappa <- (real.S-random.S)/(max.S-random.S)
  return(kappa)
}
score <- function(k,pr){
  if(pr > 1){s <- k/pr}
  if(pr <= 1){s <- k*pr}
  return(s)
}

test <- score(-0.001019,10.86)

print(paste("Initialization completed at",Sys.time()))
