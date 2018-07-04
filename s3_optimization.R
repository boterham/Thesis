print(paste("Genetic optimization of",dir,"started at",Sys.time()))

fold.dir <- paste0(dir,"folds/",sep="")
evolve.dir <- paste0(dir,"evolution/",sep="")
pop.all <- spawn(pop.size,max.ntree=max.trees,min.ntree=min.trees)
pop.all <- rf.calc(pop.all,seeds=1,dir = fold.dir)
pop <- pop.all
record <- record.update(pop)
record.dir <- paste0(evolve.dir,"record.Rda")
save(record,file = record.dir)
pop.all.dir <- paste0(evolve.dir,"pop_all_0.Rda")
save(pop.all,file = pop.all.dir)
pop.i.dir <- paste0(evolve.dir,"pop_0.Rda")
save(pop,file = pop.i.dir)

(t.pop <- pop.all[3,])
(t.rf.cv <- rf.cv(t.pop,seeds = 1,dir,folds=1:10))

# 3.2 Evolve the population X generations (X = [iterations])
for (i in 1:iterations){
  print(paste("Evolving: iteration",i))
  # load data from previous iteration
  record.dir <- paste0(evolve.dir,"record.Rda",sep="")
  load(record.dir)
  iter <- (max(record$iteration))
  pop.i.dir <- paste0(evolve.dir,"pop_",iter,".Rda")
  pop.all.dir <- paste0(evolve.dir,"pop_all_",iter,".Rda")
  load(pop.i.dir)
  load(pop.all.dir)
  
  # Update the iteration and generate a new population
  iter <- iter + 1
  pop <- evolve(pop,iter,dir=fold.dir,verbose=F)
  pop.all <- rbind(pop.all,pop) %>%
    unique()
  
  # Update the graph
  record <- record.update(pop,record)
  evo.plot(pop.all,pop,record,dir)
  
  # Save the results of this iteration
  pop.i.dir <- paste0(evolve.dir,"pop_",iter,".Rda")
  pop.all.dir <- paste0(evolve.dir,"pop_all_",iter,".Rda")
  save(pop,file=pop.i.dir)
  save(pop.all,file = pop.all.dir)
  save(record,file = record.dir)
  gc()
}
rm(fold.dir,evolve.dir,pop.all,pop,record,record.dir,pop.all.dir,pop.i.dir,i,iter)
gc()
print(paste("Genetic optimization completed at",Sys.time()))