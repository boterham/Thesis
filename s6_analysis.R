print(paste("Prediction of",dir,"started at",Sys.time()))

df.dir <- paste0(dir,"prediction/results.RDa")
load(df.dir)

defor.name <- paste0(dir,"prediction/Annual Deforestation.png",sep="")
defor.rate.name <- paste0(dir,"prediction/Annual Deforestation Rates.png",sep="")
forest.cover.name <- paste0(dir,"prediction/Annual Forest Cover.png",sep="")

defor <- ggplot(data=df,aes(x=year,y=(deforested.p))) +
  geom_line(colour="orange") +
  geom_point(colour="orange") +
  geom_line(data=df,colour="blue",aes(x=year,y=(deforested.r))) + 
  geom_point(data=df,colour="blue",aes(x=year,y=(deforested.r))) +
  expand_limits(y = 0)
ggsave(defor.name, scale=1,dpi=300,limitsize=F,units="cm")

defor.rate <- ggplot(data=df,
                     aes(x=year,y=(deforested.p/forest.p))) +
  geom_line(colour="orange") +
  geom_point(colour="orange") +
  geom_line(data=df,colour="blue",aes(x=year,y=(deforested.r/forest.r))) +
  geom_point(data=df,colour="blue",aes(x=year,y=(deforested.r/forest.r))) +
  expand_limits(y = 0)
ggsave(defor.rate.name, scale=1,dpi=300,limitsize=F,units="cm")

forest.cover <- ggplot(data=df,aes(x=year,y=forest.p)) +
  geom_line(colour="orange") +
  geom_point(colour="orange") +
  geom_line(data=df,colour="blue",aes(x=year,y=forest.r)) + 
  geom_point(data=df,colour="blue",aes(x=year,y=(forest.r))) +
  expand_limits(y = 0)
ggsave(forest.cover.name, scale=1,dpi=300,limitsize=F,units="cm")

defor
defor.rate
forest.cover

gc()
print(paste("Prediction completed at",Sys.time()))