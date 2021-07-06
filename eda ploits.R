library(ggplot2)
library(ggpubr)


pdf("xy_blocks.pdf" ,onefile = TRUE,width = 17, height = 13)

for (t in unique(df.eye$trial)){
  
  to.plot <- df.eye[df.eye$trial == t,]
  
  for (i in seq(5)){
    
    to.plot1 <- to.plot %>% filter(NUMBLOCK==i)
    xvalues <- (to.plot1 %>% filter(!is.na(message)) %>% select(time))$time
    
    pupil <- ggplot(to.plot1,aes(x=time,y=pupil))+
                geom_path() +
                geom_vline(xintercept = xvalues,linetype="dashed")+
                ggtitle(sprintf("Pupil Dilation, Trial: %s, Block: %s",t,i))+
                xlab(" ")+
                ylab(" ")+
                theme(plot.title = element_text(size = 15),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
    
    X <- ggplot(to.plot1,aes(x=time,y=right.gaze_direction_normalized.x))+
              geom_path()+
              geom_vline(xintercept = xvalues,linetype="dashed")+
              ggtitle("X location")+
              xlab(" ")+
              ylab(" ")+
              theme(plot.title = element_text(size = 15),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())
    
    Y <- ggplot(to.plot1,aes(x=time,y=right.gaze_direction_normalized.y))+
              geom_path()+
              geom_vline(xintercept = xvalues,linetype="dashed")+
              ggtitle("Y location")+
              xlab(" ")+
              ylab(" ")+
              theme(plot.title = element_text(size = 15))
    
    block <- ggarrange(pupil,X,Y,nrow = 3)
    print(block)
    
  }
  
  xvalues <- (to.plot %>% filter(!is.na(message)) %>% select(NUMBLOCK,time))
  trial <-ggplot(to.plot,aes(time,pupil))+
    geom_path()+
    geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
    ggtitle("Trial: ",t)
  print(trial)
}
dev.off()






#################################  OVERALL   ###################################
pdf("average.pdf" ,onefile = TRUE,width = 17, height = 13)

for (i in seq(5)){
  data = df.eye[df.eye$NUMBLOCK == i,]
  xvalues <- (data %>% filter(!is.na(message) & trial ==3)  %>% select(time))$time
  avg.block <- ggplot(data,aes(x=time,y=pupil))+
    geom_smooth()+
    geom_vline(xintercept = xvalues,linetype="dashed")+
    ggtitle(sprintf("Average Pupil Dilation, Block: %s", i))
  print(avg.block)
}

xvalues <- (df.eye %>% filter(!is.na(message) & trial == 3) %>% select(NUMBLOCK,time))
avg <- ggplot(df.eye,aes(time,pupil))+
  geom_smooth()+
  geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
  ggtitle(ggtitle(sprintf("Average Pupil Dilation")))
print(avg)

dev.off()

########################### PER GLOBAL CONDITION ###############################
pdf("globals.pdf" ,onefile = TRUE,width = 17, height = 13)

### STANDARD
standard <- df.eye[(df.eye$GLOBAL== "Standard"),]
trial.for.x <- standard$trial[1]

for (i in seq(5)){
  data = standard[standard$NUMBLOCK == i,]
  xvalues <- (data %>% filter(!is.na(message) & trial == trial.for.x)  %>% select(time))$time
  plt <-ggplot(data,aes(x=time,y=pupil))+
    geom_smooth()+
    geom_vline(xintercept = xvalues,linetype="dashed")+
    ggtitle(sprintf("Average Pupil Dilation, Condition: Standard, Block: %s", i))
  print(plt)
}

xvalues <- (standard %>% filter(!is.na(message) & trial == trial.for.x) %>% select(NUMBLOCK,time))
plt<-ggplot(standard,aes(time,pupil))+
  geom_smooth()+
  geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
  ggtitle(ggtitle(sprintf("Average Pupil Dilation in Global Standard")))
print(plt)


### DEVIANT
deviant =  df.eye[(df.eye$GLOBAL== "Deviant"),]
trial.for.x <- deviant$trial[1]

for (i in seq(5)){
  data = deviant[deviant$NUMBLOCK == i,]
  xvalues <- (data %>% filter(!is.na(message) & trial == trial.for.x)  %>% select(time))$time
  plt <-ggplot(data,aes(x=time,y=pupil))+
    geom_smooth()+
    geom_vline(xintercept = xvalues,linetype="dashed")+
    ggtitle(sprintf("Average Pupil Dilation, Condition: Deviant, Block: %s", i))
  print(plt)
}
xvalues <- (deviant %>% filter(!is.na(message) & trial == trial.for.x) %>% select(NUMBLOCK,time))
plt <- ggplot(deviant,aes(time,pupil))+
  geom_smooth()+
  geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
  ggtitle(ggtitle(sprintf("Average Pupil Dilation in Global Deviant")))
print(plt)



########################### PER GLOBAL/LOCAL COMBINATION #######################

standard$local.group <- as.character(rep(NA,length(standard$trial)))
deviant$local.group <- as.character(rep(NA,length(deviant$trial)))

for(i in unique(standard$trial)){
  condition<- as.character(standard[standard$trial==i & standard$NUMSQUARE==5 & standard$NUMBLOCK==5 & standard$message=="target",]$LOCAL[1])
  #print(rep(condition,length(standard[standard$trial==i,]$trial)))
  standard[standard$trial==i,]$local.group <- rep(condition,length(standard[standard$trial==i,]$trial))
}

for(i in unique(deviant$trial)){
  condition<- as.character(deviant[deviant$trial==i & deviant$NUMSQUARE==5 & deviant$NUMBLOCK==5 & deviant$message=="target",]$LOCAL[1])
  #print(rep(condition,length(standard[standard$trial==i,]$trial)))
  deviant[deviant$trial==i,]$local.group <- rep(condition,length(deviant[deviant$trial==i,]$trial))
}

standard$local.group <-as.factor(standard$local.group)
deviant$local.group <-as.factor(deviant$local.group)

standard.square <- standard[standard$local.group=="Square",]
standard.diamond <- standard[standard$local.group=="Diamond",]
deviant.square<- deviant[deviant$local.group=="Square",]
deviant.diamond<- deviant[deviant$local.group=="Diamond",]


### STANDARD.SQUARE
trial.for.x <- standard.square$trial[1]

for (i in seq(5)){
  data = standard.square[standard.square$NUMBLOCK == i,]
  xvalues <- (data %>% filter(!is.na(message) & trial == trial.for.x)  %>% select(time))$time
  plt<-ggplot(data,aes(x=time,y=pupil))+
    geom_smooth()+
    geom_vline(xintercept = xvalues,linetype="dashed")+
    ggtitle(sprintf("Average Pupil Dilation, Condition: Global Standard / Local Square, Block: %s", i))
  print(plt)
}

xvalues <- (standard.square %>% filter(!is.na(message) & trial == trial.for.x) %>% select(NUMBLOCK,time))
plt<-ggplot(standard.square,aes(time,pupil))+
  geom_smooth()+
  geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
  ggtitle(ggtitle(sprintf("Average Pupil Dilation in Global Standard / Local Square")))
print(plt)
### STANDARD.DIAMOND
trial.for.x <- standard.diamond$trial[1]

for (i in seq(5)){
  data = standard.diamond[standard.diamond$NUMBLOCK == i,]
  xvalues <- (data %>% filter(!is.na(message) & trial == trial.for.x)  %>% select(time))$time
  plt<-ggplot(data,aes(x=time,y=pupil))+
    geom_smooth()+
    geom_vline(xintercept = xvalues,linetype="dashed")+
    ggtitle(sprintf("Average Pupil Dilation, Condition: Global Standard / Local Deviant, Block: %s", i))
  print(plt)
}

xvalues <- (standard.diamond %>% filter(!is.na(message) & trial == trial.for.x) %>% select(NUMBLOCK,time))
plt<-ggplot(standard.diamond,aes(time,pupil))+
  geom_smooth()+
  geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
  ggtitle(ggtitle(sprintf("Average Pupil Dilation in Global Standard / Local Deviant")))
print(plt)

### DEVIANT.SQUARE
trial.for.x <- deviant.square$trial[1]

for (i in seq(5)){
  data = deviant.square[deviant.square$NUMBLOCK == i,]
  xvalues <- (data %>% filter(!is.na(message) & trial == trial.for.x)  %>% select(time))$time
  plt<-ggplot(data,aes(x=time,y=pupil))+
    geom_smooth()+
    geom_vline(xintercept = xvalues,linetype="dashed")+
    ggtitle(sprintf("Average Pupil Dilation, Condition: Global Deviant / Local Square, Block: %s", i))
  print(plt)
}

xvalues <- (deviant.square %>% filter(!is.na(message) & trial == trial.for.x) %>% select(NUMBLOCK,time))
plt<-ggplot(deviant.square,aes(time,pupil))+
  geom_smooth()+
  geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
  ggtitle(ggtitle(sprintf("Average Pupil Dilation in Global Deviant / Local Square")))
print(plt)

### DEVIANT.DIAMOND
trial.for.x <- deviant.diamond$trial[1]

for (i in seq(5)){
  data = deviant.diamond[deviant.diamond$NUMBLOCK == i,]
  xvalues <- (data %>% filter(!is.na(message) & trial == trial.for.x)  %>% select(time))$time
  plt<-ggplot(data,aes(x=time,y=pupil))+
    geom_smooth()+
    geom_vline(xintercept = xvalues,linetype="dashed")+
    ggtitle(sprintf("Average Pupil Dilation, Condition: Global Deviant / Local Diamond, Block: %s", i))
  print(plt)
}

xvalues <- (deviant.diamond %>% filter(!is.na(message) & trial == trial.for.x) %>% select(NUMBLOCK,time))
plt<-ggplot(deviant.diamond,aes(time,pupil))+
  geom_smooth()+
  geom_vline(data=xvalues,aes(xintercept = time,colour=NUMBLOCK))+
  ggtitle(ggtitle(sprintf("Average Pupil Dilation in Global Deviant / Local Diamond")))
print(plt)

dev.off()

