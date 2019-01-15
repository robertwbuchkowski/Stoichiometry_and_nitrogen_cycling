Figure1_gradient <-function(graddata, custylab = expression(X~w.r.t.~Microbial~C:N~(Delta*X/Delta*mu))){
  checkcor = cbind(graddata[qc_err2,-8],qualitycontrol[qc_err2,])
  Sclist = as.character(seq(1,8,1))
  checkcor = subset(checkcor, !is.na(M) & !is.na(Dv))
  table(checkcor$Scenario)
  checkcor2 = subset(checkcor, Scenario == Sclist[1])[1:7000,]
  for(i in 2:8) {
    checkcor2 = rbind(checkcor2, subset(checkcor, Scenario == Sclist[i])[1:7000,])
  }
  
  rm(checkcor)
  
  checkcor3 = matrix(NA, nrow=7, ncol=2)
  for(i in 1:7) {
    checkcor3[,1] = sum(table(checkcor2[,i]>0,checkcor2[,(i+49)]>0)[c(1,4)]) 
    checkcor3[,2] = sum(table(checkcor2[,i]>0,checkcor2[,(i+49)]>0))
    print(table(checkcor2[,i]>0,checkcor2[,(i+49)]>0))
  }
  print(paste0("Proportion sharing the same sign is ",colSums(checkcor3)[1]/colSums(checkcor3)[2]))
  
  togo = checkcor2[,c(1:7,106)]
  
  togo = tidyr::gather(togo, key=Pool, value=Change, -Scenario)
  
  togo = subset(togo, !is.na(Scenario))
  
  togo["PoolSc"] = paste0(togo$Pool, togo$Scenario)
  
  func.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)
  
  check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
  check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)
  
  func.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                        ifelse(check2[,1]==-1, "Decrease","Increase")))
  
  prod1 = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
    xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
    geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
    geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=func.summary) +
    geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=func.summary) +
    scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                       values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
    scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
    ylab(custylab)  + coord_cartesian(ylim=c(-5,10)) + 
    geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + theme(panel.grid.major = element_blank(), 
                                                                                             panel.grid.minor = element_blank(),
                                                                                             panel.background = element_blank(), 
                                                                                             axis.line = element_line(colour = "black"))
  
  return(prod1)
  
}