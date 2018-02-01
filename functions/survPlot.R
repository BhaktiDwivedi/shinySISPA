survPlot = function(clinData,sampGroups){
  
    mergedData <- merge(clinData, sampGroups, by="sample")
    combData <- mergedData[!(is.na(mergedData$OS_censor) | mergedData$OS_surtime=="."), ]
    combData <- data.frame(as.numeric(combData$OS_surtime), as.numeric(combData$OS_censor), combData$sample_group)
    colnames(combData) <- c("time", "status", "sample_group")
    #combData$sample_group[combData$sample_group==0]  <- "Without Profile Activity"
    #combData$sample_group[combData$sample_group==1]  <- "With Profile Activity"
    
    exp=as.factor(combData$sample_group)
    #exp.ref="Without Profile Activity"
    exp.ref="0"
    color=c("grey20","orange")
    Dpoint=10
    Dlabel = "T"
    KM_xlab= "Time (days)"
    
    exp <- relevel(exp, ref=exp.ref)
    cvt.length <- length(levels(exp))
    char.cvt<-levels(exp)

    ##op <- par(mfrow = c(1, cvt.length),mar = c(5,4,2,2), oma=c(4,0,0,0))
    surv <- survfit(Surv(time, status) ~ sample_group, data = combData)
    len<-floor(max(surv$time))
    pos<-seq(0,len, len/Dpoint)
    NR<-nrisk(surv, times=pos)

    plot(surv, lty =c(1,1), ylim=c(-0.05,1), xlim=c(-0.05*max(surv$time), 1.05*max(surv$time)),col=color, lwd=2, xlab= KM_xlab, ylab="Survival Probability", xaxt="n")
    axis(1,at=round(pos,2))
    for (n in 1: dim(NR)[1]){text(x=pos,y= 0,labels= NR[n,],cex=0.8,pos=3, offset=(-0.8*n), col=color[n],font=3)}
    text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=0.8, font=4)
    
    A<-survdiff(Surv(time, status) ~ sample_group, data = combData)
    p.valdec <- 1 - pchisq(A$chisq, length(A$n) - 1)
    p.val <- round(1 - pchisq(A$chisq, length(A$n) - 1),3)
   
    legend(0,0.1*cvt.length, char.cvt,lty =c(1,1), col=c("grey20","orange"),bty = "n" ,cex=1,seg.len=2,lwd =2, title=paste("log-rank P = ", p.val, sep = ""))
}