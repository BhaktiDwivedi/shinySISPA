cptsPlot <- function(x,y,LABEL){
  if(missing(x)){
    stop("data is missing!")
  }
  if(missing(y)){
    stop("changepoint cutoffs are missing!")
  }
  x <- sort(x,FALSE)
  y <- sort(y,TRUE)
  
  par(bg = "white")
  cptplot <- plot(x, type='p', lty=3, lwd=1, main="", xlab="Samples", ylab=LABEL, cex=2.0, pch=1, xlim=c(0,length(x)))
  
  for(i in 1:length(y)) {
    if(i==1) {
      abline(h=y[i],lty=2,col='red')
      text(y[i],y[i],y[i], cex=1.0, pos=4, col="orange")
    }else{
      ## display in terms of sample group profiles
      if(y[i]<0){
        abline(h=y[i],lty=2,col='grey')
        text(y[i],y[i],y[i], cex=1.0, pos=4, col="grey")
      } else{
        abline(h=y[i],lty=2,col='grey')
        text(y[i],y[i],y[i], cex=1.0, pos=4, col="grey")
      }
    }
  }
  
  return(cptplot)
}
