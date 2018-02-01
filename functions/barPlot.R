barPlot = function(x,m1,m2,ylabel,titlelable){
  par(bg='white')
  bp <- barplot(x$value, col=x$colors, border = x$border, xlab = "Samples", ylab=ylabel, main=titlelable, cex.names=2.0, cex.axis = 1.0, cex.lab=1.5)
  box(col='white')
  abline(h=m1,lty=2,col='orange')
  abline(h=m2,lty=2,col='grey')
  return(bp)
}
