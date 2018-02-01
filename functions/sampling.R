sampling <- function(x, samp, reps){
  sampledData<-list()
  #sample columns with replacement
  #samples number of times
  for ( i in 1:reps){
    sampledData[[i]]<-sample(x, samp, replace=TRUE)
  }
  #hist(sapply(sampledData, mean), xlab="Sampled Data", main="Histogram of Sampled data")
  return (sampledData)
  
 }