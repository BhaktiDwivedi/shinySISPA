btest <- function(x){
  
  btest <- bartlett.test(zscore ~ sample_group, x)
  obs.pval <- btest$p.value
  obs.stat <- btest$statistic
  
  results <- list(bstat = obs.stat, pval = obs.pval)
  return(results)
}