ttest <- function(x){
  
  btest <- bartlett.test(zscore ~ sample_group, x)
  obs.pval <- btest$p.value
  obs.tstat <- btest$statistic
  
  results <- list(bstat = obs.stat, pval = obs.pval)
  return(results)
}