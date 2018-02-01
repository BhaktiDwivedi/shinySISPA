mergeDatasets_boxplot = function(x1,x2){
  full_dat <- data.frame(rownames(x1),x1)
  rownames(full_dat) <- NULL
  colnames(full_dat)[1] <- "sample"
  results <- x2[,c(1,ncol(x2))]
  merged_data <- merge(results,full_dat,by=c("sample"), sort=FALSE)
  grp1 <- subset(merged_data, merged_data$sample_group==1)
  grp1 <- as.data.frame(grp1[,-c(1,2)])
  final_data1 <- data.frame(value=unlist(grp1), row.names=outer(rownames(grp1), colnames(grp1), paste0))
  final_data1$ind <- "With Profile Activity"
  grp2 <- subset(merged_data, merged_data$sample_group==0)
  grp2 <- as.data.frame(grp2[,-c(1,2)])
  final_data2 <- data.frame(value=unlist(grp2), row.names=outer(rownames(grp2), colnames(grp2), paste0))
  final_data2$ind <- "Without Profile Activity"
  final_data <- rbind(final_data1, final_data2)
  
  return(final_data)
}
