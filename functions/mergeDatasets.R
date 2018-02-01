mergeDatasets = function(x1,x2,sel){
  full_dat <- data.frame(rownames(x1),x1)
  rownames(full_dat) <- NULL
  colnames(full_dat)[1] <- "sample"
  results <- x2[,c(1,ncol(x2))]
  merged_data <- merge(results,full_dat,by=c("sample"), sort=FALSE)
  grp1 <- subset(merged_data, merged_data$sample_group==1)
  grp1_dt <- data.table(grp1)
  fun <- as.character(sel)
  grp1 <- as.data.frame(grp1_dt[,get(isolate(fun))])
  final_data1 <- data.frame(value=unlist(grp1), row.names=outer(rownames(grp1), colnames(grp1), paste0))
  final_data1$ind <- "With Profile Activity"
  final_data1$colors <- "orange"
  final_data1$border <- "orange"
  mean1 <- mean(final_data1$value)
  grp2 <- subset(merged_data, merged_data$sample_group==0)
  grp2_dt <- data.table(grp2)
  grp2 <- as.data.frame(grp2_dt[,get(isolate(fun))])
  final_data2 <- data.frame(value=unlist(grp2), row.names=outer(rownames(grp2), colnames(grp2), paste0))
  final_data2$ind <- "Without Profile Activity"
  final_data2$colors <- "white"
  final_data2$border <- "grey"
  mean2 <- mean(final_data2$value)
  final_data <- rbind(final_data1, final_data2)
  final_data_dt <- data.table(final_data)
  ordered_list <- final_data_dt[order(ind,value),]
  
  datasets <- list(ordered_list=ordered_list,mean1=mean1,mean2=mean2,fun=fun)
}
