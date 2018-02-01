freqplot = function(x){
  w = table(x$sample_group)
  count_data = as.data.frame(w)
  colnames(count_data) <- c("Var", "value")
  count_data$sample_group <- c("Without Profile\n Activity","With Profile\n Activity")
  count_data <- count_data[,-c(1)]
  
  DF1 <- melt(count_data, id.var="sample_group")
  bp <- ggplot(DF1,  aes(x=sample_group,y=value,fill=sample_group))
  bp <- bp + geom_bar(stat="identity",position="stack",width=1.0, colour="white")
  bp <- bp + scale_fill_manual(breaks = c("Inactive","Active"), values = c("orange", "grey"))
  bp <- bp + theme(axis.title.y = element_text(colour="black", face="plain", size=12.0))
  bp <- bp + theme(panel.background = element_rect(fill="NA"))
  bp <- bp + theme(panel.border = element_rect(colour = "black", fill="NA"))
  bp <- bp + theme(panel.grid.major.y = element_line(colour="NA"))
  bp <- bp + theme(panel.grid.minor = element_line(colour = "NA"))
  bp <- bp + theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", face="plain", size=12.0))
  bp <- bp + theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", size=12.0))
  bp <- bp + ylab("# of Samples\n")
  bp <- bp + xlab("")
  bp <- bp + theme(legend.position = "none")
  return(bp)
}