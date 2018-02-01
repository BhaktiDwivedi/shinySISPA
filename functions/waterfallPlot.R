waterfallplot = function(x){
  if(missing(x)){
    stop("input data is missing!")
  }
  colnames(x)[1] <- "samples"
  colnames(x)[ncol(x)] <- "sample_group"
  #create a start column
  x[,ncol(x)+1] <- 0
  #define global variable
  #select and modify the data for readability
  read_data_sort <- x[ order(x[,2], decreasing=FALSE), ]
  #arrange the columns
  arrange_read_data_sort <- data.frame(read_data_sort[,1],read_data_sort[,5],read_data_sort[,2],read_data_sort[,4])
  colnames(arrange_read_data_sort)<-c("samples","Start","End","sample_group")
  arrange_read_data_sort[,1] <- factor(arrange_read_data_sort[,1], levels=arrange_read_data_sort[,1])
  arrange_read_data_sort$id <- seq_along(arrange_read_data_sort$End)
  arrange_read_data_sort$type <- ifelse(arrange_read_data_sort$sample_group==1, "grp1", "other")
  arrange_read_data_sort <- arrange_read_data_sort[,c(5,1,6,2,3)]
  #generate the waterfall plot
  wf <- ggplot(arrange_read_data_sort, aes(colour=type,samples,fill=type)) + scale_fill_manual(values=c("#FF8000","white")) + scale_colour_manual(values=c("#FF8000","grey")) + geom_rect(aes(x=samples,xmin=id-0.35,xmax=id+0.35,ymin=End,ymax=Start))  
  wf <- wf + labs(x="", y="", fill="")
  wf <- wf + theme(legend.position = "none")
  wf <- wf + theme(axis.text.y = element_text(colour="black",size=16.0))
  wf <- wf + theme(axis.text.x = element_blank())
  wf <- wf + theme(axis.title.x = element_text(colour="black",face="bold",size=20.0))
  wf <- wf + theme(axis.title.y = element_text(colour="black",face="bold",size=20.0))
  wf <- wf + theme(panel.background = element_rect(fill="NA"))
  wf <- wf + theme(panel.border = element_rect(colour = "black", fill="NA"))
  wf <- wf + theme(panel.grid.major.y = element_line(colour="NA"))
  wf <- wf + theme(panel.grid.minor = element_line(colour = "NA"))
  wf <- wf + theme(axis.ticks.x = element_blank())
  wf <- wf + theme(axis.ticks.y=element_line(colour="black"))
  wf <- wf + ylab("SISPA Profile ZScore\n")
  wf <- wf + xlab("Samples\n")
  #wf <- wf + scale_fill_manual(name = "Sample Groups",
  #                             label = c("w Profile Activity", "w/o Profile Activity"),
  #                             values = c("#FF8000", "grey"))
  return(wf)
}
