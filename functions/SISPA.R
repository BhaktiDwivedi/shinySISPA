########## SISPA: Sample Integrated Set Profile Analysis
SISPA = function(feature=1,f1.df,f1.genes,f1.profile,f2.df=NULL,f2.genes=NULL,f2.profile=NULL,f3.df=NULL,f3.genes=NULL,f3.profile=NULL,cpt_data="var",cpt_method="BinSeg",cpt_max=60){
  
  #when only one feature sample profile is estimated
  if(feature==1){
    if(ncol(f1.df)<4){
      #stop("Zscore cannot be computed for samples < 3!")
    }
  } else if (feature==2){
    #zscores can not be computed if number of samples <3
    if(ncol(f1.df)<4 | ncol(f2.df)<4){
      stop("Zscore cannot be computed for samples < 3!")
    }
  } else{
    if(ncol(f1.df)<4 | ncol(f2.df)<4 | ncol(f3.df) <4){
      stop("Zscore cannot be computed for samples < 3!")
    }
  }
  
  #loop over each feature
  for(f in 1:feature){
    #sample enrichment scores: GSVA/ZSCORES/Modifiled ZSCORES
    #when number of genes is less than 3
    if (length(get(paste("f",f,".genes",sep="")))==0){
      #compute zscores for the gene set
      scores <- callMZSCORES(get(paste("f",f,".df",sep="")))
    }else if (length(get(paste("f",f,".genes",sep="")))>=1 | length(get(paste("f",f,".genes",sep="")))<3){
      scores <- callZSCORE(get(paste("f",f,".df",sep="")))
    }else{
      #when number of genes more than or equal to 3
      #compute zscores using GSVA
      scores <- callGSVA(get(paste("f",f,".df",sep="")),get(paste("f",f,".genes",sep="")))
    }
    #determine direction based on gene.profile
    if(get(paste("f",f,".profile",sep="")) == "up"){
      #increased zscore values are desirable
    }else{
      #decreased zscore values are desirable
      #Thus multiplied by -1 to obtain the desired outcome
      scores$NegOneValue_zscore <- as.numeric(as.character(scores$zscore)) * -1
      scores <- scores[,-c(2)]
      colnames(scores)[2] <- "zscore"
    }
    #save the feature profile scores
    assign(paste("feature",f,sep=""),scores)
    
    if(f==3){
      #sum the scores over the two features
      feature_1_2 <- merge(feature1,feature2,by="sample",sort=FALSE)
      summed_scores <- merge(feature_1_2,feature3,by="sample",sort=FALSE)
      summed_scores$zscores <-  as.numeric(as.character(summed_scores$zscore)) + as.numeric(as.character(summed_scores$zscore.x)) + as.numeric(as.character(summed_scores$zscore.y))
      summed_scores <- summed_scores[,c(1,ncol(summed_scores))]
    }else if (f==2){
      #sum the scores over the two features
      summed_scores <- merge(feature1,feature2,by="sample",sort=FALSE)
      summed_scores$zscores <-  as.numeric(as.character(summed_scores$zscore.x)) + as.numeric(as.character(summed_scores$zscore.y))
      summed_scores <- summed_scores[,c(1,ncol(summed_scores))]
    }else{
      summed_scores <- scores
    }
  }

    cpt_on_samples <- cptSamples(summed_scores,cpt_data,cpt_method,cpt_max)
    sispa_output = list(cpt_out=cpt_on_samples$cpt_out, cpt_plot=cpt_on_samples$cpt_plot, all_cutoffs=cpt_on_samples$all_cutoffs)
    #sample profile distribution
    if(is.null(cpt_on_samples$cpt_out)){
      warning("No changepoints identified in the data set!")
      return (0)
    }else{
      return(sispa_output)
    }
    
}


##########Sort the data
sortData = function(x,i,b) {
  if(missing(x)){
    stop("input data is missing!")
  }
  if(missing(b)){
    b=FALSE
  }
  if(missing(i)){
    i=1
  }
  return( x[ order(x[,i], decreasing=b), ] )
}

##########Run the Modified Z-score code
## For a single gene analysis
# x : A vector, obs for a particular gene
callMZSCORES <- function(x) {
  if(missing(x)){
    stop("input expression data missing!")
  }
  x_tr <- data.frame(colnames(x),(t(x)),row.names=NULL)
  x_std <- (x_tr[,2]-median(x_tr[,2]))/mad(x_tr[,2])
  x_final <- data.frame(x_tr[,1],x_std)
  colnames(x_final) <- c("sample","zscore")
  return(x_final)
}

##########Run the ZSCORE code
callZSCORE = function(x) {
  if(missing(x)){
    stop("input expression data missing!")
  }
  #compute row zscores
  zscores_results <- data.frame(colnames(x),scale(t(x)),row.names=NULL)
  colnames(zscores_results)[1] <- "sample"
  #compute the added zscores, as applicable and rename the column names
  if(ncol(zscores_results)==2){
    colnames(zscores_results)[2] <- "zscore"
  }else{
    zscores_results$zscore <- zscores_results[,2] + zscores_results[,3]
    zscores_results <- zscores_results[,-c(2,3)]
  }
  return (zscores_results)
}

##########Run the GSVA code
callGSVA = function(x,y) {
  if(missing(x)){
    stop("input expression data missing!")
  }
  if(missing(y)){
    stop("input gene set missing!")
  }
  genes <- list(set1=y)
  gsva.results <- gsva(x, genes, method="zscore", rnaseq=FALSE,verbose=FALSE,parallel.sz=2)
  tr_gsva.results <- t(gsva.results)
  #label column names
  tr_result_zscore <- cbind(samples = rownames(tr_gsva.results), tr_gsva.results)
  colnames(tr_result_zscore) <- c("sample","zscore")
  rownames(tr_result_zscore) <- NULL
  return ( as.data.frame(tr_result_zscore))
}

#########Sample profile identifier analysis
cptSamples <- function (x,cpt_data,cpt_method,cpt_max){
  ## Identify the changepoints for the data
  # x : data within which you wish to identify the changepoints
  # i : numeric column index of the data frame to sort it by
  # b : sorting order, ascending (FALSE) or descending (TRUE)
  perDiffcpt = function(x,cpt_data,cpt_method,cpt_max){
    max <- length(x)/2
    if (cpt_method == "AMOC") {
      
      if(cpt_data == "mean"){
        changepoints <- cpt.mean(x,method=cpt_method,Q=1)
      }else if(cpt_data == "var"){
        changepoints <- cpt.var(x,method=cpt_method,Q=1)
      }else{
        changepoints <- cpt.meanvar(x,method=cpt_method,Q=1)
      }
      
    }else if (cpt_method == "PELT" || cpt_method == "SegNeigh" || cpt_method == "BinSeg") {
      
      if(max > cpt_max){
        if(cpt_data == "mean"){
          changepoints <- cpt.mean(x,method=cpt_method,Q=cpt_max)
        } else if (cpt_data == "var") {
          changepoints <- cpt.var(x,method=cpt_method,Q=cpt_max)
        }else {
          changepoints <- cpt.meanvar(x,method=cpt_method,Q=cpt_max)
        }
      }else if(max <= cpt_max){
        if(cpt_data == "mean"){
          changepoints <- cpt.mean(x,method=cpt_method,Q=max)
        } else if (cpt_data == "var") {
          changepoints <- cpt.var(x,method=cpt_method,Q=max)
        }else{
          changepoints <- cpt.meanvar(x,method=cpt_method,Q=max)
        }
      }
      
    }
    return ( cpts(changepoints) )
  }
  ## Add the estimated changepoint locations to the data frame
  # x : A data frame containing the data used for change point estimation
  # y : vector containing the changepoint locations for the data supplied
  cptAdd = function(x,y){
    level=character()
    l = 1
    for(i in 1:length(y) ) {
      if(i==1){
        for(j in 1:y[i]){
          level[j]=l
        }
      }else{
        a <- (y[i-1])+1
        for(j in a:y[i]){
          level[j]=l
        }
      }
      if(i==length(y)){
        l=1000
        a <- y[i]+1
        for(j in a:nrow(x)){
          level[j]=1000
        }
      }
      
      l=l+1
    }
    return(as.numeric(level))
  }
  ## Locate all values for the estimated change point locations
  # x : A data frame containing the data values used for change point estimation
  # y : Vector containing the changepoint locations for the data supplied
  # z : Column index of the data values used for changepoint estimation
  cptLocate_all = function(x,y,z){
    cutoff_all <- character()  
    for(i in 1:length(y) ) {
      cutoff_all[i] <- x[y[i],][z]
    }
    cpts_plot_cutoff_all <- cutoff_all[!is.na(cutoff_all)]
    cpts_plot_cutoff_all <- as.numeric(cpts_plot_cutoff_all)
    cpts_plot_cutoff_all <- round(cpts_plot_cutoff_all,digits=2)
    return(cpts_plot_cutoff_all)
  }
  ######### END: sub-functions used by the cptSamples main function #########
  
  #read sample and sample zscores
  st <- x[,c(1,2)]
  colnames(st)[2] <- "zscore"
  #correct for empty cells or NAs
  st_rmv_na <- st[!is.na(st[ncol(st)]), ]
  #check if the zscores are numeric and sort them
  st_rmv_na$zscore <- as.numeric(as.character(st_rmv_na$zscore))
  st_rmv_na_sort = sortData(st_rmv_na,ncol(st_rmv_na),'TRUE')
  
  #sample profile identification
  if(nrow(st_rmv_na_sort)<10){
    warning("Number of data points not sufficient!")
    cpt_out <- NULL
    cpt_plot <- NULL
    all_cutoffs <- NULL
    cpt_results <- list(cpt_out = cpt_out,cpt_plot = cpt_plot, all_cutoffs=all_cutoffs)
  }else{
        #identify the changepoints
        cpts = perDiffcpt(st_rmv_na_sort$zscore,cpt_data,cpt_method,cpt_max)
        #check for the number of changepoints identified
        if(length(cpts)<1){
          warnings("No changepoints identified in the data set!")
          cpt_out <- NULL
          cpt_plot <- NULL
          all_cutoffs <- NULL
          cpt_results <- list(cpt_out = cpt_out,cpt_plot = cpt_plot, all_cutoffs=all_cutoffs)
        }else{
          #add estimated changepoint locations to the data
          changepoint <- cptAdd(st_rmv_na_sort,cpts)
          
          #append changepoints to the original data frame
          cpt_out <- data.frame(st_rmv_na_sort,changepoint)
          all_cutoffs <- cptLocate_all(cpt_out,cpts,2)
          cpt_out_sort <- sortData(cpt_out,ncol(cpt_out)-1,'FALSE' )
          cpt_plot <- cpt_out_sort$zscore
          
          #generate the data frame with all changepoints
          #format the data file for reading usability
          #any changepoint other than 1 is designated as '0'
          #plot changepoints within the data identified
          cpt_out$sample_group <- cpt_out$changepoint
          cpt_out [, ncol(cpt_out) ][ cpt_out [ ,ncol(cpt_out) ] >1 ] <- "0"
          cpt_out [, ncol(cpt_out)-1 ][ cpt_out [ ,ncol(cpt_out)-1 ] == 1000 ] <- "NA"
          rownames(cpt_out) <- NULL
          cpt_results <- list(cpt_out = cpt_out,cpt_plot = cpt_plot, all_cutoffs=all_cutoffs)
        }
  }
  return(cpt_results)
}
