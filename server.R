options(shiny.maxRequestSize=10000*1024^2)
options(bitmapType='cairo')


library(ggplot2)
library(GSVA)
library(genefilter)
library(changepoint)
library(data.table)
library(plyr)
library(shiny)
#library(survival)

source("functions/SISPA.R")
source("functions/filterVars.R")
source("functions/cptPlot.R")
source("functions/freqPlot.R")
source("functions/waterfallPlot.R")
#source("functions/survplot_pkg.R")
#source("functions/survPlot.R")
source("functions/mergeDatasets.R")
source("functions/mergeDatasets_boxplot.R")
source("functions/boxScatterplot.R")
source("functions/barPlot.R")
source("functions/boxPlot.R")
source("functions/mgsub.R")
#source("functions/btest.R")
#source("functions/sampling.R")

shinyServer(function(input, output, session) {

  # read in initial data input.txt i.e. a tab-delimited file containing the data
  # this file contains column corrosponding to samples
  # this file contains rows corrosponding to genes
  # the data directory of the app folder contains those example .txt files
  
        getData = reactive({
          
          if(input$analysisType == '1d'){
<<<<<<< HEAD
                
                if (input$f == 'ED'){
                  dataFile <- "data/KMS11_expvar_geneset_MM_compass_exp.txt"
                  dat <- read.csv(dataFile, header=T, sep="\t")
                } else if (input$f == 'LD'){
                  if (is.null(input$file)) 
                    return(NULL)
                  else{
                    # otherwise read the data and return the data.frame
                    dataFile <- input$file$datapath
                    dat <- read.csv(dataFile, header=T, sep="\t")
                  }
                }else{
                  return(NULL)
                }
                
                genelist = as.character(mgsub(c(";",":","-","_","|","?","@"," "), c(".",".",".",".",".",".",".","."), dat[,1], fixed=TRUE))
=======
            
              if (is.null(input$file)) {
                # if no data file is uploaded, return NULL
                data <- NULL
              } else {
                # otherwise read the data and return the data.frame
                dataFile <- input$file$datapath
                dat <- read.csv(dataFile, header=T, sep="\t")
                genelist = as.character(mgsub(c(";",":","-","_","|","?","@"), c(".",".",".",".",".",".","."), dat[,1], fixed=TRUE))
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
                sampleid = colnames(dat[,-c(1)])
                originalX = as.matrix(dat[,-c(1)])
                rownames(originalX) <- genelist
                
                data1 <- as.matrix(originalX)
                row_var_mat = rowVars(data1)
                x_rowvar <- data.frame(originalX,row_var_mat)
                colnames(x_rowvar)[ ncol(x_rowvar) ] <- "row_var"
                x_filter_zerovar = filterVars(x_rowvar,x_rowvar$row_var)
                x_filter_zerovar = x_filter_zerovar[,-c(ncol(x_filter_zerovar))]
                
                originalX = as.matrix(x_filter_zerovar)
                filtgenelist <- rownames(x_filter_zerovar)
                rownames(originalX) <- filtgenelist
                
                n <- genelist
                transposeX <- as.data.frame(t(dat[,-c(1)]))
                colnames(transposeX) <- NULL
                colnames(transposeX) <- n
                data = list(ori=originalX, trans=transposeX, genelist=genelist, filtgenelist=filtgenelist, geneid=length(genelist), sampleid=length(sampleid))
<<<<<<< HEAD
=======
              }
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
          }
          else {
            
            if (input$f1 == 'ED'){
              dataFile <- "data/IgH_tx_geneset_MM_compass_exp.txt"
              dat <- read.csv(dataFile, header=T, sep="\t")
            } else if (input$f1 == 'LD'){
              if (is.null(input$file1)) 
                return(NULL)
              else{
                # otherwise read the data and return the data.frame
                dataFile <- input$file1$datapath
                dat <- read.csv(dataFile, header=T, sep="\t")
              }
            }else{
              return(NULL)
            }
                genelist = as.character(mgsub(c(";",":","-","_","|","?","@"), c(".",".",".",".",".",".","."), dat[,1], fixed=TRUE))
                sampleid = colnames(dat[,-c(1)])
                originalX = as.matrix(dat[,-c(1)])
                rownames(originalX) <- genelist
                
                data1 <- as.matrix(originalX)
                row_var_mat = rowVars(data1)
                x_rowvar <- data.frame(originalX,row_var_mat)
                colnames(x_rowvar)[ ncol(x_rowvar) ] <- "row_var"
                x_filter_zerovar = filterVars(x_rowvar,x_rowvar$row_var)
                x_filter_zerovar = x_filter_zerovar[,-c(ncol(x_filter_zerovar))]
                
                originalX = as.matrix(x_filter_zerovar)
                filtgenelist <- rownames(x_filter_zerovar)
                rownames(originalX) <- filtgenelist
                
                n <- genelist
                transposeX <- as.data.frame(t(dat[,-c(1)]))
                #rownames(transposeX) <- sampleid
                colnames(transposeX) <- NULL
                colnames(transposeX) <- n
                data = list(ori=originalX, trans=transposeX, genelist=genelist, filtgenelist=filtgenelist, geneid=length(genelist), sampleid=length(sampleid))
              }
        })
        
        getData2 = reactive({
          
        if(input$analysisType == '2d' || input$analysisType == '3d'){  
          
          if (input$f2 == 'ED'){
            dataFile <- "data/IgH_tx_geneset_MM_compass_cnv.txt"
            dat <- read.csv(dataFile, header=T, sep="\t")
          } else if (input$f2 == 'LD'){
            if (is.null(input$file2)) 
              return(NULL)
            else{
            # otherwise read the data and return the data.frame
            dataFile <- input$file2$datapath
            dat <- read.csv(dataFile, header=T, sep="\t")
            }
            }else{
              return(NULL)
            }
            
            genelist = as.character(mgsub(c(";",":","-","_","|","?","@"), c(".",".",".",".",".",".","."), dat[,1], fixed=TRUE))
            sampleid = colnames(dat[,-c(1)])
            originalX = as.matrix(dat[,-c(1)])
            rownames(originalX) <- genelist
            
            data1 <- as.matrix(originalX)
            row_var_mat = rowVars(data1)
            x_rowvar <- data.frame(originalX,row_var_mat)
            colnames(x_rowvar)[ ncol(x_rowvar) ] <- "row_var"
            x_filter_zerovar = filterVars(x_rowvar,x_rowvar$row_var)
            x_filter_zerovar = x_filter_zerovar[,-c(ncol(x_filter_zerovar))]
            
            originalX = as.matrix(x_filter_zerovar)
            filtgenelist <- rownames(x_filter_zerovar)
            rownames(originalX) <- filtgenelist
            
            n <- genelist
            transposeX <- as.data.frame(t(dat[,-c(1)]))
            rownames(transposeX) <- sampleid
            colnames(transposeX) <- NULL
            colnames(transposeX) <- n
            data = list(ori=originalX, trans=transposeX, genelist=genelist, filtgenelist=filtgenelist, geneid=length(genelist), sampleid=length(sampleid))
            }
        
     })
        
      getData3 = reactive({
          
      if(input$analysisType == '3d'){  
            
<<<<<<< HEAD
        if (input$f3 == 'ED'){
          dataFile <- "data/KMS11_expvar_geneset_MM_compass_var.txt"
          dat <- read.csv(dataFile, header=T, sep="\t")
        } else if (input$f3 == 'LD'){
          if (is.null(input$file3)) 
            return(NULL)
          else{
            # otherwise read the data and return the data.frame
            dataFile <- input$file3$datapath
            dat <- read.csv(dataFile, header=T, sep="\t")
          }
        }else{
          return(NULL)
        }
        
=======
          if (is.null(input$file3)) {
            # if no data file is uploaded, return NULL
            data <- NULL
          } else {
            # otherwise read the data and return the data.frame
            dataFile <- input$file3$datapath
            dat <- read.csv(dataFile, header=T, sep="\t")
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
            genelist = as.character(mgsub(c(";",":","-","_","|","?","@"), c(".",".",".",".",".",".","."), dat[,1], fixed=TRUE))
            sampleid = colnames(dat[,-c(1)])
            originalX = as.matrix(dat[,-c(1)])
            rownames(originalX) <- genelist
            
            data1 <- as.matrix(originalX)
            row_var_mat = rowVars(data1)
            x_rowvar <- data.frame(originalX,row_var_mat)
            colnames(x_rowvar)[ ncol(x_rowvar) ] <- "row_var"
            x_filter_zerovar = filterVars(x_rowvar,x_rowvar$row_var)
            x_filter_zerovar = x_filter_zerovar[,-c(ncol(x_filter_zerovar))]
            
            originalX = as.matrix(x_filter_zerovar)
            filtgenelist <- rownames(x_filter_zerovar)
            rownames(originalX) <- filtgenelist
            
            n <- genelist
            transposeX <- as.data.frame(t(dat[,-c(1)]))
            rownames(transposeX) <- sampleid
            colnames(transposeX) <- NULL
            colnames(transposeX) <- n
            data = list(ori=originalX, trans=transposeX, genelist=genelist, filtgenelist=filtgenelist, geneid=length(genelist), sampleid=length(sampleid))
<<<<<<< HEAD
=======
          }
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
      }
    })
        
        output$inputData <- renderDataTable({
            if (is.null(getData()$trans)) return(NULL)
            else{
              if(ncol(getData()$trans) < 5){
                getData()$trans
                #dat$trans
              }
              else{
                getData()$trans
              }
            }
        })
        output$boxPlot <- renderPlot({
            if (is.null(getData()$trans)) return(NULL)
            else{
              if(ncol(getData()$trans) < 5){
                boxPlot(getData()$trans)
              }
              else{
                boxPlot(getData()$trans[,c(1:5)])
              }
            }
        })
        output$geneNumtext <- renderText({ 
            if (is.null(getData()$trans)) return(NULL)
            else{
              paste("Number of Genes: ", getData()$geneid, sep = "")
            }
        })     
        output$geneNumtextFiltered <- renderText({ 
          if (is.null(getData()$ori)) return(NULL)
          else{
            paste("Number of Genes with zero variance: ", getData()$geneid-length(getData()$filtgenelist), sep = "")
          }
        })
        output$sampNumtext <- renderText({ 
            if (is.null(getData()$trans)) return(NULL)
            else{
              paste("Number of Samples: ", getData()$sampleid, sep = "")
            }
        })
        
        output$inputData1 <- renderDataTable({
          if (is.null(getData()$trans)) return(NULL)
          else{
            if(ncol(getData()$trans) < 5){
              getData()$trans
            }
            else{
              getData()$trans
            }
          }
        })
        output$boxPlot1 <- renderPlot({
          if (is.null(getData()$trans)) return(NULL)
          else{
            if(ncol(getData()$trans) < 5){
              boxPlot(getData()$trans)
            }
            else{
              boxPlot(getData()$trans[,c(1:5)])
            }
          }
        })
        output$geneNumtext1 <- renderText({ 
          if (is.null(getData()$ori)) return(NULL)
          else{
            paste("Number of Genes: ", getData()$geneid, sep = "")
          }
        })
        output$geneNumtextFiltered1 <- renderText({ 
          if (is.null(getData()$ori)) return(NULL)
          else{
            paste("Number of Genes with zero variance: ", getData()$geneid-length(getData()$filtgenelist), sep = "")
          }
        })
        output$sampNumtext1 <- renderText({ 
          if (is.null(getData()$ori)) return(NULL)
          else{
            paste("Number of Samples: ", getData()$sampleid, sep = "")
          }
        })
        
        output$inputData2 <- renderDataTable({
          if (is.null(getData2()$trans)) return(NULL)
          else{
            if(ncol(getData2()$trans) < 5){
              getData2()$trans
            }
            else{
              getData2()$trans
            }
          }
        })
        output$boxPlot2 <- renderPlot({
          if (is.null(getData2()$trans)) return(NULL)
          else{
            if(ncol(getData2()$trans) < 5){
              boxPlot(getData2()$trans)
            }
            else{
              boxPlot(getData2()$trans[,c(1:5)])
            }
          }
        })
        output$geneNumtext2 <- renderText({ 
          if (is.null(getData2()$ori)) return(NULL)
          else{
            paste("Number of Genes: ", getData2()$geneid, sep = "")
          }
        })
        output$geneNumtextFiltered2 <- renderText({ 
          if (is.null(getData2()$ori)) return(NULL)
          else{
            paste("Number of Genes with zero variance: ", getData2()$geneid-length(getData2()$filtgenelist), sep = "")
          }
        })
        output$sampNumtext2 <- renderText({ 
          if (is.null(getData2()$ori)) return(NULL)
          else{
            paste("Number of Samples: ", getData2()$sampleid, sep = "")
          }
        })

        output$inputData3d_1 <- renderDataTable({
          if (is.null(getData()$trans)) return(NULL)
          else{
            if(ncol(getData()$trans) < 5){
              getData()$trans
            }
            else{
              getData()$trans
            }
          }
        })
        output$boxPlot3d_1 <- renderPlot({
          if (is.null(getData()$trans)) return(NULL)
          else{
            if(ncol(getData()$trans) < 5){
              boxPlot(getData()$trans)
            }
            else{
              boxPlot(getData()$trans[,c(1:5)])
            }
          }
        })
        output$geneNumtext3d_1 <- renderText({ 
          if (is.null(getData()$ori)) return(NULL)
          else{
            paste("Number of Genes: ", getData()$geneid, sep = "")
          }
        })
        output$geneNumtextFiltered3d_1 <- renderText({ 
          if (is.null(getData()$ori)) return(NULL)
          else{
            paste("Number of Genes with zero variance: ", getData()$geneid-length(getData()$filtgenelist), sep = "")
          }
        })
        output$sampNumtext3d_1 <- renderText({ 
          if (is.null(getData()$ori)) return(NULL)
          else{
            paste("Number of Samples: ", getData()$sampleid, sep = "")
          }
        })
        
        output$inputData3d_2 <- renderDataTable({
          if (is.null(getData2()$trans)) return(NULL)
          else{
            if(ncol(getData2()$trans) < 5){
              getData2()$trans
            }
            else{
              getData2()$trans
            }
          }
        })
        output$boxPlot3d_2 <- renderPlot({
          if (is.null(getData2()$trans)) return(NULL)
          else{
            if(ncol(getData2()$trans) < 5){
              boxPlot(getData2()$trans)
            }
            else{
              boxPlot(getData2()$trans[,c(1:5)])
            }
          }
        })
        output$geneNumtext3d_2 <- renderText({ 
          if (is.null(getData2()$ori)) return(NULL)
          else{
            paste("Number of Genes: ", getData2()$geneid, sep = "")
          }
        })
        output$geneNumtextFiltered3d_2 <- renderText({ 
          if (is.null(getData2()$ori)) return(NULL)
          else{
            paste("Number of Genes with zero variance: ", getData2()$geneid-length(getData2()$filtgenelist), sep = "")
          }
        })
        output$sampNumtext3d_2 <- renderText({ 
          if (is.null(getData2()$ori)) return(NULL)
          else{
            paste("Number of Samples: ", getData2()$sampleid, sep = "")
          }
        })
        
        output$inputData3d_3 <- renderDataTable({
          if (is.null(getData3()$trans)) return(NULL)
          else{
            if(ncol(getData3()$trans) < 5){
              getData3()$trans
            }
            else{
              getData3()$trans
            }
          }
        })
        output$boxPlot3d_3 <- renderPlot({
          if (is.null(getData3()$trans)) return(NULL)
          else{
            if(ncol(getData3()$trans) < 5){
              boxPlot(getData3()$trans)
            }
            else{
              boxPlot(getData3()$trans[,c(1:5)])
            }
          }
        })
        output$geneNumtext3d_3 <- renderText({ 
          if (is.null(getData3()$ori)) return(NULL)
          else{
            paste("Number of Genes: ", getData3()$geneid, sep = "")
          }
        })
        output$geneNumtextFiltered3d_3 <- renderText({ 
          if (is.null(getData3()$ori)) return(NULL)
          else{
            paste("Number of Genes with zero variance: ", getData3()$geneid-length(getData3()$filtgenelist), sep = "")
          }
        })
        output$sampNumtext3d_3 <- renderText({ 
          if (is.null(getData3()$ori)) return(NULL)
          else{
            paste("Number of Samples: ", getData3()$sampleid, sep = "")
          }
        })
        
        
        output$geneSelector <- renderUI({
          selectizeInput(inputId = "Genes", "Choose Option:", as.list(getData()$genelist),options=list(maxOptions=getData()$geneid)) 
        })
        
        output$geneSelector1 <- renderUI({
          selectizeInput(inputId = "Genes1", "Choose Option:", as.list(getData()$genelist),options=list(maxOptions=getData()$geneid)) 
        })
        output$geneSelector2 <- renderUI({
          selectizeInput(inputId = "Genes2", "Choose Option:", as.list(getData2()$genelist),options=list(maxOptions=getData2()$geneid)) 
        })
        output$geneSelector3 <- renderUI({
          selectizeInput(inputId = "Genes3", "Choose Option:", as.list(getData3()$genelist),options=list(maxOptions=getData3()$geneid)) 
        })
        
        ########### Results Table Output
        getSISPA_results <- reactive({
          
          if(input$analysisType == '1d'){
            
            if (is.null(getData()$ori)) return(NULL)
            else{
              withProgress(message = 'Running SISPA', value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'Generating data', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                getresults = SISPA(feature=1,f1.df=getData()$ori,f1.genes=getData()$genelist,f1.profile=input$profile,cpt_data=input$changes,cpt_method=input$method,cpt_max=input$max)
                list(cpt_out=getresults$cpt_out, cpt_plot=getresults$cpt_plot, all_cutoffs=getresults$all_cutoffs)
              })
            })
          }
          }  
          else if (input$analysisType == '2d'){
            if (is.null(getData()$ori) || is.null(getData2()$ori)) return(NULL)
            else{
            withProgress(message = 'Running SISPA', value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'Generating data', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                getresults = SISPA(feature=2,f1.df=getData()$ori,f1.genes=getData()$genelist,f1.profile=input$profile_one,f2.df=getData2()$ori,f2.genes=getData2()$genelist,f2.profile=input$profile_two,cpt_data=input$changes,cpt_method=input$method,cpt_max=input$max)
                list(cpt_out=getresults$cpt_out, cpt_plot=getresults$cpt_plot, all_cutoffs=getresults$all_cutoffs)
              })
            })
          }
          }
          else if (input$analysisType == '3d'){
            if (is.null(getData()$ori) || is.null(getData2()$ori) || is.null(getData3()$ori)) return(NULL)
            else{
              withProgress(message = 'Running SISPA', value = 0.1, {
                Sys.sleep(0.25)
                
                # Create 0-row data frame which will be used to store data
                dat <- data.frame(x = numeric(0), y = numeric(0))
                
                # withProgress calls can be nested, in which case the nested text appears
                # below, and a second bar is shown.
                withProgress(message = 'Generating data', detail = "part 0", value = 0, {
                  for (i in 1:10) {
                    # Each time through the loop, add another row of data. This a stand-in
                    # for a long-running computation.
                    dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(0.1, detail = paste("part", i))
                    
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                  }
                  getresults = SISPA(feature=3,f1.df=getData()$ori,f1.genes=getData()$genelist,f1.profile=input$profile_one,f2.df=getData2()$ori,f2.genes=getData2()$genelist,f2.profile=input$profile_two,f3.df=getData3()$ori,f3.genes=getData3()$genelist,f3.profile=input$profile_three,cpt_data=input$changes,cpt_method=input$method,cpt_max=input$max)
                  list(cpt_out=getresults$cpt_out, cpt_plot=getresults$cpt_plot, all_cutoffs=getresults$all_cutoffs)
                })
              })
            }
          }
          
          else{}
          
         # return(dat)
          
        })
        
        output$sispa_results <- renderDataTable({
          if(!is.null(getSISPA_results()$cpt_out)){
               getSISPA_results()$cpt_out
              }
        },
        options=list(scrollX = TRUE))
        
        output$cptplot <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
              cptsPlot(getSISPA_results()$cpt_plot,getSISPA_results()$all_cutoffs,"Zscores")
          }
        },
        height = 450, width = 450)
        
        output$fqplot <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
              freqplot(getSISPA_results()$cpt_out)
            }
        },
        height = 350, width = 400)

        output$wfplot <- renderPlot({
            if (!is.null(getSISPA_results()$cpt_out)){
              waterfallplot(getSISPA_results()$cpt_out)
            }
        },
        height = 700, width = 900)
        
        output$sispa_results2 <- renderDataTable({
          if (!is.null(getSISPA_results()$cpt_out)){
              getSISPA_results()$cpt_out
            }
        },
        options=list(scrollX = TRUE))
        
        output$cptplot2 <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
              cptsPlot(getSISPA_results()$cpt_plot,getSISPA_results()$all_cutoffs,"Zscores")
            }
        },
        height = 450, width = 450)
        
        output$fqplot2 <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
              freqplot(getSISPA_results()$cpt_out)
            }
        },
        height = 350, width = 400)
        
        output$wfplot2 <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
              waterfallplot(getSISPA_results()$cpt_out)
            }
        },
        height = 700, width = 900)
        
        
        output$sispa_results3 <- renderDataTable({
          if (!is.null(getSISPA_results()$cpt_out)){
            getSISPA_results()$cpt_out
          }
        },
        options=list(scrollX = TRUE))
        
        output$cptplot3 <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
            cptsPlot(getSISPA_results()$cpt_plot,getSISPA_results()$all_cutoffs,"Zscores")
          }
        },
        height = 450, width = 450)
        
        output$fqplot3 <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
            freqplot(getSISPA_results()$cpt_out)
          }
        },
        height = 350, width = 400)
        
        output$wfplot3 <- renderPlot({
          if (!is.null(getSISPA_results()$cpt_out)){
            waterfallplot(getSISPA_results()$cpt_out)
          }
        },
        height = 700, width = 900)
        
        
        
        ## Data Sample summary profile
        output$boxScatterPlot <- renderPlot({
          if(input$analysisType == '1d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                datasets <- mergeDatasets_boxplot(getData()$trans,getSISPA_results()$cpt_out)
                boxScatterplot(datasets, "Data Sample Profiles Overall Genes", "")
              })
            })
          }
        }, height = 400, width = 500)
        
        output$boxScatterPlotbyGene <- renderPlot({
          if(input$analysisType == '1d' && !is.null(getSISPA_results()$cpt_out)){
              withProgress(message = "Generating plot", value = 0.1, {
                Sys.sleep(0.25)
                
                # Create 0-row data frame which will be used to store data
                dat <- data.frame(x = numeric(0), y = numeric(0))
                
                # withProgress calls can be nested, in which case the nested text appears
                # below, and a second bar is shown.
                withProgress(message = 'in progress', detail = "part 0", value = 0, {
                  for (i in 1:10) {
                    # Each time through the loop, add another row of data. This a stand-in
                    # for a long-running computation.
                    dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(0.1, detail = paste("part", i))
                    
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                  }
                    datasets <- mergeDatasets(getData()$trans,getSISPA_results()$cpt_out,sel=input$Genes)
                    barPlot(datasets$ordered_list,datasets$mean1,datasets$mean2,"",datasets$fun)
                    #boxScatterplot(datasets,input$Genes)
                })
              })
          }
        }, height = 500, width = 550)
        
        ## Data Sample summary profile
        output$boxScatterPlot1 <- renderPlot({
          if(input$analysisType == '2d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                  datasets <- mergeDatasets_boxplot(getData()$trans,getSISPA_results()$cpt_out)
                  boxScatterplot(datasets, "Data1 Sample Profiles Overall Genes", "")
              })
            })
          }
        }, height = 400, width = 500)
        
        ## Data Sample summary profile
        output$boxScatterPlot2 <- renderPlot({
          if(input$analysisType == '2d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                  datasets <- mergeDatasets_boxplot(getData2()$trans,getSISPA_results()$cpt_out)
                  boxScatterplot(datasets, "Data2 Sample Profiles Overall Genes", "")
              })
            })
          }
        }, height = 400, width = 500)
        
        output$boxScatterPlotbyGene1 <- renderPlot({
          if(input$analysisType == '2d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                
                datasets <- mergeDatasets(getData()$trans,getSISPA_results()$cpt_out,sel=input$Genes1)
                barPlot(datasets$ordered_list,datasets$mean1,datasets$mean2,"",datasets$fun)
              })
            })
          }
        }, height = 500, width = 550)
        
        output$boxScatterPlotbyGene2 <- renderPlot({
          if(input$analysisType == '2d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                datasets <- mergeDatasets(getData2()$trans,getSISPA_results()$cpt_out,sel=input$Genes2)
                barPlot(datasets$ordered_list,datasets$mean1,datasets$mean2,"",datasets$fun)
              })
            })
          }
        }, height = 500, width = 550)
        
        
        ## Data Sample summary profile
        output$boxScatterPlot1_3d <- renderPlot({
          if(input$analysisType == '3d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                datasets <- mergeDatasets_boxplot(getData()$trans,getSISPA_results()$cpt_out)
                boxScatterplot(datasets, "Data1 Sample Profiles Overall Genes", "")
              })
            })
          }
        }, height = 400, width = 500)
        
        ## Data Sample summary profile
        output$boxScatterPlot2_3d <- renderPlot({
          if(input$analysisType == '3d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                datasets <- mergeDatasets_boxplot(getData2()$trans,getSISPA_results()$cpt_out)
                boxScatterplot(datasets, "Data2 Sample Profiles Overall Genes", "")
              })
            })
          }
        }, height = 400, width = 500)
        
        ## Data Sample summary profile
        output$boxScatterPlot3_3d <- renderPlot({
          if(input$analysisType == '3d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                datasets <- mergeDatasets_boxplot(getData3()$trans,getSISPA_results()$cpt_out)
                boxScatterplot(datasets, "Data3 Sample Profiles Overall Genes", "")
              })
            })
          }
        }, height = 400, width = 500)
        
        output$boxScatterPlotbyGene1_3d <- renderPlot({
          if(input$analysisType == '3d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                
                datasets <- mergeDatasets(getData()$trans,getSISPA_results()$cpt_out,sel=input$Genes1)
                barPlot(datasets$ordered_list,datasets$mean1,datasets$mean2,"",datasets$fun)
              })
            })
          }
        }, height = 500, width = 550)
        
        output$boxScatterPlotbyGene2_3d <- renderPlot({
          if(input$analysisType == '3d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                datasets <- mergeDatasets(getData2()$trans,getSISPA_results()$cpt_out,sel=input$Genes2)
                barPlot(datasets$ordered_list,datasets$mean1,datasets$mean2,"",datasets$fun)
              })
            })
          }
        }, height = 500, width = 550)
        
        output$boxScatterPlotbyGene3_3d <- renderPlot({
          if(input$analysisType == '3d' && !is.null(getSISPA_results()$cpt_out)){
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                datasets <- mergeDatasets(getData3()$trans,getSISPA_results()$cpt_out,sel=input$Genes3)
                barPlot(datasets$ordered_list,datasets$mean1,datasets$mean2,"",datasets$fun)
              })
            })
          }
        }, height = 500, width = 550)
        
        output$survivalPlot <- renderPlot({
          if(!is.null(getSISPA_results()$cpt_out) & !is.null(input$clinical)){
            
              clinicalFile <- input$clinical$datapath
              clinicalData <- read.csv(clinicalFile, header=T, sep="\t")

            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                survPlot(clinicalData,getSISPA_results()$cpt_out)
              })
            })
          }
        }, height = 600, width = 600)
        
        output$survivalPlot2 <- renderPlot({
          if(!is.null(getSISPA_results()$cpt_out) & !is.null(input$clinical2)){
            clinicalFile <- input$clinical2$datapath
            clinicalData <- read.csv(clinicalFile, header=T, sep="\t")
            
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                survPlot(clinicalData,getSISPA_results()$cpt_out)
              })
            })
          }
        }, height = 600, width = 600)
        
        output$survivalPlot3 <- renderPlot({
          if(!is.null(getSISPA_results()$cpt_out) & !is.null(input$clinical3)){
            clinicalFile <- input$clinical3$datapath
            clinicalData <- read.csv(clinicalFile, header=T, sep="\t")
            
            withProgress(message = "Generating plot", value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'in progress', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                survPlot(clinicalData,getSISPA_results()$cpt_out)
              })
            })
          }
        }, height = 600, width = 600)
        
        
        observeEvent(input$do, {
          cat("Running", input$rep, "Replicates\n")
        })
        
        observeEvent(input$do2, {
          cat("Running", input$rep2, "Replicates\n")
        })
        
        observeEvent(input$do3, {
          cat("Running", input$rep3, "Replicates\n")
        })

        ########### Results Table Output
        getPvalue1 <- eventReactive(input$do, {
          
          if(input$analysisType == '1d'){
            
            if (is.null(getData()$ori) || input$rep == 0 || is.null(input$rep == 0)) return(NULL)
            else{
                
              withProgress(message = 'Running SISPA', value = 0.1, {
                Sys.sleep(0.25)
                
                # Create 0-row data frame which will be used to store data
                dat <- data.frame(x = numeric(0), y = numeric(0))
                
                # withProgress calls can be nested, in which case the nested text appears
                # below, and a second bar is shown.
                withProgress(message = 'Generating data', detail = "part 0", value = 0, {
                  for (i in 1:10) {
                    # Each time through the loop, add another row of data. This a stand-in
                    # for a long-running computation.
                    dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(0.1, detail = paste("part", i))
                    
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                  }
                  tstats <- btest(getSISPA_results()$cpt_out)
                  sampledData <- sampling(getData()$ori, as.numeric(getData()$sampleid), as.numeric(input$rep))
                  sampledttest<-data.frame(1:as.numeric(input$rep),2)
                  
                  nocptCounter = 0
                  
                  for ( i in 1:input$rep){
                    df <- data.frame(t(sampledData[[i]]))
                    getresults = SISPA(feature=1,f1.df=df,f1.genes=getData()$genelist,f1.profile=input$profile,cpt_data=input$changes,cpt_method=input$method,cpt_max=input$max)
                    
                    if(is.null(getresults$cpt_out)){
                      sampledttest[i,] = c("","")
                      nocptCounter = nocptCounter + 1
                    }
                    else{
                      sampledttest[i,] <- btest(getresults$cpt_out)
                    }
                  }
                  tstatistics <- sum(sampledttest[,1] >= tstats$bstat) ##/ as.numeric(input$rep)
                  pvalue <- sum(sampledttest[,2] <= tstats$pval) ##/ as.numeric(input$rep)
                  
                  bp_table <- cbind.data.frame(signif(tstats$pval,3), 
                                               signif(tstats$bstat,3),
                                               as.numeric(input$rep),
                                               nocptCounter,
                                               pvalue,
                                               tstatistics,
                                               signif(pvalue/as.numeric(input$rep),3),
                                               signif(tstatistics/as.numeric(input$rep),3)
                  )
                  colnames(bp_table) <- c(
                    "Obs. p-val", 
                    "Obs. b-stat",
                    "# of reps",
                    "# of noCpt",
                    "reps <= obs. p-val",
                    "reps >= obs. bstat",
                    "Est. p-val",
                    "Est. b-stat"
                  )

                })
              })
            }
          }  

          return(data = list(sampledttest = sampledttest, cutoff = tstats$bstat, tstatistics=tstatistics, pvalue=pvalue, bp_table=bp_table))
        })
        
        getPvalue2 <- eventReactive(input$do2, {
          
          if (input$analysisType == '2d'){
            if (is.null(getData()$ori) || is.null(getData2()$ori) || input$rep2 == 0 || is.null(input$rep2 == 0)) return(NULL)
            else{
              withProgress(message = 'Running SISPA', value = 0.1, {
                Sys.sleep(0.25)
                
                # Create 0-row data frame which will be used to store data
                dat <- data.frame(x = numeric(0), y = numeric(0))
                
                # withProgress calls can be nested, in which case the nested text appears
                # below, and a second bar is shown.
                withProgress(message = 'Generating data', detail = "part 0", value = 0, {
                  for (i in 1:10) {
                    # Each time through the loop, add another row of data. This a stand-in
                    # for a long-running computation.
                    dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(0.1, detail = paste("part", i))
                    
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                  }
                  
                  tstats <- btest(getSISPA_results()$cpt_out)
                  sampledData <- sampling(getData()$ori, as.numeric(getData()$sampleid), as.numeric(input$rep2))
                  sampledData2 <- sampling(getData2()$ori, as.numeric(getData2()$sampleid), as.numeric(input$rep2))
                  sampledttest<-data.frame(1:as.numeric(input$rep2),2)
                  
                  nocptCounter = 0
                  
                  for ( i in 1:input$rep2){
                    df <- data.frame(t(sampledData[[i]]))
                    df2 <- data.frame(t(sampledData2[[i]]))
                    getresults = SISPA(feature=2,f1.df=df,f1.genes=getData()$genelist,f1.profile=input$profile_one,f2.df=df2,f2.genes=getData2()$genelist,f2.profile=input$profile_two,cpt_data=input$changes,cpt_method=input$method,cpt_max=input$max)
                    
                    if(is.null(getresults$cpt_out)){
                      sampledttest[i,] = c("","")
                      nocptCounter = nocptCounter + 1
                    }
                    else{
                      sampledttest[i,] <- btest(getresults$cpt_out)
                    }
                  }
                  tstatistics <- sum(sampledttest[,1] >= tstats$bstat) ##/ as.numeric(input$rep2)
                  pvalue <- sum(sampledttest[,2] <= tstats$pval) ##/ as.numeric(input$rep2)
                  
                  bp_table <- cbind.data.frame(signif(tstats$pval,3), 
                                               signif(tstats$bstat,3),
                                               as.numeric(input$rep2),
                                               nocptCounter,
                                               pvalue,
                                               tstatistics,
                                               signif(pvalue/as.numeric(input$rep2),3),
                                               signif(tstatistics/as.numeric(input$rep2),3)
                  )
                  colnames(bp_table) <- c(
                    "Obs. p-val", 
                    "Obs. b-stat",
                    "# of reps",
                    "# of noCpt",
                    "reps <= obs. p-val",
                    "reps >= obs. b-stat",
                    "Est. p-val",
                    "Est. b-stat"
                  )
                  
                })
              })
            }
          }
          
          return(data = list(sampledttest = sampledttest, cutoff = tstats$bstat, tstatistics=tstatistics, pvalue=pvalue, bp_table=bp_table))
        })
        
        getPvalue3 <- eventReactive(input$do3, {
          
        if (input$analysisType == '3d'){
          if (is.null(getData()$ori) || is.null(getData2()$ori) || is.null(getData3()$ori) || input$rep3 == 0 || is.null(input$rep3 == 0)) return(NULL)
          else{
            
            withProgress(message = 'Running SISPA', value = 0.1, {
              Sys.sleep(0.25)
              
              # Create 0-row data frame which will be used to store data
              dat <- data.frame(x = numeric(0), y = numeric(0))
              
              # withProgress calls can be nested, in which case the nested text appears
              # below, and a second bar is shown.
              withProgress(message = 'Generating data', detail = "part 0", value = 0, {
                for (i in 1:10) {
                  # Each time through the loop, add another row of data. This a stand-in
                  # for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  incProgress(0.1, detail = paste("part", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
                }
                
                tstats <- btest(getSISPA_results()$cpt_out)
                sampledData <- sampling(getData()$ori, as.numeric(getData()$sampleid), as.numeric(input$rep3))
                sampledData2 <- sampling(getData2()$ori, as.numeric(getData2()$sampleid), as.numeric(input$rep3))
                sampledData3 <- sampling(getData3()$ori, as.numeric(getData3()$sampleid), as.numeric(input$rep3))
                sampledttest<-data.frame(1:as.numeric(input$rep3),2)
                
                nocptCounter = 0
                
                for ( i in 1:input$rep3){
                  df <- data.frame(t(sampledData[[i]]))
                  df2 <- data.frame(t(sampledData2[[i]]))
                  df3 <- data.frame(t(sampledData3[[i]]))
                  getresults = SISPA(feature=3,f1.df=df,f1.genes=getData()$genelist,f1.profile=input$profile_one,f2.df=df2,f2.genes=getData2()$genelist,f2.profile=input$profile_two,f3.df=df3,f3.genes=getData3()$genelist,f3.profile=input$profile_three,cpt_data=input$changes,cpt_method=input$method,cpt_max=input$max)
                  
                  if(is.null(getresults$cpt_out)){
                    sampledttest[i,] = c("","")
                    nocptCounter=nocptCounter + 1
                  }
                  else{
                    sampledttest[i,] <- btest(getresults$cpt_out)
                  }
                }
                tstatistics <- sum(sampledttest[,1] >= tstats$bstat) ## / as.numeric(input$rep3)
                pvalue <- sum(sampledttest[,2] <= tstats$pval)## / as.numeric(input$rep3)
                
                bp_table <- cbind.data.frame(signif(tstats$pval,3), 
                                             signif(tstats$bstat,3),
                                             as.numeric(input$rep3),
                                             nocptCounter,
                                             pvalue,
                                             tstatistics,
                                             signif(pvalue/as.numeric(input$rep3),3),
                                             signif(tstatistics/as.numeric(input$rep3),3)
                )
                colnames(bp_table) <- c(
                  "Obs. p-val", 
                  "Obs. b-stat",
                  "# of reps",
                  "# of noCpt",
                  "reps <= obs. p-val",
                  "reps >= obs. bstat",
                  "Est. p-val",
                  "Est. b-stat"
                )
                
              })
            })
          }
        }
        
        return(data = list(sampledttest = sampledttest, cutoff = tstats$bstat, tstatistics=tstatistics, pvalue=pvalue, bp_table=bp_table))
      })

        
        output$sig <- renderPlot({
         if (is.null(getPvalue1()$sampledttest[,1])) return (NULL)
         else{ #if (!is.null(getPvalue1()$sampledttest)){
           h <- hist(getPvalue1()$sampledttest[,1])
           clr <- ifelse(h$breaks <= getPvalue1()$cutoff, "white", "red")[-length(h$breaks)]
           plot(h, 
                col=clr,
                xlab="Observed bartlett-statistics", 
                ylab= "Frequency", main="Histogram of bartlett-test statistics", 
                xlim=c(0, max( getPvalue1()$sampledttest[,1][!is.infinite(getPvalue1()$sampledttest[,1])] )),
                ylim=c(0, max(h$counts))
            )
           abline(v=getPvalue1()$cutoff, lty=4,col='red')
         }
        }, height = 600, width = 600)
        
        output$sigTable <- renderTable({ 
          if (is.null(getPvalue1()$sampledttest[,1])) return (NULL)
          else {# (!is.null(getPvalue1()$sampledttest)){
            getPvalue1()$bp_table
          }
        })    
        
        output$sig2 <- renderPlot({
          #if(getPvalue()$sampledttest[,4] == input$rep2) return(NULL)
          if (is.null(getPvalue2()$sampledttest[,1])) return (NULL)
          else {# (!is.null(getPvalue2()$sampledttest)){
            h <- hist(getPvalue2()$sampledttest[,1])
            clr <- ifelse(h$breaks <= getPvalue2()$cutoff, "white", "red")[-length(h$breaks)]
            plot(h, 
                 col=clr,
                 xlab="Observed bartlett-statistics", 
                 ylab= "Frequency", main="Histogram of bartlett-test statistics", 
                 xlim=c(0, max( getPvalue2()$sampledttest[,1][!is.infinite(getPvalue2()$sampledttest[,1])] )),
                 ylim=c(0, max(h$counts))
            )
            abline(v=getPvalue2()$cutoff, lty=4,col='red')
          }
        }, height = 600, width = 600)
        
        output$sigTable2 <- renderTable({ 
          if (is.null(getPvalue2()$bp_table)) return (NULL)
          else {# (!is.null(getPvalue2()$sampledttest)){
            getPvalue2()$bp_table
          }
        })    
        
        output$sig3 <- renderPlot({
          #if(getPvalue()$sampledttest[,4] == input$rep3) return(NULL)
          if (!is.null(getPvalue3()$sampledttest)){
            h <- hist(getPvalue3()$sampledttest[,1])
            clr <- ifelse(h$breaks <= getPvalue3()$cutoff, "white", "red")[-length(h$breaks)]
            plot(h, 
                 col=clr,
                 xlab="Observed bartlett-statistics", 
                 ylab= "Frequency", main="Histogram of bartlett-test statistics", 
                 xlim=c(0, max( getPvalue3()$sampledttest[,1][!is.infinite(getPvalue3()$sampledttest[,1])] )),
                 ylim=c(0, max(h$counts))
            )
            abline(v=getPvalue3()$cutoff, lty=4,col='red')
          }
        }, height = 600, width = 600)
        
        output$sigTable3 <- renderTable({ 
          if (!is.null(getPvalue3()$sampledttest)){
            getPvalue3()$bp_table
          }
        })   
        
        #download SISPA results table
        output$download <- downloadHandler(
          filename = function() { paste("SISPA_results", '.csv', sep='') },
          content = function(file) {
            write.csv(getSISPA_results()$cpt_out, file)
          }
        )
})

