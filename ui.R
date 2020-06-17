textareaInput <- function(id, label, value, rows=20, cols=35, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}

shinyUI(fluidPage(theme = "bootstrap.css",
<<<<<<< HEAD
                  tags$head(includeScript("google-analytics.js")),

=======
                  
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
             headerPanel(
               h1("shinySISPA", 
                  style = "font-family: 'Times New Roman'; font-style:bold;
                  font-size: 80px; line-height: 1.1;
                  color: red")),
             
             tags$h4("Sample Integrated Set Profile Analysis with Shiny", 
                     style = "font-family: 'Times New Roman'; font-style:italic;
                     font-size: 50px;
                     color: red"),
             

             fluidRow(
               column(2,
                      wellPanel(
                        h4("Analysis Type"),
                        tags$hr(),
                        radioButtons("analysisType", "", choices = c("1" = "1d","2" = "2d","3" = "3d"), inline=TRUE)
                      ),
                      wellPanel(
                        h4("Data Input"),
                        tags$hr(),
                        conditionalPanel(condition = "input.analysisType == '1d'",
                                         #textareaInput("datainput","Enter Data:", "", rows = 5, cols = 30),
<<<<<<< HEAD
                                         selectInput(inputId="f", label="File Input:", choices=c("User data"="LD","Example data"="ED"), selectize = TRUE, width=NULL),
                                         conditionalPanel("input.f=='LD'", fileInput(inputId="file", label="", width=NULL, multiple=TRUE)),
                                         #textInput("dataType", "Input Data Type", value="", width="149px"),
                                         selectInput("profile","Sample Profile", c('up','down'),width="130px", selectize = FALSE)
=======
                                         fileInput(inputId="file", label="File Input:"),
                                         #textInput("dataType", "Input Data Type", value="", width="149px"),
                                         selectInput("profile","Sample Profile", c('up','down'),width="130px",selectize = FALSE)
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
                                         ), 
                        conditionalPanel(condition = "input.analysisType == '2d' || input.analysisType == '3d'",
                                         #textareaInput("datainput1","Enter Data:", "", rows = 5, cols = 30),
                                         selectInput(inputId="f1", label="File Input 1:", choices=c("User data"="LD","Example data"="ED"), selectize = TRUE, width=NULL),
                                         conditionalPanel("input.f1=='LD'", fileInput(inputId="file1", label="", width=NULL, multiple=TRUE)),
                                         ##fileInput(inputId="file1", label="File Input 1:"),
                                         #textInput("dataType1", "Input Data Type 1", value="", width="149px"),
<<<<<<< HEAD
                                         selectInput("profile_one","Sample Profile 1", c('up','down'),width="130px", selected="down", selectize = FALSE),
=======
                                         selectInput("profile_one","Sample Profile 1", c('up','down'),width="130px",selectize = FALSE),
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
                                         #textareaInput("datainput2","Data 2:", "", rows = 5, cols = 30),
                                         tags$hr(),
                                         selectInput(inputId="f2", label="File Input 2:", choices=c("User data"="LD","Example data"="ED"), selectize = TRUE, width=NULL),
                                         conditionalPanel("input.f2=='LD'", fileInput(inputId="file2", label="", width=NULL, multiple=TRUE)),
                                         ##fileInput(inputId="file2", label="File Input 2:"),
                                         #textInput("dataType2", "Input Data Type 2", value="", width="149px"),
<<<<<<< HEAD
                                         selectInput("profile_two","Sample Profile 2", c('up','down'), width="130px", selected="down", selectize = FALSE)
                        ),
                        conditionalPanel(condition = "input.analysisType == '3d'",
                                         tags$hr(),
                                         selectInput(inputId="f3", label="File Input 3:", choices=c("User data"="LD","Example data"="ED"), selectize = TRUE, width=NULL),
                                         conditionalPanel("input.f3=='LD'", fileInput(inputId="file3", label="", width=NULL, multiple=TRUE)),
=======
                                         selectInput("profile_two","Sample Profile 2", c('up','down'),width="130px",selectize = FALSE)
                        ),
                        conditionalPanel(condition = "input.analysisType == '3d'",
                                         tags$hr(),
                                         fileInput(inputId="file3", label="File Input 3:"),
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
                                         #textInput("dataType3", "Input Data Type 3", value="", width="149px"),
                                         selectInput("profile_three","Sample Profile 3", c('up','down'),width="130px",selectize = FALSE)
                                         
                        )
                      ),
                      wellPanel(
                        h4("Changepoint Input"),
                        tags$hr(),
<<<<<<< HEAD
                        selectInput("changes","Changes Using:", c('mean','var','meanvar'),width="150px",selected="var", selectize = FALSE),
=======
                        selectInput("changes","Changes Using:", c('mean','var','meanvar'),width="150px",selected="mean", selectize = FALSE),
>>>>>>> 6b77c12893d36f96de2d60d81d6fc308fc49562f
                        selectInput("method","Method:", c('AMOC','PELT','BinSeg','SeqNeigh'),width="150px",selected="BinSeg", selectize = FALSE),
                        numericInput("max","Max Q Allowed:", min=1, max=60, step=1, width="149px", value=60)
                        #sliderInput("max", label="Range:",min=0, max=60, value=5, step=1)
                      ),
                      wellPanel(
                        downloadButton('download', 'Download Results')
                      )
               ),
               
               column(8,
                      
                conditionalPanel(condition = "input.analysisType == '1d'",     
                  tabsetPanel(id="Onedim",
                      tabPanel ("Input Data",value=1,
                                fluidRow(
                                  column(6,
                                      textOutput("geneNumtext"),
                                      textOutput("geneNumtextFiltered"),
                                      textOutput("sampNumtext"),
                                      #br(),
                                      plotOutput("boxPlot", height="600px", width="600px"),
                                      tags$p("*Only the First Five rows are shown")
                                      #br(),
                                      #dataTableOutput("inputData")
                                  ))
                      ),
                      tabPanel("SISPA Results",value=2,
                               fluidRow(
                                 column(8,
                                       dataTableOutput("sispa_results")
                                 ),
                                 column(4,
                                        plotOutput("cptplot"),
                                        br(),
                                        br(),
                                        plotOutput("fqplot")
                                 ))
                      ),
                      tabPanel("Waterfall Plot",value=3,
                               fluidRow(
                                 column(8,
                                        plotOutput("wfplot")
                                 ))
                      ),
                      tabPanel("Sample Profile",value=4,
                               br(),
                               fluidRow(
                                 column(6,
                                  plotOutput("boxScatterPlot")
                                 )),
                               br(),
                               br(),
                               fluidRow(
                                 column(6,
                                  plotOutput("boxScatterPlotbyGene")
                                 ))
                      ),
                      #tabPanel("Survival Analysis",value=5,
                      #         fluidRow(
                      #           column(8,
                      #                  plotOutput("survivalPlot", height="600px", width="600px")
                      #           ))
                      #),
                      #tabPanel("Significance",value=6,
                      #         fluidRow(
                      #           column(8,
                      #                 plotOutput("sig", height="600px", width="600px"),
                      #                 br(),
                      #                 tableOutput("sigTable")
                      #           ))
                      #),
                      tabPanel("How to Cite",value=7,
                               #tags$div(class="header", checked=NA,
                               h4("shinySISPA is a product of the Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University (https://bbisr.winship.emory.edu)."),
                               br(),
                               h4("This work is funded by the Leukemia and Lymphoma Society Translational Research Program Award (Jeanne Kowalski); Georgia Research Alliance Scientist Award (Jeanne Kowalski); Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI [Award number P30CA138292, in part]. The content is solely the responsibility of the authors (Bhakti Dwivedi & Jeanne Kowalski) and does not necessarily represent the official views of the NIH."),
                               br(),
                               h4("The shinySISPA source code is freely available to download from the GitHub (https://github.com/BhaktiDwivedi/shinySISPA)"),
                               br(),
                               h4("The Bioconductor R package for SISPA is available at https://www.bioconductor.org/packages/SISPA."),
                               br(),
                               h4("Please cite the method as: Kowalski J, Dwivedi B, Newman S, Switchenko JM, Pauly R, Gutman DA, Arora J, Gandhi K, Ainslie K, Doho G, Qin Z, Moreno CS, Rossi MR, Vertino PM, Lonial S, Bernal-Mizrachi L, Boise LH. Gene integrated set profile analysis: a context-based approach for inferring biological endpoints. Nucleic Acids Research. 2016 Apr 20;44(7):e69. doi: 10.1093/nar/gkv1503. Epub 2016 Jan 29. PubMed PMID: 26826710; PubMed Central PMCID: PMC4838358."),
                               tags$iframe(src = "shinySISPA_Manual.pdf", width = 1200, height = 1000)
                               
                    ))
               ),
               conditionalPanel(condition = "input.analysisType == '2d'",     
                  tabsetPanel(id="Twodim",
                      tabPanel ("Input Data",value=1,
                                fluidRow(
                                  column(6,
                                    #tags$p("*Data from only first five genes is shown"),
                                    #plotOutput("boxPlot1", height="600px", width="600px"),
                                    #br(),
                                    #br(),
                                    textOutput("geneNumtext1"),
                                    textOutput("geneNumtextFiltered1"),
                                    textOutput("sampNumtext1"), #verbatimTextOutput
                                    plotOutput("boxPlot1", height="600px", width="600px"),
                                    br(),
                                    tags$p("*Only the First Five rows are shown")
                                    #dataTableOutput("inputData1")
                                  ),
                                  column(6,
                                    #tags$p("*Data from only first five genes is shown"),
                                    #plotOutput("boxPlot2", height="600px", width="600px"),
                                    #br(),
                                    #br(),
                                    textOutput("geneNumtext2"),
                                    textOutput("geneNumtextFiltered2"),
                                    textOutput("sampNumtext2"),
                                    plotOutput("boxPlot2", height="600px", width="600px"),
                                    br(),
                                    tags$p("*Only the First Five rows are shown")
                                    #dataTableOutput("inputData2")
                                  ))
                      ),
                      tabPanel("SISPA Results",value=2,
                               fluidRow(
                                 column(8,
                                    dataTableOutput("sispa_results2")
                                 ),
                                 column(4,
                                    plotOutput("cptplot2"),
                                    br(),
                                    br(),
                                    plotOutput("fqplot2")
                                 ))
                      ),
                      tabPanel("Waterfall Plot",value=3,
                               fluidRow(
                                 column(8,
                                        plotOutput("wfplot2")
                                 ))
                      ),
                      tabPanel("Sample Profile",value=4,
                               br(),
                               fluidRow(
                                 column(6,
                                        plotOutput("boxScatterPlot1")
                                 ),
                                 column(6,
                                        plotOutput("boxScatterPlot2")
                                 )),
                               br(),
                               fluidRow(
                                 column(6,
                                        plotOutput("boxScatterPlotbyGene1")
                                 ),
                                 column(6,
                                        plotOutput("boxScatterPlotbyGene2")
                                 ))
                      ),
                      #tabPanel("Survival Analysis",value=5,
                      #         fluidRow(
                      #           column(8,
                      #                  plotOutput("survivalPlot2", height="600px", width="600px")
                      #           ))
                      #),
                      #tabPanel("Significance",value=6,
                      #         fluidRow(
                      #           column(8,
                      #                  plotOutput("sig2", height="600px", width="600px"),
                      #                  br(),
                      #                  tableOutput("sigTable2")
                      #           ))
                      #),
                      tabPanel("How to Cite",value=7,
                               #tags$div(class="header", checked=NA,
                               h4("shinySISPA is a product of the Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University (https://bbisr.winship.emory.edu)."),
                               br(),
                               h4("This work is funded by the Leukemia and Lymphoma Society Translational Research Program Award (Jeanne Kowalski); Georgia Research Alliance Scientist Award (Jeanne Kowalski); Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI [Award number P30CA138292, in part]. The content is solely the responsibility of the authors (Bhakti Dwivedi & Jeanne Kowalski) and does not necessarily represent the official views of the NIH."),
                               br(),
                               h4("The shinySISPA source code is freely available to download from the GitHub (https://github.com/BhaktiDwivedi/shinySISPA)"),
                               br(),
                               h4("The Bioconductor R package for SISPA is available at https://www.bioconductor.org/packages/SISPA."),
                               br(),
                               h4("Please cite the method as: Kowalski J, Dwivedi B, Newman S, Switchenko JM, Pauly R, Gutman DA, Arora J, Gandhi K, Ainslie K, Doho G, Qin Z, Moreno CS, Rossi MR, Vertino PM, Lonial S, Bernal-Mizrachi L, Boise LH. Gene integrated set profile analysis: a context-based approach for inferring biological endpoints. Nucleic Acids Research. 2016 Apr 20;44(7):e69. doi: 10.1093/nar/gkv1503. Epub 2016 Jan 29. PubMed PMID: 26826710; PubMed Central PMCID: PMC4838358."),
                               tags$iframe(src = "shinySISPA_Manual.pdf", width = 1200, height = 1000)
                      )
                      
               )),
               conditionalPanel(condition = "input.analysisType == '3d'",
                  tabsetPanel(id="Threedim",
                     tabPanel ("Input Data", value=1,
                      conditionalPanel(condition = "input.DataDescription == 10",  
                           fluidRow(
                             column(4,
                                    textOutput("geneNumtext3d_1"),
                                    textOutput("geneNumtextFiltered3d_1"),
                                    textOutput("sampNumtext3d_1") #verbatimTextOutput
                                    #plotOutput("boxPlot3d_1", height="600px", width="600px")
                                    #br(),
                                    #tags$p("*Only the First Five rows are shown")
                                    #dataTableOutput("inputData3d_1")
                             ))),
                      conditionalPanel(condition = "input.DataDescription == 20", 
                           fluidRow(
                             column(4,
                                    textOutput("geneNumtext3d_2"),
                                    textOutput("geneNumtextFiltered3d_2"),
                                    textOutput("sampNumtext3d_2")
                                    #br()
                                    #tags$p("*Only the First Five rows are shown")
                                    #dataTableOutput("inputData3d_2")
                             ))),
                      conditionalPanel(condition = "input.DataDescription == 30",   
                           fluidRow(
                             column(4,
                                    textOutput("geneNumtext3d_3"),
                                    textOutput("geneNumtextFiltered3d_3"),
                                    textOutput("sampNumtext3d_3")
                                    #br(),
                                    #tags$p("*Only the First Five rows are shown")
                                    #dataTableOutput("inputData3d_3")
                             )))
                    ),
                    tabPanel("SISPA Results",value=2,
                           fluidRow(
                             column(8,
                                    dataTableOutput("sispa_results3")
                             ),
                             column(4,
                                    plotOutput("cptplot3"),
                                    br(),
                                    br(),
                                    plotOutput("fqplot3")
                             ))
                    ),
                    tabPanel("Waterfall Plot",value=3,
                           fluidRow(
                             column(8,
                                    plotOutput("wfplot3")
                             ))
                    ),
                    tabPanel("Sample Profile",value=4,
                             conditionalPanel(condition = "input.SampleProfile == 10",  
                                              fluidRow(
                                              )),
                             conditionalPanel(condition = "input.SampleProfile == 20", 
                                              fluidRow(
                                              )),
                             conditionalPanel(condition = "input.SampleProfile == 30",   
                                              fluidRow(
                                              )) 
                    ),
                    #tabPanel("Survival Analysis",value=5,
                    #     fluidRow(
                    #       column(8,
                    #              plotOutput("survivalPlot3", height="600px", width="600px")
                    #       ))
                    #),
                    #tabPanel("Significance",value=6,
                    #         fluidRow(
                    #           column(8,
                    #                  plotOutput("sig3", height="600px", width="600px"),
                    #                  br(),
                    #                  tableOutput("sigTable3")
                    #           ))
                    #),
                    tabPanel("How to Cite",value=7,
                             #tags$div(class="header", checked=NA,
                             h4("shinySISPA is a product of the Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University (https://bbisr.winship.emory.edu)."),
                             br(),
                             h4("This work is funded by the Leukemia and Lymphoma Society Translational Research Program Award (Jeanne Kowalski); Georgia Research Alliance Scientist Award (Jeanne Kowalski); Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI [Award number P30CA138292, in part]. The content is solely the responsibility of the authors (Bhakti Dwivedi & Jeanne Kowalski) and does not necessarily represent the official views of the NIH."),
                             br(),
                             h4("The shinySISPA source code is freely available to download from the GitHub (https://github.com/BhaktiDwivedi/shinySISPA)"),
                             br(),
                             h4("The Bioconductor R package for SISPA is available at https://www.bioconductor.org/packages/SISPA."),
                             br(),
                             h4("Please cite the method as: Kowalski J, Dwivedi B, Newman S, Switchenko JM, Pauly R, Gutman DA, Arora J, Gandhi K, Ainslie K, Doho G, Qin Z, Moreno CS, Rossi MR, Vertino PM, Lonial S, Bernal-Mizrachi L, Boise LH. Gene integrated set profile analysis: a context-based approach for inferring biological endpoints. Nucleic Acids Research. 2016 Apr 20;44(7):e69. doi: 10.1093/nar/gkv1503. Epub 2016 Jan 29. PubMed PMID: 26826710; PubMed Central PMCID: PMC4838358."),
                             tags$iframe(src = "shinySISPA_Manual.pdf", width = 1200, height = 1000)
                    )
                 )),
               
               conditionalPanel(condition = "input.Threedim == 1 & input.analysisType == '3d'",
                  tabsetPanel(id="DataDescription",
                      tabPanel ("Data 1",value=10,
                          fluidRow(
                            column(6,
                                   plotOutput("boxPlot3d_1", height="600px", width="600px")
                            ))
                      ),
                      tabPanel ("Data 2",value=20,
                          fluidRow(
                            column(6,
                                   plotOutput("boxPlot3d_2", height="600px", width="600px")
                            ))
                      ),
                      tabPanel ("Data 3",value=30,
                          fluidRow(
                            column(6,
                                   plotOutput("boxPlot3d_3", height="600px", width="600px")
                            ))
                      )
                  )),
               conditionalPanel(condition = "input.Threedim == 4 & input.analysisType == '3d'",
                  tabsetPanel(id="SampleProfile",
                      tabPanel ("Data 1",value=10,
                          br(),
                          fluidRow(
                            column(6,
                                   plotOutput("boxScatterPlot1_3d")
                            )),
                          br(),
                          fluidRow(
                            column(6,
                                   plotOutput("boxScatterPlotbyGene1_3d")
                            ))
                      ),
                     tabPanel ("Data 2",value=20,
                          br(),
                          fluidRow(
                            column(6,
                                   plotOutput("boxScatterPlot2_3d")
                            )),
                          br(),
                          fluidRow(
                            column(6,
                                   plotOutput("boxScatterPlotbyGene2_3d")
                            ))
                      ),
                      tabPanel ("Data 3",value=30,
                          br(),
                          fluidRow(
                            column(6,
                                   plotOutput("boxScatterPlot3_3d")
                            )),
                          br(),
                          fluidRow(
                            column(6,
                                   plotOutput("boxScatterPlotbyGene3_3d")
                            ))
                      )
                ))
               
        ),
        column(2,
                 conditionalPanel(condition = "input.Onedim == 4 & input.analysisType == '1d'",
                      wellPanel(             
                       h4("Genes to Display"),
                       tags$hr(),
                       uiOutput(outputId="geneSelector",width=NULL)
                    )),
                    conditionalPanel(condition = "input.Twodim == 4 | input.Threedim == 4",
                       wellPanel(             
                         h4("Genes to Display in Data 1"),
                         tags$hr(),
                         uiOutput(outputId="geneSelector1",width=NULL)
                       ),
                       wellPanel(
                         h4("Genes to Display in Data 2"),
                         tags$hr(),
                         uiOutput(outputId="geneSelector2",width=NULL)
                       )),
                    conditionalPanel(condition = "input.Threedim == 4 & input.analysisType == '3d'",
                      wellPanel(             
                        h4("Genes to Display in Data 3"),
                        tags$hr(),
                        uiOutput(outputId="geneSelector3",width=NULL)
                      )),
                     conditionalPanel(condition = "input.Onedim == 5 & input.analysisType == '1d'",
                        wellPanel(             
                          #h4("Upload Sample Clinical Data File"),
                          #tags$hr(),
                          fileInput(inputId="clinical", label="Upload Clinical Data File")
                    )),
                     conditionalPanel(condition = "input.Twodim == 5 & input.analysisType == '2d'",
                        wellPanel(             
                          #h4("Upload Sample Clinical Data File"),
                          #tags$hr(),
                          fileInput(inputId="clinical2", label="Upload Clinical Data File")
                    )),
                     conditionalPanel(condition = "input.Threedim == 5 & input.analysisType == '3d'",
                        wellPanel(             
                          #h4("Upload Sample Clinical Data File"),
                          #tags$hr(),
                          fileInput(inputId="clinical3", label="Upload Clinical Data File")
                    )),
                     conditionalPanel(condition = "input.Onedim == 6 & input.analysisType == '1d'",
                                      wellPanel(             
                                        h4("Number of Bootstraps"),
                                        tags$hr(),
                                        textInput("rep", "", value="0", width="149px"),
                                        actionButton("do", "Submit")
                    )),
                    conditionalPanel(condition = "input.Twodim == 6 & input.analysisType == '2d'",
                                wellPanel(             
                                  h4("Number of Bootstraps"),
                                  tags$hr(),
                                  textInput("rep2", "", value="0", width="149px"),
                                  actionButton("do2", "Submit")
                    )),
                    conditionalPanel(condition = "input.Threedim == 6 & input.analysisType == '3d'",
                                wellPanel(             
                                  h4("Number of Bootstraps"),
                                  tags$hr(),
                                  textInput("rep3", "", value="0", width="149px"),
                                  actionButton("do3", "Submit")
                                ))
               
                )
        ) #fluid Row
        
))#shinyServer

