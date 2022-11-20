#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyBS)
library(tidyverse)
library(ComplexHeatmap)
library(circlize)

# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("RnaVarBrowser"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

sample_info.filtered.final<-readRDS("data/sample_info.filtered.final.rds")


shinyUI=fluidPage(theme = shinytheme("cerulean"),
                  
                  titlePanel("RnaVarBrowser"),
                  navbarPage("",
                             tabPanel(icon("home"),
                                      
                                      fluidRow(#column(tags$img(src="Antioquia.png",width="200px",height="260px"),width=2),
                                               column(
                                                 width=6,
                                                 br(),
                                                 p("RnaVarBrowser is a webtools which can  visulize the variations identified from RNA-seq data. it can show the"),
                                                 p("RnaVarBrowser can also compare users' input data with public data, present the relationship between datasets and group similar datasets together"),
                                                 br()
                                               )
                                      ),
                                      
                                      hr(),
                              
                                      
                                      hr(),
                                      p(em("Developed by"),br("Miao Zhengqiang"),style="text-align:center; font-family: times")
                             ),
                             tabPanel("Genome Variation",
                                      br(),
                                      
                                      
                                      
                                      
                                      sidebarLayout(
                                        
                                        sidebarPanel(
                                          width = 3,
                                        
                                          tags$h3("Select Sample:"),
                                          bsCollapse(id = "collapseExample", open = NULL,
                                                     
                                                     bsCollapsePanel(
                                                       "Select by Strain",
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           div(
                                                             style = "height:auto;overflow-y:auto;",
                                                             checkboxGroupInput(
                                                               "Strain",
                                                               "Strain:",
                                                               choiceNames =unique(sample_info.filtered.final[, 1]),
                                                               choiceValues =unique(sample_info.filtered.final[, 1]),
                                                               selected = unique(sample_info.filtered.final[, 1])
                                                             )
                                                           )
                                                         ),
                                                         column(12, hr()),
                                                         column(12, "You chose"),
                                                         
                                                         column(12, div(style = "height:auto;overflow-y:auto;",
                                                                        textOutput("Strain")))
                                                         
                                                       ),
                                                       style = "info"
                                                     ),
                                                     bsCollapsePanel(
                                                       "Select by Substrain",
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           div(
                                                             style = "height:auto;overflow-y:auto;",
                                                             checkboxGroupInput(
                                                               "Substrain",
                                                               "Substrain:",
                                                               
                                                               choiceNames =
                                                                 unique(sample_info.filtered.final[, 2]),
                                                               choiceValues =
                                                                 unique(sample_info.filtered.final[, 2]),
                                                               #selected = unique(sample_info.filtered.final[, 2])
                                                             ),
                                                             
                                                           )
                                                         ),

                                                         column(12, "You chose"),
                                                         column(12, div(style = "height:auto;overflow-y:auto;",
                                                                      textOutput("txt")))
                                                         
                                                       ),
                                                       style = "info"
                                                     ),
                                                     bsCollapsePanel(
                                                       "Select by Experiment",
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           div(
                                                             style = "height:auto;overflow-y:auto;",
                                                             checkboxGroupInput(
                                                               "Experiment",
                                                               "Experiment:",
                                                               
                                                               choiceNames =
                                                                 unique(sample_info.filtered.final[, 3]),
                                                               choiceValues =
                                                                 unique(sample_info.filtered.final[, 3]),
                                                               #selected = unique(sample_info.filtered.final[, 3])
                                                             ),
                                                             
                                                           )
                                                         ),
                                                         
                                                         column(12, "You chose"),
                                                         column(12, div(style = "height:auto;overflow-y:auto;",
                                                                        textOutput("Experiment")))
                                                         
                                                       ),
                                                       style = "info"
                                                     ),
                                                     bsCollapsePanel(
                                                       "Select by Dataset",
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           div(
                                                             style = "height:auto;overflow-y:auto;",
                                                             checkboxGroupInput(
                                                               "Dataset",
                                                               "Dataset:",
                                                               
                                                               choiceNames =
                                                                 unique(sample_info.filtered.final[, 4]),
                                                               choiceValues =
                                                                 unique(sample_info.filtered.final[, 4]),
                                                               #selected = unique(sample_info.filtered.final[, 3])
                                                             ),
                                                             
                                                           )
                                                         ),
                                                         
                                                         column(12, "You chose"),
                                                         column(12, div(style = "height:auto;overflow-y:auto;",
                                                                        textOutput("Dataset")))
                                                         
                                                       ),
                                                       style = "info"
                                                     )
                                          ),
                                          
                                        ),

                                          # Show a plot of the generated distribution
                                        mainPanel(
                                          navbarPage("Analyisis:", 
                                                     tabPanel("Variation Overview",
                                                              tags$p("##: Large plot needs wait for ~3 mins.", style="color:grey;"),
                                                              #plotOutput(outputId = "heatmap",height="2000px",width = "1500px"),
                                                             
                                                              ),
                                                      tabPanel("TSNE",
                                                               p(""),
                                                               #plotOutput(outputId = "tsne")
                                                               tags$p("##: Large plot needs wait for ~30 seconds.", style="color:grey;"),
                                                               sliderInput(inputId = "perplexity",
                                                                           label = "perplexity:",
                                                                           min = 3,
                                                                           max = 30,
                                                                           value = 30),
                                                               shinycssloaders::withSpinner(
                                                                 plotOutput("tsne")
                                                               ),
                                                      )
                                                               
                                                              
                                                     ),
                                            br(),br(),
                                            
                                            #print(output$data)
                                            
                                            
                                        )
                                        
                                      )
  
                             ),
                             tabPanel("Allelic Imbalance",
                                      
                                      fluidRow(column(width=1),
                                               column(
                                                 h4(p("Exploratory analysis",style="color:black;text-align:center")),
                                                 width=6,style="background-color:lavender;border-radius: 10px")),
                                      br(),
                                      #source("Rscript/FungiExz.R")
                                      
                                      
                             ),
                             tabPanel("Data Preparing",
                                      
                                      fluidRow(column(width=1),
                                               column(
                                                 h4(p("Exploratory analysis",style="color:black;text-align:center")),
                                                 width=6,style="background-color:lavender;border-radius: 10px")),
                                      br()
                                      
                                      
                             ),
                             tabPanel("News&Updates",
                                      
                                      fluidRow(column(width=1),
                                               column(
                                                 h4(p("Exploratory analysis",style="color:black;text-align:center")),
                                                 width=6,style="background-color:lavender;border-radius: 10px")),
                                      br()
                                      
                                      
                             ),
                             tabPanel("Tutorial",
                                      
                                      fluidRow(column(width=1),
                                               column(
                                                 h4(p("Exploratory analysis",style="color:black;text-align:center")),
                                                 width=6,style="background-color:lavender;border-radius: 10px")),
                                      br()
                                      
                                      
                             ),
                             
                             tabPanel("Downloads",
                                      
                                      fluidRow(column(width=1),
                                               column(
                                                 h4(p("Exploratory analysis",style="color:black;text-align:center")),
                                                 width=6,style="background-color:lavender;border-radius: 10px")),
                                      br()
                                      
                                      
                             ),
                             tabPanel("Contact",
                                      
                                      fluidRow(column(width=1),
                                               column(
                                                 h4(p("Exploratory analysis",style="color:black;text-align:center")),
                                                 width=6,style="background-color:lavender;border-radius: 10px")),
                                      br()
                                      
                                      
                             )
                             
                  )
                  
                  
                  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    af293.snp_ratio.table.v10.mat<-readRDS("data/af293.snp_ratio.table.v10.mat.rds")
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })

    
    output$txt <- renderText({
      Substrain <- paste(input$Substrain, collapse = ", ")
      paste(Substrain)
    })
    output$Strain <- renderText({
      Strain <- paste(input$Strain, collapse = ", ")
      paste(Strain)
    })
    output$Experiment <- renderText({
      Experiment <- paste(input$Experiment, collapse = ", ")
      paste(Experiment)
    })
    output$Dataset <- renderText({
      Dataset <- paste(input$Dataset, collapse = ", ")
      paste(Dataset)
    })
    output$heatmap<-renderPlot({
      
      
      
      mat=af293.snp_ratio.table.v10.mat[,input$sample]
      
      mat.ha_column = HeatmapAnnotation(
        df = data.frame(
          strains = sample_info.filtered.final[input$sample,1],
          derivatives = sample_info.filtered.final[input$sample,2],
          dataset=sample_info.filtered.final[input$sample,3]
        ),
        col = list(
          isolates = c(
            "Af293"="#e31a1c",
            "CEA17"="#1f78b4",
            "D141"="#b15928",
            "other"="#6a3d9a",
            "ATCC46645"="#fdbf6f"
          ),
          derivatives = c(
            "CEA10"="#a6cee3",
            "A1163"="#1f78b4",
            "A1160-"="#b2df8a",
            "A1160+"="#33a02c",
            "ΔakuKU80_A1160"="#ff7f00",
            "Af293"="#e31a1c",
            "ATCC46645"="#fdbf6f",
            "AfS35"="#cab2d6",
            "AfIR974"="#6a3d9a",
            "AfIR964"="#ffff99",
            "D141"="#b15928",
            "AF293.1"="#fb9a99"
          )
        )
      )
      
      mat.heatmap = Heatmap(
        mat,
        rect_gp = gpar(lty=0),
        col = colorRamp2(c(-1,0,0.5,1),c("grey","blue", "yellow", "red")),
        cluster_columns = TRUE,
        cluster_rows = FALSE ,
        row_names_gp = gpar(fontsize = 16),
        column_names_gp = gpar(fontsize = 8),
        row_dend_width = unit(100, "mm"),
        column_dend_height = unit(10, "mm"),
        show_row_names = FALSE,
        show_column_names = TRUE,
        gap = unit(1, "mm"),
        column_names_max_height = unit(2, "cm"),
        row_names_max_width = unit(2, "cm"),
        #row_names_side = c("left"),
        #row_labels = c("pyrG","akuB","An_pyrG","pyrG_mut"),
        heatmap_legend_param = list(color_bar = "continuous"),
        column_title = "Samples",
        name = "variation ratio",
        jitter = TRUE,
        row_split = unlist(strsplit(rownames(af293.snp_ratio.table.v10.mat)," ",fixed=T))[seq(1,dim(mat)[1]*2,2)],
        column_split = sample_info.filtered.final[input$sample,1],
        top_annotation=mat.ha_column
      )
      mat.heatmap
      
    })
    
    output$tsne <- renderPlot({
      sample=c()
      if(length(input$Dataset)>0){
        print(input$Dataset)
        sample=input$Dataset
      }
      else if(length(input$Experiment)>0){
        print(input$Experiment)
        sample=sample_info.filtered.final[sample_info.filtered.final[,3] %in% input$Experiment,4]
        
      }
      else if(length(input$Substrain)>0){
        print(input$Substrain)
        sample=sample_info.filtered.final[sample_info.filtered.final[,2] %in% input$Substrain,4]
        
      }
      else if(length(input$Strain)>0){
        print(input$Strain)
        sample=sample_info.filtered.final[sample_info.filtered.final[,1] %in% input$Strain,4]
      }
      
      print(sample)
      
      
      
      library(Rtsne) # Load package
      set.seed(1234) # Sets seed for reproducibility
      #tsne <- Rtsne(t(af293.snp_ratio.table.v10.mat[,input$sample])) # Run TSNE
      isolates = c(
        "Af293"="#e31a1c", 
        "CEA17"="#1f78b4",
        "D141"="#b15928",
        "other"="#6a3d9a",
        "ATCC46645"="#fdbf6f"
      )
      
      isolates=isolates[names(isolates) %in% sample_info.filtered.final[sample,1]]
      
      tsne <- Rtsne(t(af293.snp_ratio.table.v10.mat[,sample]),perplexity = input$perplexity) 
      plot(tsne$Y,col=isolates[sample_info.filtered.final[sample,1]],asp=1,pch=16) # Plot the result
      legend("topright",legend=names(isolates), col = isolates, pch =16 ,bty="n", horiz=FALSE,cex=1,text.col="black")
      #plot(1)
      },width=800,height = 600)
   
    
    
    

}

# Run the application 
shinyApp(ui = shinyUI, server = server)
