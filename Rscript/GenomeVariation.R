


af293.snp_ratio.table.v10.mat<-readRDS("data/af293.snp_ratio.table.v10.mat.rds")
sample_info.filtered.final<-readRDS("data/sample_info.filtered.final.rds")


sidebarLayout(
  sidebarPanel(width = 3,
               sliderInput(inputId = "bins",
                           label = "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30),
  ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput(outputId = "distPlot")
  )
)
