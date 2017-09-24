#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- # Define UI for miles per gallon application
  shinyUI(pageWithSidebar(
    
    headerPanel("K-means Clustering of Select Datasets"),
    
    sidebarPanel(
      #Select dataset
      uiOutput("choose_dataset"),
      # Select X variable
      uiOutput("choose_xvar"),
      # Select Y variable
      uiOutput("choose_yvar"),
      numericInput('clusters', 'Cluster Count', 3,
                   min = 1, max = 7)
    ),
    
    
    mainPanel(
      plotOutput('plot1')
    )
  ))


# Set datasets to choose from
data_sets <- c(
               "USArrests",
               "USJudgeRatings",
               "ChickWeight",
               "cars",
               "iris",
               "mtcars"
               )

# Server section below
server<-shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_dataset <- reactiveUI(function() {
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  
  # Drop-down selection box for xvar
  output$choose_xvar <- reactiveUI(function() {
    dat <- get(input$dataset)
    colnames <- names(dat)
    selectInput("xvar", "X Variable", as.list(colnames))
  })
  
  # Drop-down selection box for yvar
  output$choose_yvar <- reactiveUI(function() {
    dat <- get(input$dataset)
    colnames <- names(dat)
    selectInput("yvar", "Y Variable", as.list(colnames),selected=colnames[2])
  })
  
  # Combine select variable into a new data frame
  selectedData<-reactive({
    dat <- get(input$dataset)
    dat[, c(input$xvar, input$yvar)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  # Create the plot
  output$plot1 <- renderPlot({
    palette(c("red", "blue", "green", "orange",
              "purple", "yellow", "black"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 4)
    points(clusters()$centers, pch = 3, cex = 5, lwd = 3)
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)

