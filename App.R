library(shiny)
library(maps)
library(mapproj)
# need to install.packages(c("maps", "mapproj"))
source("./helpers.R")
counties <- readRDS("data/counties.rds")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("CensusVis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps from the 2010 US Census."),
      selectInput("select", strong("Choose a variable to display"), 
                  choices = list("percent white",
                                 "percent asian",
                                 "percent hispanic", 
                                 "percent black"), 
                                  selected = "percent white"),
      sliderInput("slider", strong("Range of interest"),
                  min = 0, max = 100, value = c(0,100))
    ),
    # send var selecter and slider to output 
    mainPanel(
      plotOutput("map"))
  )
)



server <- function(input, output) {
  
  output$map <- renderPlot({
    # select the good data in counties
    data <- switch(input$select, 
                   "percent white" = counties$white,
                   "percent black" = counties$black,
                   "percent hispanic" = counties$hispanic,
                   "percent asian" = counties$asian)
    colour <- switch(input$select, 
                    "percent white" = "darkgreen",
                    "percent black" = "black",
                    "percent hispanic" = "darkorange",
                    "percent asian" = "darkviolet")
    
    legend <- switch(input$select, 
                     "percent white" = "% white",
                     "percent black" = "% black",
                     "percent hispanic" = "% hispanic",
                     "percent asian" = "% asian")
    
    #percent_map(data, colour, legend, min, max)
    percent_map(data, colour, legend, input$slider[1], input$slider[2])
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)