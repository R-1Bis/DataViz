library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggplot2)

#Useful variables
CP_lat <- 40.783027
CP_lon <- -73.965126

#Loading data from csv file and renaming position columns
datafile <- './data/2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv'
squirrel_data <- read.csv(datafile)
colnames(squirrel_data)[1] <-"longitude"
colnames(squirrel_data)[2] <-"latitude"

#Converting date strings to Date objects
new_dates<-c()
for (d in squirrel_data$Date ){
  d<-as.Date(toString(d),format="%m%d%Y")
  new_dates<-append(new_dates,d)
}
squirrel_data$Date<-new_dates


#Rotation
rotate <- function(x0,y0,tet) {
  a<-cos(tet*pi/180)
  b<-sin(tet*pi/180)
  R <- matrix(c(a, b, -b, a), ncol = 2, nrow = 2)
  v0<- c(x0,y0)
  v1<- R%*%v0
  return (c(v1[1],v1[2]))
}


new_lat<-c()
new_lon<-c()
new<-c()
for (i in 1:length(squirrel_data$latitude)) {
  pos<-rotate(squirrel_data$latitude[i],squirrel_data$longitude[i],0)
  new_lat<-append(new_lat,pos[1])
  new_lon<-append(new_lon,pos[2])
}
squirrel_data$newLat<-new_lat
squirrel_data$newLon<-new_lon

#Show data summary
print(head(squirrel_data))

ui <- fluidPage(
  fluidRow(
    
    column(8,
           h3("Map of Squirrel presence in the chosen date range"),
           leafletOutput("map")
           ),
    
    column(4,
           dateRangeInput("dates",label=h3("Date range"),
                          min = "2018-10-01",max = "2018-10-31",
                          start = "2018-10-01",end="2018-10-31"),
           hr(),
           numericInput("num",label=h3("Latitude"),value=-36)
           )
  ),
  
  fluidRow(
    column(12,
           plotOutput("plot")
    )
  )
)
  



server <- function(input, output) {
  
  dataInput <- reactive({
    squirrel_data<-squirrel_data[squirrel_data$Date>=input$dates[1] & squirrel_data$Date<=input$dates[2],]
    squirrel_data
  })
  
  dataRotate <-reactive({
    new_lat<-c()
    new_lon<-c()
    new<-c()
    data<-dataInput()
    for (i in 1:length(data$latitude)) {
      pos<-rotate(data$latitude[i],data$longitude[i],input$num)
      new_lat<-append(new_lat,pos[1])
      new_lon<-append(new_lon,pos[2])
    }
    data$newLat<-new_lat
    data$newLon<-new_lon
    data
    
  })
  
  
  output$value <- renderPrint({input$dates})
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng=CP_lon,lat=CP_lat,zoom=14) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircles(
        data=dataInput(),
        radius=1,
        fillOpacity=1,
        popup=paste(
          "<strong>Time: </strong>", dataInput()$Date, "<br>"
        )
      )
  })
  
  output$plot <- renderPlot({
    data<-dataRotate()
    plot(data$newLat,data$newLon, xlab = "Latitude", ylab = "Longitude")
  })
}


shinyApp(ui = ui, server = server)
