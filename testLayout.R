library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggplot2)



#Useful variables for map center
CP_lat <- 40.783027
CP_lon <- -73.965126

#Loading data from csv file and renaming position columns
datafile <- './data/2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv'
squirrel_data <- read.csv(datafile)
colnames(squirrel_data)[1] <-"longitude"
colnames(squirrel_data)[2] <-"latitude"
colnames(squirrel_data)[29] <- "run_from"

#Converting date strings to Date objects
new_dates<-c()
for (d in squirrel_data$Date ){
  d<-as.Date(toString(d),format="%m%d%Y")
  new_dates<-append(new_dates,d)
}
squirrel_data$Date<-new_dates


#Rotation function
rotate <- function(x0,y0,tet=-29.85) {
  a<-cos(tet*pi/180)
  b<-sin(tet*pi/180)
  R <- matrix(c(a, b, -b, a), ncol = 2, nrow = 2)
  #Changing origin to park corner and scaling to meter units
  v0<- 1609.34*c((x0-40.764361)*69,(y0+73.973039)*54.6)
  v1<- R%*%v0
  return (c(v1[1],v1[2]))
}

#Calculating new coordinates for grid plots
new_lat<-c()
new_lon<-c()
new<-c()
for (i in 1:length(squirrel_data$latitude)) {
  pos<-rotate(squirrel_data$latitude[i],squirrel_data$longitude[i],tet=0)
  new_lat<-append(new_lat,pos[1])
  new_lon<-append(new_lon,pos[2])
}
squirrel_data$newLat<-new_lat
squirrel_data$newLon<-new_lon

#Show data summary
# print(head(squirrel_data))



ui <- fluidPage(
  fluidRow(
    
    column(8,
           h3("Map of Squirrel presence in Central Park"),
           leafletOutput("map"),
           h4("The map here above presents squirrel presence in central park for the data range selected. 
              The map shows the squirrels that present the selected behavior.")
           ),
    
    column(4,
           dateRangeInput("dates",label=h3("Date range"),
                          min = "2018-10-01",max = "2018-10-31",
                          start = "2018-10-01",end="2018-10-31"),
           h4("Changing the Theta parameter will change the orientation of the plot here under on the left."),
           numericInput("num",label=h3("Theta"),value=-29.85),
           selectInput("select", h3("Behavior"), 
                       choices = list("ALL",
                                      "Approaches humans",
                                      "Indifferent", 
                                      "Run from humans"), 
                       selected = "ALL"),
           )
  ),
  
  fluidRow(
    column(6,
           plotOutput("plot")
           ),
    column(6,
           plotOutput(outputId = "histPlot"),
           h4("This histogram presents the number of squirrels found in Central Park within the previously selected time
              range and with the chosen behavior")
           )
    )
)
  



server <- function(input, output) {
  
  #Time window filter
  dataInput <- reactive({
    if(input$select=="ALL"){
      squirrel_data<-squirrel_data[squirrel_data$Date>=input$dates[1] & squirrel_data$Date<=input$dates[2],]
    }else{
      if(input$select=="Approaches humans"){
        squirrel_data<-squirrel_data[squirrel_data$Date>=input$dates[1] & squirrel_data$Date<=input$dates[2] & squirrel_data$Approaches=="true",]
      }else{
        if(input$select=="Indifferent"){
          squirrel_data<-squirrel_data[squirrel_data$Date>=input$dates[1] & squirrel_data$Date<=input$dates[2] & squirrel_data$Indifferent=="true",]
        }else{
          squirrel_data<-squirrel_data[squirrel_data$Date>=input$dates[1] & squirrel_data$Date<=input$dates[2] & squirrel_data$run_from=="true",]
        }
      }
    }
    squirrel_data
  })
  
  #Reactive rotation process (might not be used in final app ?)
  dataRotate <-reactive({
    new_lat<-c()
    new_lon<-c()
    new<-c()
    data<-dataInput()
    for (i in 1:length(data$latitude)) {
      pos<-rotate(data$latitude[i],data$longitude[i], input$num)
      new_lat<-append(new_lat,pos[1])
      new_lon<-append(new_lon,pos[2])
    }
    data$newLat<-new_lat
    data$newLon<-new_lon
    data
  })
  
  #Time histogram plot
  output$histPlot <- renderPlot({
    
    bins<-c()
    theDate<-input$dates[1]
    while(theDate<=input$dates[2]) {
      bins<-append(bins,theDate)
      theDate<-theDate+1
    }
    
    hist(dataInput()$Date,breaks=bins, col = "#75AADB", border = "white", xlab = "Date", ylab = "number of squirrel", freq = TRUE, main = "Histogram of squirrel presence in time")
  })
  
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
