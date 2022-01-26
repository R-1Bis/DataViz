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
  pos<-rotate(squirrel_data$latitude[i],squirrel_data$longitude[i])
  new_lat<-append(new_lat,pos[1])
  new_lon<-append(new_lon,pos[2])
}
squirrel_data$newLat<-new_lat
squirrel_data$newLon<-new_lon


ui <- fluidPage(
  fluidRow(
    h2("October 2018 Squirrel Census in Central Park"),
    p("This little application presents data collected in the ",
         a("2018 Squirrel Census", href ="https://data.cityofnewyork.us/Environment/2018-Squirrel-Census-Fur-Color-Map/fak5-wcft"),
         ". We offer different visualisation tools to examine this dataset. These visual tools are parameterized by a time range 
      and the behavior of the squirrels.", style="font-size:16px"),
    
    column(8,
           h3("Map of Squirrel presence in Central Park"),
           leafletOutput("map"),
           p("The map here above presents squirrel presence in central park for the time range selected. 
              The map shows the squirrels that present the selected behavior. Clicking a point will display additional data about the sighting.", style="font-size:16px"),
           
           h3("Map of Squirrel/Human interactions"),
           plotOutput("plot"),
           p("The map here above shows where and how squirrels behave with humans. Red dots represent occasions when squirrels ran away, green ones when they approached
             and grey ones when they stayed indifferent.", style="font-size:16px")
           ),
    
    column(4,
           dateRangeInput("dates",label=h3("Date range"),
                          min = "2018-10-01",max = "2018-10-31",
                          start = "2018-10-01",end="2018-10-31"),
           selectInput("select", h3("Behavior"), 
                       choices = list("ALL",
                                      "Approaches humans",
                                      "Indifferent", 
                                      "Run from humans"), 
                       selected = "ALL"),
           h3("Time repartition of sightings"),
           plotOutput(outputId = "histPlot"),
           p("This histogram presents the number of squirrels found in Central Park within the previously selected time 
             range and with the chosen behavior", style="font-size:16px"),
           h3("Proportions of squirrel/Human interactions"),
           plotOutput(outputId = "pie")
           )
    )
)



server <- function(input, output) {
  
  #Time window filter
  dataTime <- reactive({
    squirrel_data<-squirrel_data[squirrel_data$Date>=input$dates[1] & squirrel_data$Date<=input$dates[2],]
    if(input$select=="Approaches humans") squirrel_data<-squirrel_data[squirrel_data$Approaches=="true",]
    if(input$select=="Indifferent") squirrel_data<-squirrel_data[squirrel_data$Indifferent=="true",]
    if(input$select=="Run from humans") squirrel_data<-squirrel_data[squirrel_data$run_from=="true",]
    squirrel_data
  })
  
  #Human interactions filter
  dataHuman <-reactive({
    data<-dataTime()
    if(input$select=="Approaches humans") data<-data[data$Approaches=="true",]
    if(input$select=="Indifferent") data<-data[data$Indifferent=="true",]
    if(input$select=="Run from humans") data<-data[data$run_from=="true",]
    data
  })
  
  #Reactive rotation process
  dataRotate <-reactive({
    new_lat<-c()
    new_lon<-c()
    new<-c()
    data<-dataHuman()
    for (i in 1:length(data$latitude)) {
      pos<-rotate(data$latitude[i],data$longitude[i])
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
    hist(dataHuman()$Date,breaks=bins, col = "#75AADB", xlab = "Date", ylab = "number of squirrel", freq = TRUE, main = "Histogram of squirrel presence in time")
  })
  
  #Leaflet map render
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng=CP_lon,lat=CP_lat,zoom=14) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircles(
        data=dataHuman(),
        radius=1,
        fillOpacity=1,
        popup=paste(
          "<strong>Time: </strong>", dataHuman()$Date, "<br>",
          "<strong>Location: </strong>", dataHuman()$latitude,", ", dataHuman()$longitude, "<br>",
          "<strong>Color: </strong>", dataHuman()$Primary.Fur.Color, "<br>"
        )
      )
  })
  #Custom map plot rendering
  output$plot <- renderPlot({
    data<-dataRotate()
    colors<-c()
    si<-c()
    for (r in 1:nrow(data)){
      if (data[r,"Approaches"]=="true") {
        colors<-append(colors,'green')
        si<-append(si,2)
      }
      else if (data[r,"run_from"]=="true"){
        colors<-append(colors,'red')
        si<-append(si,2)
      }
      else{
        colors<-append(colors,'grey')
        si<-append(si,1)
      }
    }
    plot(data$newLat,data$newLon,
         xlab = "X", ylab = "Y",
         col=colors,
         xlim = c(0,4000), ylim=c(-800,0),
         pch=19, bg=par("bg"), cex=si
         )
  })
  
  #Behavior chart render
  output$pie <- renderPlot({
    data<-dataTime()
    app <- (data %>%  count(Approaches))[2,2]
    run <- (data %>%  count(run_from))[2,2]
    ind <- (data %>%  count(Indifferent))[2,2]
    x<-c(app,run,ind)
    pie(x,labels=c(paste(app," Approaches"),paste(run," Run away"),paste(ind," Indifferent")))
  })
}


shinyApp(ui = ui, server = server)
