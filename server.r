# Data set http://stat-computing.org/dataexpo/2009/the-data.html

#install.packages("devtools")
#install.packages("ggmap")
#install.packages("ggrepel")
#install.packages("readr")
#install.packages("ggthemes")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("dplyr")
#install.packages("ggplot2")

library(ggplot2)
library(ggmap)
library(ggrepel)
library(readr)
library(ggthemes)
library(shiny)
library(shinydashboard)
library(dplyr)

#Load Data Set and preparing airline names       
DelayedFlights <- read_csv("On_Time_On_Time_Performance1.csv")
DelayedFlights$DepTime <- ifelse(substr(DelayedFlights$DepTime,1,1)=="0", as.numeric(substr(DelayedFlights$DepTime,2,4)) , as.numeric(substr(DelayedFlights$DepTime,1,4)))
DelayedFlights$ArrTime <- ifelse(substr(DelayedFlights$ArrTime,1,1)=="0", as.numeric(substr(DelayedFlights$ArrTime,2,4)) , as.numeric(substr(DelayedFlights$ArrTime,1,4)))

DelayedFlights[DelayedFlights$UniqueCarrier=="AA", 7]<-"American Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="AS", 7]<-"Alaska Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="B6", 7]<-"Jet Blue Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="DL", 7]<-"Delta Air Lines"
DelayedFlights[DelayedFlights$UniqueCarrier=="UA", 7]<-"United Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="OO", 7]<-"SkyWest  Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="WN", 7]<-"Southwest Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="VX", 7]<-"Virgin Atlantic Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="F9", 7]<- "Frontier Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="HA", 7]<-"Hawaiian Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="EV", 7]<-"Atlantic Southeast Airlines"
DelayedFlights[DelayedFlights$UniqueCarrier=="NK", 7]<-"Spirit Airlines"

#Compute Total Delay Time and transform NA to 0
DelayedFlights["Total_Delay"] <- (DelayedFlights$CarrierDelay + DelayedFlights$WeatherDelay + DelayedFlights$NASDelay + DelayedFlights$SecurityDelay + DelayedFlights$LateAircraftDelay)
DelayedFlights["Total_Delay"] <- ifelse(is.na(DelayedFlights$Total_Delay),0,DelayedFlights$Total_Delay)
DelayedFlights["DelayBoolean"] <- ifelse(DelayedFlights$Total_Delay>0,"Delayed","On Time")
DelayedFlights[is.na(DelayedFlights)] <- 0


#Start of Server Script
shinyServer(function(input, output, session) {
  
  #Start of Progress Bar Loader    
  withProgress(message = 'Loading Application', style = "notification", value = 0.1, {
    incProgress(1/15)
    Sys.sleep(0.1)  
    
    #Progress Bar Update        
    incProgress(3/15)
    withProgress(message = 'Creating Map Data', style = "notification", value = NULL, {Sys.sleep(1)})
    
    #Prepare Data for Map Plot by allocating coordinates from Google Maps
    FlightData<-reactive({
      DLFlights <- table(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]$Origin, DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]$Dest)
      DLFlights <-as.data.frame(as.table((rowSums(DLFlights, na.rm = FALSE, dims = 1))))
      colnames(DLFlights)[2] <- "Number_Flights"
      colnames(DLFlights)[1] <- "Destination"
      geocodedata <- data.frame(read.table(file = "geocodedat.csv", sep=";", header = TRUE))
      DLFlightsShort <- filter(DLFlights, Number_Flights>0)
      DLFlightsJoin <-left_join(DLFlightsShort, geocodedata, by = "Destination")
      DLFlightsJoin <- na.omit(DLFlightsJoin)
      DLFlightsJoin <- arrange(DLFlightsJoin, desc(Number_Flights))
      DLFlightsJoin["lon.x"] <- DLFlightsJoin[1,4]
      DLFlightsJoin["lat.x"] <- DLFlightsJoin[1,5]
      DLFlightsJoin<- rename(DLFlightsJoin, lon.y=lon, lat.y=lat)
      DLFlightsJoin["SizeVariable"] <- ((((2.5-0.5)*(DLFlightsJoin$Number_Flights-min(DLFlightsJoin$Number_Flights)))/(max(DLFlightsJoin$Number_Flights)-min(DLFlightsJoin$Number_Flights)))+0.5)
      FlightData<-DLFlightsJoin
      print(nrow(FlightData))
      FlightData
      
      })
    
    #Change route selection slider range dependent on airline number of routes
    observe({
      val <- nrow(FlightData())
      updateSliderInput(session, "TopRange", value = 5,
                        min = 1, max = val, step = 1)
      print(val)
    })
    
    #Progress Bar Update    
    incProgress(1/15)
    withProgress(message = 'Plotting Map Data', style = "notification", value = NULL, {Sys.sleep(1)})
    
    #World map plot
    output$worldplot <- renderPlot({ggplot() + borders("usa", colour="#efede1", fill="#efede1") + 
        geom_curve(data=FlightData()[2:input$TopRange+1,], aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y), col = "#b29e7d", size = FlightData()[2:input$TopRange+1,8] , curvature = .2) + 
        geom_point(data=FlightData()[2:input$TopRange+1,], aes(x = lon.y, y = lat.y), col = "#2c3e50", size = FlightData()[2:input$TopRange+1,8]+4) + 
        geom_point(data=FlightData()[2:input$TopRange+1,], aes(x = lon.x, y = lat.x, size = 4), col = "blue") +
        geom_text_repel(data=FlightData()[2:input$TopRange+1,], aes(x = lon.y, y = lat.y, label = FlightData()[2:input$TopRange+1,1]), col = "black", size = FlightData()[2:input$TopRange+1,8]+3, segment.color = NA) + 
        coord_fixed(ratio = 1.3/1) + 
        {if(input$MapLabels)geom_label_repel(data=FlightData()[2:input$TopRange+1,], aes(x = lon.y, y = lat.y, label = FlightData()[2:input$TopRange+1,2]), col = "black", size = FlightData()[2:input$TopRange+1,8]+3, segment.color = NA)} +
        geom_text_repel(data=FlightData()[1,], aes(x = lon.x, y = lat.x, label = FlightData()[1,1]), col = "blue", size = 4, segment.color = NA) +
        theme(panel.background = element_rect(fill="white"), 
              axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position="none")})
      
    #Progress Bar Update  
    incProgress(1/15)
    withProgress(message = 'Plotting Top 10 Flights', style = "notification", value = NULL, {Sys.sleep(1)})
    
    
    #PieChart Number of Flights
    FlightPercentage <- reactive({data.frame(Airline = c(input$Airline, "Other"),
                                   Percentage = c(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,])/nrow(DelayedFlights),
                                                  1-(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,])/nrow(DelayedFlights))))})
    
    output$piechart1 <- renderPlot({ggplot(FlightPercentage(), aes(x="", y=Percentage, fill=Airline))+
      geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+
      theme_minimal()+geom_label(aes(label = paste(Airline,round(Percentage*100,2),"%")),position = position_stack(vjust = 0.5)) +
      theme(panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5))+ggtitle("Percentage of Total Flights")})
    
    
    FlightDelayed <- reactive({data.frame(Delay = c("Delayed", "Not Delayed"),
                                Percentage = c(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline & DelayedFlights$Total_Delay>0,])/nrow(DelayedFlights),
                                               1-(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline & DelayedFlights$Total_Delay>0,])/nrow(DelayedFlights))))})
    
    
    
    output$piechart2 <- renderPlot({ggplot(FlightDelayed(), aes(x="", y=Percentage, fill=Delay))+
        geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+
        theme_minimal()+geom_label(aes(label = paste(Delay,round(Percentage*100,2),"%")), position = position_stack(vjust = 0.5)) +
        theme(panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5))+ggtitle("Percentage of Flights Delayed")})
    
    
    FlightDelayedType <- reactive({FlightDelayed <- data.frame(Delay = c("Carrier Delay","Weather Delay","NAS Delay","Security Delay","Late Aircraft Delay"),
                                                           Percentage = c(nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & CarrierDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                          nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & WeatherDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                          nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & NASDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                          nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & SecurityDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                          nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & LateAircraftDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,])))
                                                sumperc <- sum(FlightDelayed$Percentage)
                                                for (i in 1:5){
                                                  FlightDelayed[i,2]<- FlightDelayed[i,2]/sumperc
                                                }
                              FlightDelayed
                              })
    
    output$piechart3 <- renderPlot({ggplot(FlightDelayedType(), aes(x="", y=Percentage, fill=Delay))+
        geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+
        theme_minimal()+geom_label(aes(label = paste(Delay,round(Percentage*100,2),"%")), position = position_stack(vjust = 0.5)) +
        theme(panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5))+ggtitle("Percentage of Delays per Type")})
    
    #Progress Bar Update         
    incProgress(1/15)
    withProgress(message = 'Calculating Performance', style = "notification", value = NULL, {Sys.sleep(2)})
    
    #Prepare Data for Performance
    NumberAirplane <- reactive({nrow(unique(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline, 12]))})
    NumberFlights <- reactive({nrow(unique(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline, 11]))})
    NumberDestinations <- reactive({nrow(unique(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline, 19]))})
    
    output$AirplaneNumbertext <- renderText(paste("Number of airplane operated by:", input$Airline))
    output$FlightsNumbertext <- renderText(paste("Number of routes operated by:", input$Airline))
    output$Destinationstext <- renderText(paste("Number of flight destinations for:", input$Airline))
    output$AirplaneNumberX <- renderText(NumberAirplane())
    output$FlightsNumberX <- renderText(NumberFlights())
    output$DestinationsX <- renderText(NumberDestinations())
    
        #Progress Bar Update      
    incProgress(2/15)
    withProgress(message = 'Plotting Delay Data', style = "notification", value = NULL, {Sys.sleep(1)})
    
    #Delay Distribution Plot
    output$DelayTypePlot <- renderPlot({
      ggplot(data=DelayDataPlot(), aes(x = Month, y = Duration/60, fill=Type))+ geom_bar(stat = "Identity", position = "Stack")+ylab("Delay in hours")+ scale_x_discrete(limits=c("Month")) + scale_fill_manual("legend", values = c("Carrier Delay" = "#dc6900", "NAS Delay" = "#e0301e", "Security Delay" = "#a32020", "Weather Delay" = "#602320", "Late Aircraft Delay"= "#eb8c00"))
    })
    
    #Select Data for Reactive Dropdown
    outDeparture = reactive({
      DepartureData <- as.data.frame(unique(DelayedFlights$Origin))
      colnames(DepartureData)[1] <- "Departure"
      DepartureData <- data.frame(DepartureData[order(DepartureData$Departure),])
    })
    observe({
      updateSelectInput(session, "DepartureDestination",
                        choices = outDeparture()[,1]
      )})
    
    outDestination = reactive({
      DestinationData <- as.data.frame(unique(subset(DelayedFlights, Origin==input$DepartureDestination)$Dest))
      colnames(DestinationData)[1] <- "Destination"
      DestinationData <- data.frame(DestinationData[order(DestinationData$Destination),])
    })
    observe({
      updateSelectInput(session, "Destination",
                        choices = outDestination()[,1]
      )})
    
    #Prepare Data for Flights Data Table
    FlightReport <- reactive({
      DelayedFlights["Duration"] <- (DelayedFlights$TaxiIn+DelayedFlights$TaxiOut+DelayedFlights$AirTime)
      AirFlightDataComp <- subset(DelayedFlights, Origin ==input$DepartureDestination & Dest==input$Destination, select = c(UniqueCarrier, AirTime, TaxiIn, TaxiOut, Total_Delay, Duration))
      AirFlightDataMean <- aggregate(cbind(TaxiOut, AirTime, TaxiIn, Total_Delay, Duration)~UniqueCarrier, data = AirFlightDataComp, FUN = function(x) c(mean=format(round(mean(x),0),nsmall = 0)))
      AirFlightDataSD <- aggregate(cbind(TaxiOut, AirTime, TaxiIn, Total_Delay, Duration)~UniqueCarrier, data = AirFlightDataComp, FUN = function(x) c(sd=format(round(qnorm(0.95)*sd(x)/sqrt(length(x)),0),nsmall = 0)))
      TaxiIn <- paste(AirFlightDataMean$TaxiIn, "+/-",AirFlightDataSD$TaxiIn)
      AirTime <- paste(AirFlightDataMean$AirTime, "+/-",AirFlightDataSD$AirTime)
      TaxiOut <- paste(AirFlightDataMean$TaxiOut, "+/-",AirFlightDataSD$TaxiOut)
      Duration <- paste(AirFlightDataMean$Duration, "+/-",AirFlightDataSD$Duration)
      TotalDelay <- paste(AirFlightDataMean$Total_Delay, "+/-",AirFlightDataSD$Total_Delay)
      FlightReport <- data.frame(AirFlightDataMean$UniqueCarrier,TaxiOut,AirTime, TaxiIn ,Duration,TotalDelay )
      colnames(FlightReport)[1] <- "Airline"
      FlightReport})
    
    output$table <- renderTable({FlightReport()})
    
    #Prepare data for Flight Data Plots
    AirFlightDataCompPlot <- reactive({
      DelayedFlights["Duration"] <- (DelayedFlights$TaxiIn+DelayedFlights$TaxiOut+DelayedFlights$AirTime)
      AirFlightDataComp <- subset(DelayedFlights, Origin ==input$DepartureDestination & Dest==input$Destination, select = c(UniqueCarrier, AirTime, TaxiIn, TaxiOut, Total_Delay, Duration))
      AirFlightDataComp$Total_Delay <- AirFlightDataComp$Total_Delay*60
      AirFlightDataComp
    })
    
    output$DurationPlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= Duration)) + geom_density(aes(fill=UniqueCarrier),alpha = 0.2)+ scale_fill_brewer(name = "Airlines", palette="Blues")})
    output$TaxiInPlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= TaxiIn))+ geom_density(aes(fill=UniqueCarrier),alpha = 0.2)+ theme(legend.position = "none")+scale_fill_brewer(name = "Airlines", palette="Blues")})
    output$AirTimePlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= AirTime))+ geom_density(aes(fill=UniqueCarrier),alpha = 0.2)+ theme(legend.position = "none")+scale_fill_brewer(name = "Airlines", palette="Blues")})
    output$TaxiOutPlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= TaxiOut))+ geom_density(aes(fill=UniqueCarrier),alpha = 0.2)+ theme(legend.position = "none")+scale_fill_brewer(name = "Airlines", palette="Blues")})
    
    
    #Airport Selection List third tab
    AirportSelect = reactive({
      AirportSelectData <- as.data.frame(unique(DelayedFlights$Origin))
      AirportSelectData <- data.frame(AirportSelectData[order(unique(DelayedFlights$Origin)),])
    })
    observe({
      updateSelectInput(session, "AirportSelection",
                        choices = AirportSelect()[,1]
      )})
    
    #Number of flight to an Airport
    NumberFlight <- reactive({
      NumberFlightCount <- nrow(DelayedFlights[DelayedFlights$Origin==input$AirportSelection,])
    })
    
    output$NumberFlightText <- renderText(paste("The number of flights per year:",NumberFlight()))
    
    #Number of Airlines to an Airport 
    NumberAirlines <- reactive({
       NumberAirlinesCount <- length(unique(DelayedFlights[DelayedFlights$Origin==input$AirportSelection,]$UniqueCarrier))
    })
    
    output$NumberAirlinesText <- renderText(paste("The Number of Airlines flying to this airport:",NumberAirlines()))
    
    
    #Arrival and Departure Pattern Data
        DepartureInterval <- reactive({
           DepartureInterval <- subset(DelayedFlights, Origin==input$AirportSelection & DepTime >input$timeslider[1] & DepTime<input$timeslider[2])
    })
    ArrivalInterval <- reactive({
      ArrivalIntervalData <- subset(DelayedFlights, Origin==input$AirportSelection & ArrTime >input$timeslider[1] & ArrTime<input$timeslider[2])
    })

    #Arrival and Departure Plots
    output$ArrivalIntervalPlot <- renderPlot({ggplot(data=ArrivalInterval(), aes(x= ArrTime))+geom_histogram(aes(fill=UniqueCarrier),position="stack")+ggtitle("Arrival Distribution")+xlab("Hour")+scale_fill_brewer(palette="Blues")+theme(legend.position = "none")})               
    output$DepartureIntervalPlot <- renderPlot({ggplot(data=DepartureInterval(), aes(x= DepTime))+geom_histogram(aes(fill=UniqueCarrier),position="stack")+ggtitle("Departure Distribution")+xlab("Hour")+scale_fill_brewer(palette="Blues")+theme(legend.position = "none")})    
    
    #DelayedFlights Data 
    DelayedFlightsData <- reactive({
      DelayedFlightsDataSet <- subset(DelayedFlights, Origin==input$AirportSelection & DelayBoolean=="Delayed")
    })
    
    #DelayedFlights Plot
    output$DelayedFlightsPlot <- renderPlot({ggplot(data=DelayedFlightsData(), aes(x=DepTime))+geom_histogram(aes(fill=UniqueCarrier),position="stack", bins=24)+ggtitle("Delay Fequency")+scale_fill_brewer(palette="Blues")+xlab("Hours")+ylab("Number of Delays")})
    
    #Progress Bar Update       
    setProgress(1)
    withProgress(message = "Finished Loading App, Data will be presented soon", style = "notification", value = NULL, {Sys.sleep(2)})
    })
  
})
