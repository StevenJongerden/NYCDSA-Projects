#################################################################################################################################
#Shiny App for Descriptive Statistics and Exploratory Analysis on USA Flights of the period April 2017.                         #
# Version 1,0                                                                                                                   #
# Author: S. Jongerden                                                                                                          #
# Data Set is publicly available on: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time       #
#################################################################################################################################

#Installing required packages if required
#install.packages("ggmap")
#install.packages("ggrepel")
#install.packages("readr")
#install.packages("ggthemes")
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("shinythemes")

#Loading packages for script
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(ggmap)
library(ggrepel)
library(ggthemes)
library(shinythemes)
library(fiftystater)

#Record StartingTime
startTime <-Sys.time()

#Load Data Set and preparing airline names (The data set has been prepared)       
DelayedFlights <- read_csv("On_Time_On_Time_PerformanceFinal2.csv")

#Start of Server Script
shinyServer(function(input, output, session) {
  
    #Prepare Data for Map Plot by allocating coordinates from goecordata file for lonitude and latitude coordinations. 
    #This part of code will first create a cross table between origins and destinations for a specific airline.
    #Based on that table it will create a ranked list of the top destinations and join the coordinates to this list
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
      FlightData
    })
    
    #Change route selection slider range dependent on airline number of routes
    #As the number of destinations changes per airline, the slider on the frist tab needs to change accordingly.
    observe({
      val <- nrow(FlightData())
      updateSliderInput(session, "TopRange", value = 5,
                        min = 1, max = val, step = 1)
    })
    
    #World map plot
    #THe following code creates the plot of the world map. It contains an if statement to control the number of flight labels in the plot.
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
    
    
    #World plot 2
    #Creates an overview of the number of flights per state.
    DataTableDeparting <- reactive({ Departing <-data.frame(filter(DelayedFlights, UniqueCarrier==input$Airline) %>%  group_by(OriginStateName) %>%  
                                      summarise(NumFlights = sum(FlightNum)) %>% arrange(OriginStateName) %>%  mutate(OriginStateName = tolower(OriginStateName)))
                                      })

    
    output$worldplot2 <- renderPlot({ggplot() + geom_map(data=map_data("state"), map=map_data("state"),aes(long, lat, map_id=region), color="#2b2b2b", fill=NA) + 
        geom_map(data=DataTableDeparting(), map=map_data("state"), aes(fill=NumFlights, map_id=OriginStateName), color="white", size=0.15) + 
          theme_map() +  theme(plot.margin=margin(20,20,20,20), legend.position=c(0.9, 0.2), panel.background = element_blank(), panel.border = element_blank()) + coord_fixed(ratio = 1.3/1)})
  
    
    #PieChart Number of Flights
    #The following parts of code creates the data for the piecharts on the first page.
    FlightPercentage <- reactive({data.frame(Airline = c(input$Airline, "Other"),
                                             Percentage = c(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,])/nrow(DelayedFlights),
                                                            1-(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,])/nrow(DelayedFlights))))})
    
    output$piechart1 <- renderPlot({ggplot(FlightPercentage(), aes(x="", y=Percentage, fill=Airline))+
        geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+
        theme_minimal()+geom_label(aes(label = paste(Airline,round(Percentage*100,2),"%")),position = position_stack(vjust = 0.5)) +
        theme(panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5))+ggtitle("Percentage of Total Flights")})
    
    
    FlightDelayed <- reactive({data.frame(Delay = c("Delayed", "Not Delayed"),
                                          Percentage = c(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline & DelayedFlights$Total_Delay>0,])/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                         1-(nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline & DelayedFlights$Total_Delay>0,])/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]))))})
    
    
    
    output$piechart2 <- renderPlot({ggplot(FlightDelayed(), aes(x="", y=Percentage, fill=Delay))+
        geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Blues")+
        theme_minimal()+geom_label(aes(label = paste(Delay,round(Percentage*100,2),"%")), position = position_stack(vjust = 0.5)) +
        theme(panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5))+ggtitle("Percentage of Flights Delayed")})
    
    
    FlightDelayedType <- reactive({FlightDelayed <- data.frame(Delay = c("Carrier Delay","Weather Delay","NAS Delay","Security Delay","Late Aircraft Delay"),
                                                               Percentage = c(nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & CarrierDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                              nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & WeatherDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                              nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & NASDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                              nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & SecurityDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,]),
                                                                              nrow(subset(DelayedFlights, UniqueCarrier==input$Airline & LateAircraftDelay>0))/nrow(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline,])))
    #Correct percentages to weight with the overall number. 
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
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5))+ggtitle("Percentage of Delays per Type")})
    
    #Prepare Data for Performance
    # THe following code creates the information about the airline that is presented in the three blue boxes on the first page,
    NumberAirplane <- reactive({nrow(unique(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline, 12]))})
    NumberFlights <- reactive({nrow(unique(DelayedFlights[DelayedFlights$UniqueCarrier==input$Airline, 11]))})
    NumberDestinations <- reactive({nrow(FlightData())})
    
    output$AirplaneNumbertext <- renderText(paste("Number of airplane operated by:", input$Airline))
    output$FlightsNumbertext <- renderText(paste("Number of routes operated by:", input$Airline))
    output$Destinationstext <- renderText(paste("Number of flight destinations for:", input$Airline))
    output$AirplaneNumberX <- renderText(NumberAirplane())
    output$FlightsNumberX <- renderText(NumberFlights())
    output$DestinationsX <- renderText(NumberDestinations())
    
    #Delay Distribution Plot
    output$DelayTypePlot <- renderPlot({
      ggplot(data=DelayDataPlot(), aes(x = Month, y = Duration/60, fill=Type))+ geom_bar(stat = "Identity", position = "Stack") + ylab("Delay in hours") + 
        scale_x_discrete(limits=c("Month")) + 
        scale_fill_manual("legend", values = c("Carrier Delay" = "#dc6900", "NAS Delay" = "#e0301e", "Security Delay" = "#a32020", "Weather Delay" = "#602320", "Late Aircraft Delay"= "#eb8c00"))
    })
    
    #Select Data for Reactive Dropdown
    #The following code creates the information for the reactive dropdown menus on the second page. The dropdown only shows the information that is 
    # available in the dataset. The second dropdown then adjusts the presented information based on the selection of the first dropdown. 
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
    #The following code creates a dataframe with average flight information with a confidence interval of 95%.
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
    #The following script prepares the data to present dentsity plots about taxi, airtime, taxi out and total flight duration. 
    AirFlightDataCompPlot <- reactive({
      DelayedFlights["Duration"] <- (DelayedFlights$TaxiIn+DelayedFlights$TaxiOut+DelayedFlights$AirTime)
      AirFlightDataComp <- subset(DelayedFlights, Origin ==input$DepartureDestination & Dest==input$Destination, select = c(UniqueCarrier, AirTime, TaxiIn, TaxiOut, Total_Delay, Duration))
      AirFlightDataComp$Total_Delay <- AirFlightDataComp$Total_Delay*60
      AirFlightDataComp
      })
    
    AirlineCompare <- reactive({
      DelayedFlights["Duration"] <- (DelayedFlights$TaxiIn+DelayedFlights$TaxiOut+DelayedFlights$AirTime)
      AirFlightDataComp <- subset(DelayedFlights, Origin ==input$DepartureDestination & Dest==input$Destination, select = c(UniqueCarrier, AirTime, TaxiIn, TaxiOut, Total_Delay, Duration))
      AirFlightDataComp$Total_Delay <- AirFlightDataComp$Total_Delay*60
      AovTest<-aov(AirFlightDataComp$Duration ~ AirFlightDataComp$UniqueCarrier, data = AirFlightDataComp)
      TuckeyTest <- TukeyHSD(x=AovTest, 'AirFlightDataComp$UniqueCarrier', conf.level=0.95)
      TuckeyTest <- as.data.frame(TuckeyTest$`AirFlightDataComp$UniqueCarrier`)
      ResultTable <- data.frame(rownames(TuckeyTest))
      colnames(ResultTable) <- "Flight Duration"
      ResultTable$PValue <- TuckeyTest$`p adj`
      ResultTable$Direction <- TuckeyTest$diff
      ResultTable$`Statistical Difference` <- ifelse(ResultTable$PValue <0.05, "Different", "Not Different")
      ResultTable$Duration <- ifelse(ResultTable$Direction <0, "Shorter", ifelse(ResultTable$`Statistical Difference` == "Not Different", "The Same", "Longer"))
      ResultTable$Duration <- ifelse(ResultTable$`Statistical Difference` == "Not Different", "The Same", ResultTable$Duration)
      ResultTable$PValue <- NULL
      ResultTable$Direction <- NULL
      ResultTable
    })
    
    output$tabledif <- renderTable({AirlineCompare()})
    
    output$DurationPlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= Duration)) + geom_density(aes(fill=UniqueCarrier),alpha = 0.2) + 
        theme(panel.background = element_rect(fill="white")) + scale_fill_brewer(name = "Airlines", palette="Blues") + xlab("Duration (min)")})
    
    output$TaxiInPlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= TaxiIn))+ geom_density(aes(fill=UniqueCarrier),alpha = 0.2) + 
        theme(legend.position = "none", panel.background = element_rect(fill="white")) + scale_fill_brewer(name = "Airlines", palette="Blues") + xlab("Duration (min)")})
    
    output$AirTimePlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= AirTime))+ geom_density(aes(fill=UniqueCarrier),alpha = 0.2) + 
        theme(legend.position = "none", panel.background = element_rect(fill="white")) + scale_fill_brewer(name = "Airlines", palette="Blues") + xlab("Duration (min)")})
    
    output$TaxiOutPlot <- renderPlot({ggplot(data=AirFlightDataCompPlot(), aes(x= TaxiOut)) + geom_density(aes(fill=UniqueCarrier),alpha = 0.2)+ 
        theme(legend.position = "none", panel.background = element_rect(fill="white")) + scale_fill_brewer(name = "Airlines", palette="Blues") + xlab("Duration (min)")})
    
    #Airport Selection List third tab
    #The following script creates information for the dropdown so that the destinations match with the information that is in the dataset. 
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
      DepartureInterval <- subset(DelayedFlights, Origin==input$AirportSelection & DepTime >=input$timeslider[1] & DepTime<=input$timeslider[2])
    })
    ArrivalInterval <- reactive({
      ArrivalIntervalData <- subset(DelayedFlights, Origin==input$AirportSelection & ArrTime >=input$timeslider[1] & ArrTime<=input$timeslider[2])
    })
    
    #Arrival and Departure Plots
    output$ArrivalIntervalPlot <- renderPlot({ggplot(data=ArrivalInterval(), aes(x= ArrTime))+geom_histogram(aes(fill=UniqueCarrier),position="stack", bins=48)+ggtitle("Arrival Distribution")+
        xlab("Hour")+scale_fill_brewer(palette="Blues")+xlim(c(input$timeslider[1],input$timeslider[2])) + theme(legend.position = "none")+ ylab("Number of flights arriving")})               
    output$DepartureIntervalPlot <- renderPlot({ggplot(data=DepartureInterval(), aes(x= DepTime))+geom_histogram(aes(fill=UniqueCarrier),position="stack", bins=48)+ggtitle("Departure Distribution")+
        xlab("Hour")+scale_fill_brewer(palette="Blues")+xlim(c(input$timeslider[1],input$timeslider[2])) + theme(legend.position = "none")+ylab("Number of flights departing")})    
    
    #DelayedFlights Data 
    DelayedFlightsData <- reactive({
      DelayedFlightsDataSet <- subset(DelayedFlights, Origin==input$AirportSelection & DelayBoolean=="Delayed" & ArrTime >=input$timeslider[1] & ArrTime<=input$timeslider[2])
    })
    
    #DelayedFlights Plot
    output$DelayedFlightsPlot <- renderPlot({ggplot(data=DelayedFlightsData(), aes(x=DepTime))+geom_histogram(aes(fill=UniqueCarrier),position="stack", bins=48)+ggtitle("Delay Fequency")+
        scale_fill_brewer(palette="Blues")+xlab("Hours")+ylab("Number of Delays")+xlim(c(input$timeslider[1],input$timeslider[2]))})
  
  #End of shiny script. 
  
})

endTime <- Sys.time()
print(endTime-startTime)

