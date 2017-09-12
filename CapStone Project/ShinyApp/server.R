##########################################################################################################################################
### Capstone Project for the NYC Data Science Academy Cohort 10, submitted on 12 September 2017.                                       ###
### Authors: Steven Jongerden, Huanghaotian Fu.                                                                                        ###         
##########################################################################################################################################

##########################################################################################################################################
############################################################ Libraries and Setup #########################################################
library(shiny)
library(ggplot2)
library(dplyr)
library(arules)
library(recommenderlab)
library(tidyr)
library(stringdist)
library(shinyjs)

#Load in recommendation dataset and the regression models that predict the price, co2 and mpg. 
load("PriceModel.rda")
load("ModelAvgMPG.rda")
load("CO2model.rda")
RecommendData <- read.csv("RecommenderData.csv")
RecommendData <- RecommendData[sample(1:13626, 8000, replace = FALSE),]
RecommendData[1,] <- NA
##########################################################################################################################################

##########################################################################################################################################
############################################################### Shiny Server #############################################################

#Start the shiny server
shinyServer(function(input, output, session) {
  
  ################################################################ SWIPTER ############################################################### 
  
  #Start the swiping application 
  card_swipe <- callModule(shinyswipr, "quote_swiper")
  
  #Observer that changes the car counter based on swiping activity. 
  observeEvent(card_swipe(),{
    Sys.sleep(1)
    count$counter <- if (card_swipe()=="left"){
      if ((count$counter + 1)>numberofrecommendations()){
        count$counter = 1
      } else {
        count$counter + 1
      }
    } else if (card_swipe()=="right") {
      if ((count$counter - 1)< 1){
        count$counter = numberofrecommendations()
      } else {
        count$counter - 1
      }
    } else if (card_swipe()=="up") {
      if ((count$counter + 1)> numberofrecommendations()){
        count$counter = numberofrecommendations()
      } else {
        count$counter + 1
      }
    } else if (card_swipe()=="down") {
      if ((count$counter - 1)< 1){
        count$counter = numberofrecommendations()
      } else {
        count$counter - 1
      }} 
  })
  
  ######################################################### RECOMMENDATION ENGINE ########################################################
  
  #Input for the recommendation engine and create a recommendation based on the user input.
  RecommendDataSet <- reactive({
    #Input the user choices from the shiny front end
    # RecommendData[1,"Price.0.5000"] <- ifelse(input$PriceRange == "Price 0 - 5000",1,0)
    # RecommendData[1,"Price.5000.10000"] <- ifelse(input$PriceRange == "Price 5000 - 10000",1,0)
    # RecommendData[1,"Price.10000.15000"] <- ifelse(input$PriceRange == "Price 10000 - 15000",1,0)
    # RecommendData[1,"Price.15000.20000"] <- ifelse(input$PriceRange == "Price 15000 - 20000",1,0)
    # RecommendData[1,"Price.20000.25000"] <- ifelse(input$PriceRange == "Price 20000 - 25000",1,0)
    # RecommendData[1,"Price.25000.30000"] <- ifelse(input$PriceRange == "Price 25000 - 30000",1,0)
    # RecommendData[1,"Price.30000.35000"] <- ifelse(input$PriceRange == "Price 30000 - 35000",1,0)
    # RecommendData[1,"Price.35000."] <- ifelse(input$PriceRange == "Price 35000+",1,0)
    RecommendData[1,"X25...50.bph"] <- ifelse(input$Horsepower == "25 - 50 bph",1,0)
    RecommendData[1,"X50...75.bph"] <- ifelse(input$Horsepower == "50 - 75 bph",1,0)
    RecommendData[1,"X75...100.bph"] <- ifelse(input$Horsepower == "75 - 100 bph",1,0)
    RecommendData[1,"X100...125.bph"] <- ifelse(input$Horsepower == "100 - 125 bph",1,0)
    RecommendData[1,"X125...150.bph"] <- ifelse(input$Horsepower == "125 - 150 bph",1,0)
    RecommendData[1,"X150...175.bph"] <- ifelse(input$Horsepower == "150 - 175 bph",1,0)
    RecommendData[1,"X175...200.bph"] <- ifelse(input$Horsepower == "175 - 200 bph",1,0)
    RecommendData[1,"CarTypeConvertible"] <- ifelse(input$CarType == "Convertible",1,0)
    RecommendData[1,"CarTypeCoupe"] <- ifelse(input$CarType == "Coupe",1,0)
    RecommendData[1,"CarTypeEstate"] <- ifelse(input$CarType == "Estate",1,0)
    RecommendData[1,"CarTypeHatchback"] <- ifelse(input$CarType == "Hatchback",1,0)
    RecommendData[1,"CarTypeMPV"] <- ifelse(input$CarType == "MPV",1,0)
    RecommendData[1,"CarTypeOthers"] <- ifelse(input$CarType == "Others",1,0)
    RecommendData[1,"CarTypeSaloon"] <- ifelse(input$CarType == "Saloon",1,0)
    RecommendData[1,"CarTypeSUV"] <- ifelse(input$CarType == "SUV",1,0)
    RecommendData[1,"TransmissionAutomatic"] <- ifelse(input$Transmission == "Automatic",1,0)
    RecommendData[1,"TransmissionManual"] <- ifelse(input$Transmission == "Manual",1,0)
    RecommendData[1,"FuelTypeDiesel"] <- ifelse(input$Fuel == "Diesel",1,0)
    RecommendData[1,"FuelTypeElectric.Hybird"] <- ifelse(input$Fuel == "Electric",1,0)
    RecommendData[1,"FuelTypePetrol"] <- ifelse(input$Fuel == "Petrol",1,0)
    RecommendData[1,"DrivetrainFour.Wheel.Drive"] <- ifelse(input$Drivetrain == "Four Wheel Drive",1,0)
    RecommendData[1,"DrivetrainFront.Wheel.Drive"] <- ifelse(input$Drivetrain == "Front Wheel Drive",1,0)
    RecommendData[1,"DrivetrainRear.Wheel.Drive"] <- ifelse(input$Drivetrain == "Rear Wheel Drive",1,0)
    
    #Transform the columns to factors
    for (i in 1:length(colnames(RecommendData))) {
      RecommendData[,i] <- factor(RecommendData[[i]])
    }
    
    #Transform the dataframe to a binaryRatingMatrix
    RecommendationEngine <- as(RecommendData, "transactions") 
    RecommendationEngine <- new("binaryRatingMatrix", data = RecommendationEngine )
    
    #Train the Recommender
    rec <- Recommender(RecommendationEngine, method = "UBCF")
    
    #Recommend fof the first user, which is the case inputed by the used through the front end. 
    pre <- predict(rec, RecommendationEngine[1], n = 100)
    prediction <- data.frame(as(pre, "list"))
    colnames(prediction) <- "Recommendation"
    prediction <- filter(prediction, grepl("RecommendData",prediction[,1]))
    prediction <- separate(prediction, Recommendation, paste0("X",1:2), sep = "=")
    prediction <- cbind(c(1:10), prediction[1:10,])
    
    #Results from the recommendation
    prediction
  })
  
  ############################################################# LINEAR MODELS ############################################################
  
  ModelData <- reactive({
    #Create and empty dataset on which imputation can be performed   
    ModelData <- data.frame(Enginepower = 0, CarType = "Convertible", Transmission = "Automatic", FuelType = "Diesel", Drivetrain = "Four Wheel Drive",
                            Year = 0, Miles = 0, Navigation = "", ParkingSensors = "", PowerSteering = "",
                            ReversingCamera = "", TCS = "", AirConditioning = "", TyrePressureMonitoringSystem = "", CruiseControl = "")
    
    #Transform factors to character classes to allow character imputation
    ModelData[,sapply(ModelData, function(x) is.factor(x))] <- as.character(ModelData[,sapply(ModelData, function(x) is.factor(x))])
    
    #User input from the shiny front end
    ModelData[1,"Enginepower"] <- ifelse(input$Horsepower == "25 - 50 bph", 50, ModelData[1,"Enginepower"])
    ModelData[1,"Enginepower"] <- ifelse(input$Horsepower == "50 - 75 bph", 75, ModelData[1,"Enginepower"])
    ModelData[1,"Enginepower"] <- ifelse(input$Horsepower == "75 - 100 bph", 100, ModelData[1,"Enginepower"])
    ModelData[1,"Enginepower"] <- ifelse(input$Horsepower == "100 - 125 bph", 125, ModelData[1,"Enginepower"])
    ModelData[1,"Enginepower"] <- ifelse(input$Horsepower == "125 - 150 bph", 150, ModelData[1,"Enginepower"])
    ModelData[1,"Enginepower"] <- ifelse(input$Horsepower == "150 - 175 bph", 175, ModelData[1,"Enginepower"])
    ModelData[1,"Enginepower"] <- ifelse(input$Horsepower == "175 - 200 bph", 200, ModelData[1,"Enginepower"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "Convertible", "Convertible", ModelData[1,"CarType"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "Coupe", "Coupe", ModelData[1,"CarType"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "Estate", "Estate", ModelData[1,"CarType"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "Hatchback","Hatchback", ModelData[1,"CarType"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "MPV", "MPV", ModelData[1,"CarType"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "Others","Others", ModelData[1,"CarType"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "Saloon","Saloon", ModelData[1,"CarType"])
    ModelData[1,"CarType"] <- ifelse(input$CarType == "SUV","SUV", ModelData[1,"CarType"])
    ModelData[1,"Transmission"] <- ifelse(input$Transmission == "Automatic","Automatic",  ModelData[1,"Transmission"])
    ModelData[1,"Transmission"] <- ifelse(input$Transmission == "Manual","Manual", ModelData[1,"Transmission"])
    ModelData[1,"FuelType"] <- ifelse(input$Fuel == "Diesel","Diesel", ModelData[1,"FuelType"])
    ModelData[1,"FuelType"] <- ifelse(input$Fuel == "Electric","Electric", ModelData[1,"FuelType"])
    ModelData[1,"FuelType"] <- ifelse(input$Fuel == "Petrol","Petrol", ModelData[1,"FuelType"])
    ModelData[1,"Drivetrain"] <- ifelse(input$Drivetrain == "Four wheel drive", "Four Wheel Drive", ModelData[1,"Drivetrain"])
    ModelData[1,"Drivetrain"] <- ifelse(input$Drivetrain == "Front wheel drive", "Front Wheel Drive", ModelData[1,"Drivetrain"])
    ModelData[1,"Drivetrain"] <- ifelse(input$Drivetrain == "Rear wheel drive", "Rear Wheel Drive", ModelData[1,"Drivetrain"])
    ModelData[1,"Year"] <- input$Year 
    ModelData[1,"Miles"] <- input$Miles
    ModelData[1,"Navigation"] <- ifelse(input$Navigation == 1, "yes", "no")
    ModelData[1,"ParkingSensors"] <- ifelse(input$ParkingSensors == 1, "yes", "no")
    ModelData[1,"PowerSteering"] <- ifelse(input$PowerSteering == 1, "yes", "no")
    ModelData[1,"ReversingCamera"] <- ifelse(input$ReversingCamera == 1, "yes", "no")
    ModelData[1,"TCS"] <- ifelse(input$TCS == 1, "yes", "no")
    ModelData[1,"AirConditioning"] <- ifelse(input$AirConditioning == 1, "yes", "no")
    ModelData[1,"TyrePressureMonitoringSystem"] <- ifelse(input$TyrePressureMonitoringSystem == 1, "no", "yes")
    ModelData[1,"CruiseControl"] <- ifelse(input$CruiseControl == 1, "yes", "no")
    
    #Creating 10 rows to bind with the brand information
    for (i in 1:4){
      ModelData <- rbind(ModelData, ModelData)
    }
    ModelData <- ModelData[1:10,]
    
    #Combine the input from the shiny front end with the brand recommendations
    ModelData <- cbind(RecommendDataSet(), ModelData)
    colnames(ModelData)[3] <- "Brand"
    ModelData[2] <- NULL
    ModelData[1] <- NULL
    
    #Create a new column "Brand" that is the brand name of the car
    carlist <- c("Land Rover", "Mazda", "Audi", "Mercedes-Benz", "MINI", "Nissan", "Citroen", "Peugeot", "Renault",
                 "Skoda", "Ford", "Hyundai", "Toyota", "Vauxhall", "Jaguar", "Volkswagen", "Volvo", "KIA", "Mitsubishi", "BMW", "Alpha Romeo")
    for (i in 1:length(carlist)){
      ModelData$Brand <- ifelse(grepl(carlist[i], ModelData$Brand), carlist[i], ModelData$Brand)
    }
    
    ModelData$Brand <- ifelse(grepl("MAZDA3", ModelData$Brand), "Mazda", ModelData$Brand)
    ModelData$Brand <- ifelse(grepl("MAZDA6", ModelData$Brand), "Mazda", ModelData$Brand)
    ModelData$Brand <- ifelse(grepl("MAZDA2", ModelData$Brand), "Mazda", ModelData$Brand)
    ModelData$Brand <- ifelse(grepl("MAZDA 2", ModelData$Brand), "Mazda", ModelData$Brand)
    ModelData$Brand <- ifelse(grepl("Mitsubishi", ModelData$Brand), "Mitsubishi", ModelData$Brand)
    ModelData$Brand <- factor(ModelData$Brand)
    
    #Set the Year to nummeric 
    ModelData$Year <- as.numeric(ModelData$Year)
    
    #Predict Car Price
    CarPrice <- round(predict(ManualAdjustedLinear, newdata=ModelData)/100,0)*100
    ModelData <- cbind(ModelData, CarPrice)
    
    #Predict the Co2 and MPG of the car 
    Co2 <- predict(CO2model, newdata=ModelData)
    MPG <- predict(ModelAvgMPG, newdata=ModelData)
    ModelData <- cbind(ModelData, Co2, MPG)
    
    #Present the data. 
    ModelData
  })
  
  #Set the number of recommendations
  numberofrecommendations <- reactive({sum(!is.na(RecommendDataSet()[,2]))})
  
  ################################################################ PLOTS ################################################################
  
  #Generate the ggplots 
  #Change to price 
  output$horsepower <- renderPlot({
    ggplot(ModelData()[count$counter,], aes(x = Brand, y=MPG)) + 
      geom_bar(stat="identity", fill="#2b3e50") + coord_flip() + xlab("MPG") + ylim(c(0,80)) +
      theme(aspect.ratio=0.03, axis.title.x = element_blank(),
            legend.position="none", 
            axis.title.y =  element_text(angle = 0, vjust = 0.65),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill="white"))
  }, height = 40, width = 450)
  
  output$co2Plot <- renderPlot({
    ggplot(ModelData()[count$counter,], aes(x = Brand, y=Co2)) + 
      geom_bar(stat="identity", fill="#2b3e50") + coord_flip() + xlab("Co2") + ylim(c(0,160)) +
      theme(aspect.ratio=0.03, axis.title.x = element_blank(),
            legend.position="none", 
            axis.title.y =  element_text(angle = 0, vjust = 0.65),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill="white"))
  }, height = 40, width = 450)
  
  ############################################################### PICTURE ##############################################################
  
  #Generate the pictures
  output$picture <- renderImage({
    list(src = paste("CarPictures/",RecommendDataSet()[count$counter,3],".jpg", sep = ""), width = 250)
  }, deleteFile = FALSE)
  
  ############################################################# TEXT OUTPUT ############################################################
  
  #Set the car counter
  count <- reactiveValues(counter = 1)
  
  #Set the text in the swiper
  output$carname <- renderText({RecommendDataSet()[count$counter,3]})
  output$cartype <- renderText({input$CarType})
  output$counter <- renderText({paste("$ ",substr(as.character(round(ModelData()[count$counter, "CarPrice"], 0)),0,2),",",
                                      substr(as.character(round(ModelData()[count$counter, "CarPrice"], 0)),3,10), sep = "")})
  output$co2 <- renderText({paste("Co2 Emission:",round(ModelData()[count$counter, "Co2"], 2), "g/km")})
  output$mpg <- renderText({paste("Fuel Consumption: ",round(ModelData()[count$counter, "MPG"],2), "mpg")})
  
  ########################################################## TEXT RECOMMENDER #########################################################
  
  SelectOptionsTable <- reactive({
    SearchString <- unlist(strsplit(tolower(as.character(gsub("  ", " ", (input$text)))), " "))
    SearchStringLength <- length(unlist(strsplit(tolower(as.character(SearchString)), " ")))
    if (SearchStringLength >0){ 
      Options <- data.frame(Options = c('airconditioning',"conditioning", "navigation", "parking sensors", "parking",
                                        "power steering", "steering", "reverse camera", "camera", "TCS", "traction",
                                        "pressure", "cruisecontrol", "cruise", "horsepower", "convertible",
                                        "coupe", "estate", "hatchback", "mpv", "saloon", "suv", "automatic", "manual",
                                        "diesel", "petrol", "electric", "four", "front","rear"))
      
      for (i in 1:SearchStringLength){
        Options[,SearchString[i]] <- unlist(sapply(Options[,1], Map, f=stringdist, SearchString[i], method="cosine"))
      }
      Options$min <- apply(Options[2:(SearchStringLength+1)], 1, min)
      Options$present <- ifelse(Options$min < 0.05, "yes", "no")
      SelectOptions <-select(Options, Options, present, min)
      if (SelectOptions[15,2] == "yes"){
        SelectOptions[15,2] <- regmatches(input$text , gregexpr('[0-9]+',input$text))
      }} else {
        Options <- data.frame(Options = c('airconditioning',"conditioning", "navigation", "parking sensors", "parking",
                                          "power steering", "steering", "reverse camera", "camera", "TCS", "traction",
                                          "pressure", "cruisecontrol", "cruise", "horsepower", "convertible",
                                          "coupe", "estate", "hatchback", "mpv", "saloon", "suv", "automatic", "manual",
                                          "diesel", "petrol", "electric", "four", "front","rear"))
        
        SelectOptions <- cbind(Options, replicate(nrow(Options), "no"))
      }
    SelectOptions
  })
  
  #Update the selection based on the text recommender
  observeEvent(input$buttionclick, {
      updateCheckboxInput(session, "AirConditioning", label = "Air Conditioning", value = SelectOptionsTable()[1,2]=="yes")
      updateCheckboxInput(session, "Navigation", label = "Navigation", value = SelectOptionsTable()[3,2]=="yes")
      updateCheckboxInput(session, "ParkingSensors", label = "Parking Sensors", value = SelectOptionsTable()[4,2]=="yes")
      updateCheckboxInput(session, "ParkingSensors", label = "Parking Sensors", value = SelectOptionsTable()[5,2]=="yes")
      updateCheckboxInput(session, "PowerSteering", label = "Power Steering", value = SelectOptionsTable()[6,2]=="yes")
      updateCheckboxInput(session, "PowerSteering", label = "Power Steering", value = SelectOptionsTable()[7,2]=="yes")
      updateCheckboxInput(session, "ReversingCamera", label = "Reversing Camera", value = SelectOptionsTable()[8,2]=="yes")
      updateCheckboxInput(session, "ReversingCamera", label = "Reversing Camera", value = SelectOptionsTable()[9,2]=="yes")
      updateCheckboxInput(session, "TCS", label = "Traction Control System", value = SelectOptionsTable()[10,2]=="yes")
      updateCheckboxInput(session, "TCS", label = "Traction Control System", value = SelectOptionsTable()[11,2]=="yes")
      updateCheckboxInput(session, "TyrePressureMonitoringSystem", label = "Tyre Pressure Monitoring System", value = SelectOptionsTable()[12,2]=="yes")
      updateCheckboxInput(session, "CruiseControl", label = "Cruise Control", value = SelectOptionsTable()[13,2]=="yes")
      updateCheckboxInput(session, "CruiseControl", label = "Cruise Control", value = SelectOptionsTable()[14,2]=="yes")

      if (SelectOptionsTable()[15,2]!="no"){
        if (as.numeric(SelectOptionsTable()[15,2]) >= 50 & as.numeric(SelectOptionsTable()[15,2]) <= 75){
          print(50)
          updateSelectInput(session, "Horsepower", label = "Horsepower", choices = c("50 - 75 bph","75 - 100 bph","100 - 125 bph","125 - 150 bph",
                                                                                     "150 - 175 bph","175 - 200 bph"), selected = "50 - 75 bph")}
        if (as.numeric(SelectOptionsTable()[15,2]) >= 75 & as.numeric(SelectOptionsTable()[15,2]) <= 100){
          print(75)
          updateSelectInput(session, "Horsepower", label = "Horsepower", choices = c("50 - 75 bph","75 - 100 bph","100 - 125 bph","125 - 150 bph",
                                                                                     "150 - 175 bph","175 - 200 bph"), selected = "75 - 100 bph")}
        if (as.numeric(SelectOptionsTable()[15,2]) >= 100 & as.numeric(SelectOptionsTable()[15,2]) <= 125){
          updateSelectInput(session, "Horsepower", label = "Horsepower", choices = c("50 - 75 bph","75 - 100 bph","100 - 125 bph","125 - 150 bph",
                                                                                     "150 - 175 bph","175 - 200 bph"), selected = "100 - 125 bph")}
        if (as.numeric(SelectOptionsTable()[15,2]) >= 125 & as.numeric(SelectOptionsTable()[15,2]) <= 150){
          updateSelectInput(session, "Horsepower", label = "Horsepower", choices = c("50 - 75 bph","75 - 100 bph","100 - 125 bph","125 - 150 bph",
                                                                                     "150 - 175 bph","175 - 200 bph"), selected = "125 - 150 bph")}
        if (as.numeric(SelectOptionsTable()[15,2]) >= 150 & as.numeric(SelectOptionsTable()[15,2]) <= 175){
          updateSelectInput(session, "Horsepower", label = "Horsepower", choices = c("50 - 75 bph","75 - 100 bph","100 - 125 bph","125 - 150 bph",
                                                                                     "150 - 175 bph","175 - 200 bph"), selected = "150 - 175 bph")}
        if (as.numeric(SelectOptionsTable()[15,2]) >= 175){
          updateSelectInput(session, "Horsepower", label = "Horsepower", choices = c("50 - 75 bph","75 - 100 bph","100 - 125 bph","125 - 150 bph",
                                                                                     "150 - 175 bph","175 - 200 bph"), selected = "175 - 200 bph")}
        }
      if (SelectOptionsTable()[16,2] == "yes"){
        updateSelectInput(session, "CarType", label = "Car Type", choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other"), selected = "Convertible")}
      if (SelectOptionsTable()[17,2] == "yes"){
        updateSelectInput(session, "CarType", label = "Car Type", choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other"), selected = "Coupe")}
      if (SelectOptionsTable()[18,2] == "yes"){
        updateSelectInput(session, "CarType", label = "Car Type", choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other"), selected = "Estate")}
      if (SelectOptionsTable()[19,2] == "yes"){
        updateSelectInput(session, "CarType", label = "Car Type", choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other"), selected = "Hatchback")}
      if (SelectOptionsTable()[20,2] == "yes"){
        updateSelectInput(session, "CarType", label = "Car Type", choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other"), selected = "MPV")}
      if (SelectOptionsTable()[21,2] == "yes"){
        updateSelectInput(session, "CarType", label = "Car Type", choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other"), selected = "Saloon")}
      if (SelectOptionsTable()[22,2] == "yes"){
        updateSelectInput(session, "CarType", label = "Car Type", choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other"), selected = "SUV")}
      if (SelectOptionsTable()[23,2] == "yes"){
        updateSelectInput(session, "Transmission", label = "Type of Transmission", choices = c("Automatic", "Manual"), selected = "Automatic")}
      if (SelectOptionsTable()[24,2] == "yes"){
        updateSelectInput(session, "Transmission", label = "Type of Transmission", choices = c("Automatic", "Manual"), selected = "Manual")}
      if (SelectOptionsTable()[25,2] == "yes"){
        updateSelectInput(session, "Fuel",label = "Type of Fuel",choices = c("Petrol", "Diesel", "Electric"), selected = "Diesel")}
      if (SelectOptionsTable()[26,2] == "yes"){
        updateSelectInput(session, "Fuel",label = "Type of Fuel",choices = c("Petrol", "Diesel", "Electric"), selected = "Petrol")}
      if (SelectOptionsTable()[27,2] == "yes"){
        updateSelectInput(session, "Fuel",label = "Type of Fuel",choices = c("Petrol", "Diesel", "Electric"), selected = "Electric")}
      if (SelectOptionsTable()[28,2] == "yes"){
        updateSelectInput(session, "Drivetrain", label = "Type of drivetrain", choices = c("Front wheel drive", "Rear wheel drive", "Four wheel drive"), selected = "Four wheel drive")}
      if (SelectOptionsTable()[29,2] == "yes"){
        updateSelectInput(session, "Drivetrain", label = "Type of drivetrain", choices = c("Front wheel drive", "Rear wheel drive", "Four wheel drive"), selected = "Front wheel drive")}
      if (SelectOptionsTable()[30,2] == "yes"){
        updateSelectInput(session, "Drivetrain", label = "Type of drivetrain", choices = c("Front wheel drive", "Rear wheel drive", "Four wheel drive"), selected = "Rear wheel drive")}
      
  })
  
  observeEvent(input$LikeButton, {
    if(!file.exists('Recommended.csv')){write.csv(rbind(c('ac', 'conditioning', 'navigation', 'parking senser', 'parking',
                                                   'power steering', 'steering', 'reverse camera', 'camera', 'TCS',
                                                   'traction', 'pressure', 'cruisecontrol', 'cruise', 'horsepower',
                                                   'convertible', 'coupe', 'estate', 'hatchback', 'mpv', 'saloon',
                                                   'suv', 'automatic', 'manual', 'diesel', 'petrol', 'electric',
                                                   'four', 'front', 'rear', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', "ShownCar"), 
                                                   cbind(t(SelectOptionsTable()[,2]), t(RecommendDataSet()[,3]), count$counter)), 'Recommended.csv')} else {
                                                   append1 = read.csv('C:/Users/Steven Jongerden/Desktop/CapStone Project/ShinyApp/Recommended.csv')
                                                   append1 = rbind(append1[2:42], cbind(t(SelectOptionsTable()[,2]), t(RecommendDataSet()[,3]), count$counter))
                                                   write.csv(append1, 'Recommended.csv')
                                                 }})
  
})


##########################################################################################################################################

