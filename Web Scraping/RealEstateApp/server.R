library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {
  
  FinalModelResults <- read.table("FinalModel2.txt", sep = " ")
  HousesInfo <- read.csv('RHouseInformation.csv',  sep = ";")
  
  ModelResults <- reactive({
    Variable <-c("SoortWoning","Postcode","Ligging","Woonoppervlakte","Perceeloppervlakte",
                 "Tuin","Aantal woonlagen", "Aantal badkamers","VolledigIsolatieyes","Parkeerplaats" )
    VariableSelected <-c("a","b","c","d","e","f","g","h","i","j")
    SelectionTable <-data.frame(Variable,VariableSelected )
    SelectionTable$VariableSelected <- as.character(SelectionTable$VariableSelected)
    SelectionTable$Variable <- as.character(SelectionTable$Variable)
    SelectionTable[1,2] <- input$SoortWoning
    SelectionTable[2,2] <- input$Postcode
    SelectionTable[3,2] <- input$Ligging
    SelectionTable[4,2] <- input$Woonoppervlakte
    SelectionTable[5,2] <- input$Perceeloppervlakte
    SelectionTable[6,2] <- input$Tuin
    SelectionTable[7,2] <- input$Woonlagen
    SelectionTable[8,2] <- input$Badkamers
    SelectionTable[9,2] <- input$Isolatie
    SelectionTable[10,2] <- input$Parkeerplaats
    SelectionTable$VariableMatch <- c(SelectionTable[1,2], SelectionTable[2,2], SelectionTable[3,2], SelectionTable[4,1],
                                      SelectionTable[5,1], SelectionTable[6,2], SelectionTable[7,1], SelectionTable[8,1],
                                      SelectionTable[9,1], SelectionTable[10,2])
    RegressionTable <-left_join(SelectionTable, FinalModelResults, by = "VariableMatch")
    RegressionTable$LowerCalc <- c(RegressionTable[1,4], RegressionTable[2,4], RegressionTable[3,4], RegressionTable[4,4]*as.numeric(RegressionTable[4,2]),
                                   RegressionTable[5,4]*as.numeric(RegressionTable[5,2]), RegressionTable[6,4], RegressionTable[7,4]*as.numeric(RegressionTable[7,2]),
                                   RegressionTable[8,4]*as.numeric(RegressionTable[8,2]), as.logical(RegressionTable[9,2])*as.numeric(RegressionTable[9,4]), RegressionTable[10,4])
    RegressionTable$EstimateCalc <- c(RegressionTable[1,5], RegressionTable[2,5], RegressionTable[3,5], RegressionTable[4,5]*as.numeric(RegressionTable[4,2]),
                                      RegressionTable[5,5]*as.numeric(RegressionTable[5,2]), RegressionTable[6,5], RegressionTable[7,5]*as.numeric(RegressionTable[7,2]),
                                      RegressionTable[8,5]*as.numeric(RegressionTable[8,2]), as.logical(RegressionTable[9,2])*as.numeric(RegressionTable[9,5]), RegressionTable[10,5])
    RegressionTable$UpperCalc <- c(RegressionTable[1,6], RegressionTable[2,6], RegressionTable[3,6], RegressionTable[4,6]*as.numeric(RegressionTable[4,2]),
                                   RegressionTable[5,6]*as.numeric(RegressionTable[5,2]), RegressionTable[6,6], RegressionTable[7,6]*as.numeric(RegressionTable[7,2]),
                                   RegressionTable[8,6]*as.numeric(RegressionTable[8,2]), as.logical(RegressionTable[9,2])*as.numeric(RegressionTable[9,6]), RegressionTable[10,6])
    RegressionTable
  })
  
  format(round(as.numeric(1000.64), 1), nsmall=1, big.mark=",")
  
  output$price <- renderText(paste(format(round(as.numeric(sum(ModelResults()$EstimateCalc))), nsmall=0, big.mark=","), " Euro"))
  output$SoortWoning <- renderText(paste(format(round(as.numeric(ModelResults()[1,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$Postcode <- renderText(paste(format(round(as.numeric(ModelResults()[2,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$Ligging <- renderText(paste(format(round(as.numeric(ModelResults()[3,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$Woonoppervlakte <- renderText(paste(format(round(as.numeric(ModelResults()[4,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$Perceeloppervlakte <- renderText(paste(format(round(as.numeric(ModelResults()[5,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$Tuin <- renderText(paste(format(round(as.numeric(ModelResults()[6,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$`Aantal woonlagen` <- renderText(paste(format(round(as.numeric(ModelResults()[7,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$`Aantal badkamers` <- renderText(paste(format(round(as.numeric(ModelResults()[8,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$VolledigIsolatieyes <- renderText(paste(format(round(as.numeric(ModelResults()[9,8]), 0), nsmall=0, big.mark=","), " Euro"))
  output$Parkeerplaats <- renderText(paste(format(round(as.numeric(ModelResults()[10,8]), 0), nsmall=0, big.mark=","), " Euro"))
  
  output$CompHouses <- renderPlot({
    selectedhouses <- subset(HousesInfo, Adress == input$Postcode)
    plot = ggplot(selectedhouses, aes(x=reorder(Soort.woonhuis, Vraagprijs , mean), fill = Soort.woonhuis))+geom_bar() + 
      scale_fill_brewer(palette="Blues") +  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.background = element_rect(fill="white"), legend.position="none") +
      xlab("Type of house") + ylab("Number of houses for sale")
    plot
  })
  
  output$NumbHouses <- renderPlot({
    selectedhouses <- subset(HousesInfo, Adress == input$Postcode)
    plot = ggplot(selectedhouses, aes(x=Vraagprijs))+geom_histogram(bins = 10) + 
      scale_fill_brewer(palette="Blues") +  theme(axis.text.x = element_text(angle = 0, hjust = 1), panel.background = element_rect(fill="white")) +
      xlab("Asking price in Euro") + ylab("Number of houses for sale")+ scale_x_continuous(labels = scales::comma)
    plot
  })
})
