library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinythemes::shinytheme("flatly"),
                  navbarPage(
                    title = 'House Price Estimation', 
                      tabPanel('Determine the Price of your House',
                             titlePanel("What is the selling price of your house?"),
                             sidebarLayout(
                               sidebarPanel(
                                 helpText("TWhat type of house do you have?"),
                                 div(fluidRow(
                               column(6,
                                 selectInput("SoortWoning", label = h6("Type of the House"), 
                                             choices = list("Two under one roof" = "2-onder-1-kapwoning", 
                                                            "Bungalow" = "Bungalow",
                                                            "One family corner home" = "Eengezinswoning, hoekwoning",
                                                            "One family between home" = "Eengezinswoning, tussenwoning",
                                                            "One familiy free standing home" = "Eengezinswoning, vrijstaande woning",
                                                            "Mansion" = "Herenhuis",
                                                            "Country house" = "Landhuis",
                                                            "Caravan" =  "Stacaravan, vrijstaande woning",
                                                            "Villa",
                                                            "Farm house " = "Woonboerderij",
                                                            "Houseboat" = "Woonboot"), 
                                             selected = "Two under one roof"),
                                 
                                 numericInput("Postcode", label = h6("The zipcode of the House (1012 - 9997)"), value = 1012, min = 1012, max = 9997 ),
                                 
                                 selectInput("Ligging", label = h6("Surrounding of the House"), 
                                             choices = list("Forest edge" = "Bosrand",
                                                            "Forest environment" = "Bosrijke omgeving",
                                                            "Open Area" = "Open ligging",
                                                            "Rural area" = "Landelijk",
                                                            "Outside of city limits" = "Buiten bebouwde kom",
                                                            "Inside the city center" = "Centrum",
                                                            "In a Residential area" = "Woonwijk",
                                                            "On a quite road" = "Aan rustige weg",
                                                            "On a busy road" = "Drukke weg",
                                                            "In a park" = "Park",
                                                            "On the water" = "Water",
                                                            "Any other surrounding" = "Andere Ligging"), 
                                             selected = "Forest edge"),
                                 
                                 numericInput("Woonoppervlakte", label = h6("The m2 of living space"), value = 50),
                                 
                                 numericInput("Perceeloppervlakte", label = h6("The m2 of ground surrounding the House"), value = 100)),
                                 
                                 column(6, selectInput("Tuin", label = h6("The type of garden"), 
                                             choices = list("Front and back garden" = "Voortuin",
                                                            "Side garden" = "Zijtuin",
                                                            "Garden around the house" = "Tuin rondom",
                                                            "Sun terrace" = "Zonneterras",
                                                            "Patio" = "Patio/atrium",
                                                            "Other type of garden" = "Andere tuin"), 
                                             selected = "Front garden"),
                                 
                                 sliderInput("Woonlagen", label = h6("The number of floors in the House"), min = 0, 
                                             max = 5, value = 1),
                                 
                                 sliderInput("Badkamers", label = h6("The number of bathrooms in the House"), min = 0, 
                                             max = 10, value = 1),
                                 
                                 checkboxInput("Isolatie", label = h6("Is your House fully isolated?"), value = TRUE),
                                 
                                  selectInput("Parkeerplaats", label = h6("What are the parking options at your House?"), 
                                            choices = list("No parking space available" = "Geen Parkeerplaats",
                                                           "Public parking" = "Openbaar parkeren",
                                                           "Parking garage" = "Parkeergarage",
                                                           "Parking license required" = "Parkeervergunningen"), 
                                            selected = "Public parking")), style = "font-size:80%"))),
                               
                               mainPanel(
                                 fluidRow(
                                   column(6,
                                          helpText(h4("The price estimation")),
                                          helpText(h6("Added value because of the house type:")),
                                          textOutput("SoortWoning"),
                                          helpText(h6("Added value because of the region:")),
                                          textOutput("Postcode"),
                                          helpText(h6("Added value because of the surrounding area:")),
                                          textOutput("Ligging"),
                                          helpText(h6("Added value because of the house size:")),
                                          textOutput("Woonoppervlakte"),
                                          helpText(h6("Added value because of the land size:")),
                                          textOutput("Perceeloppervlakte"),
                                          helpText(h6("Added value because of the garden:")),
                                          textOutput("Tuin"),
                                          helpText(h6("Added value because of the number of floors:")),
                                          textOutput("Aantal woonlagen"),
                                          helpText(h6("Added value because of the number of bathrooms:")),
                                          textOutput("Aantal badkamers"),
                                          helpText(h6("Added value because of the isolation:")),
                                          textOutput("VolledigIsolatieyes"),
                                          helpText(h6("Added value because of the parkingspace:")),
                                          textOutput("Parkeerplaats"),
                                          br(),
                                          helpText("The estimated asking price for your house:"),
                                          h3(textOutput("price"))),
                                   column(6,
                                          helpText(h4("Current houses in the market")),    
                                          plotOutput("CompHouses"),
                                          plotOutput("NumbHouses")
                                 ))
                             )
                             
                    ))
                
)))
