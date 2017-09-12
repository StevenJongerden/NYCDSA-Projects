##########################################################################################################################################
### Capstone Project for the NYC Data Science Academy Cohort 10, submitted on 12 September 2017.                                       ###
### Authors: Steven Jongerden, Huanghaotian Fu.                                                                                        ###         
##########################################################################################################################################

##########################################################################################################################################
############################################################ Libraries and Setup #########################################################
library(shiny)
#devtools::install_github("nstrayer/shinyswipr")
library(shinyswipr)
library(shinythemes)
##########################################################################################################################################

##########################################################################################################################################
################################################################# Shiny ui ###############################################################
shinyUI(
  fluidPage(theme = shinythemes::shinytheme("superhero"),
            mainPanel(
              
              ################################################## Swiper ##################################################################
              
              fluidRow(
                hr(),
                shinyswiprUI("quote_swiper", column(12, 
                                                    column(8, 
                                                           h2(textOutput("carname")),
                                                           h4(textOutput("counter")),
                                                           textOutput("co2"),
                                                           textOutput("mpg"),
                                                           plotOutput("horsepower", width = "90%", height = "40px"),
                                                           plotOutput("co2Plot", width = "90%", height = "40px"), align = 'left' ),
                                                    column(4, 
                                                           br(),
                                                           actionButton("LikeButton", "Like!"),
                                                           uiOutput("tab"),
                                                           imageOutput("picture")), align = 'right' ),
                                              
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             style = "background-color:#FFFFFF;", 
                             style="color:black")),
              
              ####################################################### White line ##########################################################
              
              fluidRow(column(12,style = "background-color:#FFFFFF;"), 
                       textOutput("brand")),
              
              ####################################################### Search querry #######################################################
              
              fluidRow(
                column(2),
                column(6,
                       textInput("text", label = "", value = "Enter your recommend querry like: 'a 200 horsepower hatchback with airconditioning' ", width = '740px'), align = "right"
                ),
                column(2,
                       br(),
                       actionButton("buttionclick", "Recommend"), align = "left"
                       ),
                column(2)
              ),
              
              #############################################################################################################################
              
              fluidRow(
                br(),
                column(12,
                       column(3),
                       column(8,
                              
                              ############################################## Car Recommender ##############################################
                              
                              column(4,
                              helpText(h4("Car Recommendation")),
                              # selectInput("PriceRange",
                              #             label = "Price Range",
                              #             choices = c("Price 5000 - 10000","Price 10000 - 15000","Price 15000 - 20000",
                              #                         "Price 20000 - 25000","Price 25000 - 30000","Price 35000+")),
                              selectInput("Horsepower",
                                          label = "Horsepower",
                                          choices = c("50 - 75 bph","75 - 100 bph","100 - 125 bph","125 - 150 bph",
                                                      "150 - 175 bph","175 - 200 bph")),
                              selectInput("CarType",
                                          label = "Car Type",
                                          choices = c("Convertible", "Coupe", "Estate", "Hatchback", "MPV", "Saloon", "SUV", "Other")),
                              selectInput("Transmission",
                                          label = "Type of Transmission",
                                          choices = c("Automatic", "Manual")),
                              selectInput("Fuel",
                                          label = "Type of Fuel",
                                          choices = c("Petrol", "Diesel", "Electric")),
                              selectInput("Drivetrain",
                                          label = "Type of drivetrain",
                                          choices = c("Front wheel drive", "Rear wheel drive", "Four wheel drive"))
                              ),
                              
                              ############################################## Car Specifications ###########################################
                              
                              column(3,
                                     helpText(h4("Car Specifications")),
                                     selectInput("Year",
                                                 label = "Construction Year",
                                                 choices = c(2015, 2016, 2017)),
                                     sliderInput("Miles", label = "Miles driven", min = 0, 
                                                 max = 10000, value = 50), style="color:white"),
                              
                              ################################################ Car Options ################################################
                              
                              column(4,
                                     helpText(h4("Car Options")),
                                     checkboxInput("Navigation", label = "Navigation", value = FALSE),
                                     checkboxInput("ParkingSensors", label = "Parking Sensors", value = FALSE),
                                     checkboxInput("PowerSteering", label = "Power Steering", value = FALSE),
                                     checkboxInput("ReversingCamera", label = "Reversing Camera", value = FALSE),
                                     checkboxInput("TCS", label = "Traction Control System", value = FALSE),
                                     checkboxInput("AirConditioning", label = "Air Conditioning", value = FALSE),
                                     checkboxInput("TyrePressureMonitoringSystem", label = "Tyre Pressure Monitoring System", value = FALSE),
                                     checkboxInput("CruiseControl", label = "Cruise Control", value = FALSE)
                                     )),
                       column(2))
              ), width = "100%")
            
  )
)
###########################################################################################################################################