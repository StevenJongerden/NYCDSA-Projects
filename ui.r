#################################################################################################################################
#Shiny App for Descriptive Statistics and Exploratory Analysis on USA Flights of the period April 2017.                         #
# Version 1,0                                                                                                                   #
# Author: S. Jongerden                                                                                                          #
# Data Set is publicly available on: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time       #
#################################################################################################################################

shinyUI(fluidPage(theme = shinythemes::shinytheme("flatly"),
                  navbarPage(
                    title = 'USA Airline Performance', 
                    tabPanel('Compare USA Airlines',
                             titlePanel("Performance of Airlines in the USA"),
                             sidebarLayout(
                               sidebarPanel(
                                 helpText("This application provides insights into the performance of several USA airlines in the United States in the period April 2017"),
                                 selectInput("Airline",
                                             label = "Choose the Airline",
                                             choices = c("American Airlines",
                                                         "Alaska Airlines",
                                                         "Jet Blue Airlines",
                                                         "Delta Air Lines",
                                                         "United Airlines",
                                                         "SkyWest  Airlines",
                                                         "Southwest Airlines",
                                                         "Frontier Airlines",
                                                         "Hawaiian Airlines",
                                                         "Atlantic Southeast Airlines",
                                                         "Spirit Airlines"),
                                             selected = "Delta Air Lines"),
                                 sliderInput("TopRange", "Select the top flights", min = 1, max = 10, value = 5),
                                 checkboxInput("MapLabels", "Number of flights lables", value=FALSE)),
                               mainPanel(
                                 fluidRow(
                                   column(3, style = "background-color:#2c3e50;", style = "color:white",
                                          h5(textOutput("AirplaneNumbertext")),
                                          h4(textOutput("AirplaneNumberX"))     
                                   ),
                                   column(1, NULL),
                                   column(3, style = "background-color:#2c3e50;",style = "color:white",
                                          h5(textOutput("FlightsNumbertext")),
                                          h4(textOutput("FlightsNumberX"))      
                                   ),
                                   column(1, NULL),
                                   column(3, style = "background-color:#2c3e50;",style = "color:white",
                                          h5(textOutput("Destinationstext")),
                                          h4(textOutput("DestinationsX"))      
                                   ),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   plotOutput("worldplot")
                                 ),
                                 fluidRow(column(12, align="center",
                                                 column(1, NULL),
                                                 column(3,
                                                        plotOutput("piechart1")
                                                 ),
                                                 column(3,
                                                        plotOutput("piechart2")
                                                 ),
                                                 column(3,
                                                        plotOutput("piechart3")
                                                 )
                                 ) 
                                 )
                               )
                             )
                             
                    ),
                    tabPanel('USA Airlines Route Performance', 
                             titlePanel("Comparing Airline Performance per Route"),
                             sidebarLayout(
                               sidebarPanel(
                                 helpText("This application provides insights into the performance of USA airlines on specific routes"),
                                 selectInput('DepartureDestination', 'City of Origin', ""),
                                 selectInput('Destination', 'City of Destination', ""),
                                 br(),
                                 helpText(h4("Average Flight Durations (min)")),
                                 div(tableOutput("table"), style = "font-size:80%")
                               ),
                               mainPanel(
                                 fluidRow(
                                   column(12,
                                          helpText(h4("Flight Duration per Airline")),
                                          plotOutput("DurationPlot"),
                                          column(4,
                                                 helpText(h4("Taxi out Duration per Airline")),
                                                 plotOutput("TaxiOutPlot")
                                          ),
                                          column(4,
                                                 helpText(h4("Air Time per Airline")),
                                                 plotOutput("AirTimePlot")
                                          ),
                                          column(4,
                                                 helpText(h4("Taxi in Duration per Airline")),
                                                 plotOutput("TaxiInPlot")
                                          )
                                   )
                                 )
                               )
                             )
                    ),
                    tabPanel('USA Airline operated Airports',
                             titlePanel("Information on Airports in the USA"),
                             sidebarLayout(
                               sidebarPanel(
                                 helpText("This application provides insights into the performance of several (USA airline operation) airports in the United States"),
                                 selectInput("AirportSelection", "Select Airport", ""),
                                 sliderInput("timeslider", label = "Select time", min=0, max = 2400, value =c(0,2400), step = 100),
                                 br(),
                                 h5(textOutput("NumberFlightText")),
                                 h5(textOutput("NumberAirlinesText"))
                               ),
                               mainPanel(
                                 fluidRow(
                                   column(12,
                                          column(6,
                                                 helpText(h4("Airplane Arrival Destribution")),
                                                 plotOutput("ArrivalIntervalPlot")
                                          ),
                                          column(6,
                                                 helpText(h4("Airplane Departure Destribution")),
                                                 plotOutput("DepartureIntervalPlot")
                                          )
                                   ),
                                   column(12, 
                                          helpText(h4("Number of Delays")),
                                          plotOutput("DelayedFlightsPlot")
                                   )
                                 )
                               )
                             )
                    )
                  )
)
)

