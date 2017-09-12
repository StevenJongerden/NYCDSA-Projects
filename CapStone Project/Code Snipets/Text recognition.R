############## R CODE ##########

#install.packages("stringdist")
library(dplyr)
library(stringdist)

#Input string
SearchString <- c('a saloon with a 150 horsepower engine aircnditoning cruisecontrol and four wheel drive')

#Function to find which options to include
SearchString <- unlist(strsplit(tolower(as.character(SearchString)), " "))
SearchStringLength <- length(unlist(strsplit(tolower(as.character(SearchString)), " ")))
Options <- data.frame(Options = c('airconditioning',"conditioning", "navigation", "parking sensors", "parking",
                                  "power steering", "steering", "reverse camera", "camera", "TCS", "traction",
                                  "tyre pressure", "cruisecontrol", "cruise", "horsepower", "convertible",
                                  "coupe", "estate", "hatchback", "mpv", "saloon", "suv", "automatic", "manual",
                                  "diesel", "petrol", "electric", "four wheel drive", "front wheel drive","rear wheel drive"))

for (i in 1:SearchStringLength){
  Options[,SearchString[i]] <- unlist(sapply(Options[,1], Map, f=stringdist, SearchString[i], method="cosine"))
}

Options$min <- apply(Options[2:(SearchStringLength)], 1, min)
Options$present <- ifelse(Options$min < 0.05, "yes", "no")
SelectOptions <-select(Options, Options, present)
rownames(SelectOptions) <- SelectOptions$Options
SelectOptions$Options <- NULL

if (is.numeric(as.numeric(SearchString[grep("horsepower", SearchString)-1])) & 
    !is.na(as.numeric(SearchString[grep("horsepower", SearchString)-1]))){
  SelectOptions["horsepower",1] = as.numeric(SearchString[grep("horsepower", SearchString)-1])
} else if (is.numeric(as.numeric(SearchString[grep("horsepower", SearchString)+1])) & 
           !is.na(as.numeric(SearchString[grep("horsepower", SearchString)+1]))) {
  SelectOptions["horsepower",1] = as.numeric(SearchString[grep("horsepower", SearchString)+1])
}

#Options in the car 


SelectOptions

########################## SHINY #####################

observeEvent("buttionclick", {
  
  #Inport the search querry
  SearchString <- input$textquery
  
  #Function to find which options to include
  SearchString <- unlist(strsplit(tolower(as.character(SearchString)), " "))
  SearchStringLength <- length(unlist(strsplit(tolower(as.character(SearchString)), " ")))
  Options <- data.frame(Options = c('airconditioning',"conditioning", "navigation", "parking sensors", "parking",
                                    "power steering", "steering", "reverse camera", "camera", "TCS", "traction",
                                    "tyre pressure", "cruisecontrol", "cruise", "horsepower", "convertible",
                                    "coupe", "estate", "hatchback", "mpv", "saloon", "suv", "automatic", "manual",
                                    "diesel", "petrol", "electric", "four wheel drive", "front wheel drive","rear wheel drive"))
  
  for (i in 1:SearchStringLength){
    Options[,SearchString[i]] <- unlist(sapply(Options[,1], Map, f=stringdist, SearchString[i], method="cosine"))
  }
  
  Options$min <- apply(Options[2:(SearchStringLength)], 1, min)
  Options$present <- ifelse(Options$min < 0.05, "yes", "no")
  SelectOptions <-select(Options, Options, present)
  rownames(SelectOptions) <- SelectOptions$Options
  SelectOptions$Options <- NULL
  
  if (is.numeric(as.numeric(SearchString[grep("horsepower", SearchString)-1])) & 
      !is.na(as.numeric(SearchString[grep("horsepower", SearchString)-1]))){
    SelectOptions["horsepower",1] = as.numeric(SearchString[grep("horsepower", SearchString)-1])
  } else if (is.numeric(as.numeric(SearchString[grep("horsepower", SearchString)+1])) & 
             !is.na(as.numeric(SearchString[grep("horsepower", SearchString)+1]))) {
    SelectOptions["horsepower",1] = as.numeric(SearchString[grep("horsepower", SearchString)+1])
  }
  
  #Update all settings based on text field 
  if (SelectOptions["airconditioning",]=="yes"){
    updateSelectInput(session, "AirConditioning", label = "AirConditioning",  value = TRUE)
  } else {
    updateSelectInput(session, "AirConditioning", label = "AirConditioning",  value = FALSE)
    }
  
  #Add more functions

  
  
})




