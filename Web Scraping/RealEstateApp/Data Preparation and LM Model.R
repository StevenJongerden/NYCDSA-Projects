library(corrplot)
library(dplyr)
library(ggplot2)
library(readr)
library(stats)


HouseInformation<- read_csv("C:/Users/Steven Jongerden/Desktop/Real Estate App/HouseInformation.csv", 
                                     col_types = cols(Bouwjaar = col_number(), 
                                                      Capagiteit = col_number(), `Externe bergruimte` = col_number(), 
                                                      `Gebouwgebonden buitenruimte` = col_number(), 
                                                      Inhoud = col_number(), `Overige inpandige ruimte` = col_number(), 
                                                      Perceeloppervlakte = col_number(), 
                                                      Vraagprijs = col_number(), Woonoppervlakte = col_number()), 
                                     na = "0")






#Correct roves
roves = c('Lessenaardak', 'Dwarskap', 'Mansarde', 'Plat dak', 'Samengesteld dak', 'Schilddak', 'Tentdak', 'Zadeldak')
for (i in 1:8) {
HouseInformation$`Soort dak` <- ifelse(grepl(roves[i], HouseInformation$`Soort dak`), roves[i], HouseInformation$`Soort dak`)
}
HouseInformation$`Soort dak`[HouseInformation$`Soort dak`=="Soort dak"] <- "Plat dak"

#Correct number of rooms
HouseInformation$`Aantal kamers` <- as.numeric(substr(HouseInformation$`Aantal kamers`,start = 1,stop = 1))

#Correct number of bathrooms
HouseInformation$`Aantal badkamers` <- as.numeric(substr(HouseInformation$`Aantal badkamers`,start = 1,stop = 1))

#Correct number of floors
HouseInformation$`Aantal woonlagen` <- as.numeric(substr(HouseInformation$`Aantal woonlagen`,start = 1,stop = 1))


#Correct heating
heating = c('Blokverwarming', 'Cv-ketel', 'vloerverwarming', 'Cv-ketel en gehele vloerverwarming', 'Cv-ketel, gedeeltelijke vloerverwarming',
          'Cv-ketel, open haard en gedeeltelijke vloerverwarming', 'Gaskachels', 'Heteluchtverwarming', 'Stadsverwarming', 'Warmtepomp', 'Open haard')
for (i in 1:11) {
  HouseInformation$Verwarming <- ifelse(grepl(heating[i], HouseInformation$Verwarming), heating[i], HouseInformation$Verwarming)
}
heatingother = c('Houtkachel', 'Moederhaard', 'Warmte terugwininstallatie', 'Aardwarmte', 'Elektrische verwarming', 'Geen verwarming',
                 'Houtkachel en stadsverwarming', 'Mogelijkheid voor open haard', 'Verwarming', 'Warmte terugwininstallatie en aardwarmte',
                 'Gashaard', 'Kolenkachel', 'Open haard')

for (i in 1:13) {
  HouseInformation$Verwarming <- ifelse(grepl(heatingother[i], HouseInformation$Verwarming), "Other", HouseInformation$Verwarming)
}

#Correct location
location = c("woonwijk", "centrum", "bosrijke", "landelijk", "bosrand", "water", "park", "buiten bebouwde kom", "drukke weg", "vrij uitzicht")

for (i in 1:10) {
  HouseInformation$Ligging <- ifelse(grepl(location[i], HouseInformation$Ligging), location[i], HouseInformation$Ligging)
}

locationother = c('Ligging', 'Vrij uitzicht en aan rustige weg', 'Landelijk gelegen', 'Buiten bebouwde kom')
for (i in 1:4) {
  HouseInformation$Ligging <- ifelse(grepl(locationother[i], HouseInformation$Ligging), 'other', HouseInformation$Ligging)
}

#garden
HouseInformation$Tuin <- ifelse(grepl("rondom", HouseInformation$Tuin), "Tuin rondom", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("Voortuin, zijtuin", HouseInformation$Tuin), "Voortuin, zijtuin", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("achtertuin", HouseInformation$Tuin), "Achtertuin", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("Achtertuin", HouseInformation$Tuin), "Achtertuin", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("voortuin", HouseInformation$Tuin), "Voortuin", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("Voortuin", HouseInformation$Tuin), "Voortuin", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("Zijtuin", HouseInformation$Tuin), "Zijtuin", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("zijtuin", HouseInformation$Tuin), "Zijtuin", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("Patio/atrium", HouseInformation$Tuin), "Patio/atrium", HouseInformation$Tuin)
HouseInformation$Tuin <- ifelse(grepl("Plaats en zonneterras", HouseInformation$Tuin), "Other", HouseInformation$Tuin)
HouseInformation[HouseInformation$Tuin=="",'Tuin'] <- "None"

#Balcony
HouseInformation$`Balkon/dakterras` <- ifelse(grepl("frans balkon", HouseInformation$`Balkon/dakterras`), "Frans balkon aanwezig", HouseInformation$`Balkon/dakterras`)
HouseInformation[HouseInformation$`Balkon/dakterras`=="",'Balkon/dakterras'] <- "None"

#Additional storage
HouseInformation[HouseInformation$`Schuur/berging`=="",'Schuur/berging'] <- "None"


HouseInformation$`Soort garage` <- ifelse(grepl("parkeerplaats", HouseInformation$`Soort garage`), "Parkeerplaats", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("Parkeerplaats", HouseInformation$`Soort garage`), "Parkeerplaats", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("Garage", HouseInformation$`Soort garage`), "Garage", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("garage", HouseInformation$`Soort garage`), "Garage", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("Niet aanwezig, wel mogelijk", HouseInformation$`Soort garage`), "None", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("parkeerkelder", HouseInformation$`Soort garage`), "Parkeerkelder", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("Parkeerkelder", HouseInformation$`Soort garage`), "Parkeerkelder", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("Carport en inpandig", HouseInformation$`Soort garage`), "Inpandig en carport", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("Inpandig", HouseInformation$`Soort garage`), "Inpandig", HouseInformation$`Soort garage`)
HouseInformation$`Soort garage` <- ifelse(grepl("Aangebouwde", HouseInformation$`Soort garage`), "Aangebouwd", HouseInformation$`Soort garage`)
HouseInformation[HouseInformation$`Soort garage`=="","Soort garage"] <- "None"

#Parking
HouseInformation$`Soort parkeergelegenheid` <- ifelse(grepl("Betaald parkeren", HouseInformation$`Soort parkeergelegenheid`), "Betaald parkeren", HouseInformation$`Soort parkeergelegenheid`)
HouseInformation$`Soort parkeergelegenheid` <- ifelse(grepl("Openbaar parkeren", HouseInformation$`Soort parkeergelegenheid`), "Openbaar parkeren", HouseInformation$`Soort parkeergelegenheid`)
HouseInformation$`Soort parkeergelegenheid` <- ifelse(grepl("Parkeergarage", HouseInformation$`Soort parkeergelegenheid`), "Parkeergarage", HouseInformation$`Soort parkeergelegenheid`)
HouseInformation$`Soort parkeergelegenheid` <- ifelse(grepl("Soort parkeergelegenheid", HouseInformation$`Soort parkeergelegenheid`), "None", HouseInformation$`Soort parkeergelegenheid`)
HouseInformation[HouseInformation$`Soort parkeergelegenheid`=="","Soort parkeergelegenheid"] <- "None"

#Type of house
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Herenhuis", HouseInformation$`Soort woonhuis`), "Herenhuis", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Villa", HouseInformation$`Soort woonhuis`), "Villa", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Landhuis", HouseInformation$`Soort woonhuis`), "Landhuis", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Grachtenpand", HouseInformation$`Soort woonhuis`), "Herenhuis", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("2-onder-1-kapwoning", HouseInformation$`Soort woonhuis`), "2-onder-1-kapwoning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Eengezinswoning, vrijstaande woning", HouseInformation$`Soort woonhuis`), "Eengezinswoning, vrijstaande woning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Bungalow", HouseInformation$`Soort woonhuis`), "Bungalow", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("tussenwoning", HouseInformation$`Soort woonhuis`), "Eengezinswoning, tussenwoning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Landgoed", HouseInformation$`Soort woonhuis`), "Villa", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Woonboerderij", HouseInformation$`Soort woonhuis`), "Woonboerderij", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("hoekwoning", HouseInformation$`Soort woonhuis`), "Eengezinswoning, hoekwoning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("geschakelde woning", HouseInformation$`Soort woonhuis`), "Eengezinswoning, tussenwoning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("eindwoning", HouseInformation$`Soort woonhuis`), "Eengezinswoning, hoekwoning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("halfvrijstaande", HouseInformation$`Soort woonhuis`), "Eengezinswoning, hoekwoning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("verspringend", HouseInformation$`Soort woonhuis`), "Eengezinswoning, hoekwoning", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("waterwoning", HouseInformation$`Soort woonhuis`), "Woonboot", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Woonboot", HouseInformation$`Soort woonhuis`), "Woonboot", HouseInformation$`Soort woonhuis`)
HouseInformation$`Soort woonhuis` <- ifelse(grepl("Soort woonhuis", HouseInformation$`Soort woonhuis`), "Other", HouseInformation$`Soort woonhuis`)
HouseInformation[HouseInformation$`Soort woonhuis`=="","Soort woonhuis"] <- "None"

#isolation
HouseInformation$Dakisolatie <- ifelse(grepl("Dakisolatie", HouseInformation$Isolatie), 'yes', 'no')
HouseInformation$Dakisolatie <- ifelse(grepl("dakisolatie", HouseInformation$Isolatie), 'yes', HouseInformation$Dakisolatie)
HouseInformation$Dubbeglas <- ifelse(grepl("Dubbel glas", HouseInformation$Isolatie), 'yes', 'no')
HouseInformation$Dubbeglas <- ifelse(grepl("dubbel glas", HouseInformation$Isolatie), 'yes', HouseInformation$Dubbeglas)
HouseInformation$Muurisolatie <- ifelse(grepl("muurisolatie", HouseInformation$Isolatie), 'yes', 'no')
HouseInformation$Muurisolatie <- ifelse(grepl("Muurisolatie", HouseInformation$Isolatie), 'yes', HouseInformation$Muurisolatie)
HouseInformation$Vloerisolatie <- ifelse(grepl("Vloerisolatie", HouseInformation$Isolatie), 'yes', 'no')
HouseInformation$Vloerisolatie <- ifelse(grepl("vloerisolatie", HouseInformation$Isolatie), 'yes', HouseInformation$Vloerisolatie)
HouseInformation$VolledigIsolatie <- ifelse(grepl("Volledig", HouseInformation$Isolatie), 'yes', 'no')
HouseInformation$VolledigIsolatie <- ifelse(grepl("volledig", HouseInformation$Isolatie), 'yes', HouseInformation$VolledigIsolatie)

HouseInformation$Adress <- substr(HouseInformation$Adress, 1, 4)


model <- lm(Vraagprijs ~ 0 + Woonoppervlakte + Perceeloppervlakte + `Soort woonhuis` + VolledigIsolatie + `Aantal badkamers`+
            `Aantal woonlagen` + `Soort parkeergelegenheid` + Ligging + Tuin + Adress, data = HouseInformation)

# https://economictheoryblog.com/2016/08/08/robust-standard-errors-in-r/
#https://economictheoryblog.com/2016/08/07/robust-standard-errors/

# load necessary packages for importing the function
library(RCurl)

# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

modelresults <- summary(model, robust=T)
write.table(modelresults[4], "summary.txt")
FinalModelResults <- read.table("summary.txt", sep = " ")
View(FinalModelResults)


library(lmtest)
bgtest(Vraagprijs ~ Woonoppervlakte + Perceeloppervlakte + `Soort woonhuis` + VolledigIsolatie + `Aantal badkamers`+
         `Aantal woonlagen` + `Soort parkeergelegenheid` + Ligging + Tuin + Adress, data = HouseInformation)

bptest(Vraagprijs ~ Woonoppervlakte + Perceeloppervlakte + `Soort woonhuis` + VolledigIsolatie + `Aantal badkamers`+
         `Aantal woonlagen` + `Soort parkeergelegenheid` + Ligging + Tuin + Adress, data = HouseInformation)

dwtest(Vraagprijs ~ Woonoppervlakte + Perceeloppervlakte + `Soort woonhuis` + VolledigIsolatie + `Aantal badkamers`+
         `Aantal woonlagen` + `Soort parkeergelegenheid` + Ligging + Tuin + Adress, data = HouseInformation)











