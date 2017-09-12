##########################################################################################################################################
### Capstone Project for the NYC Data Science Academy Cohort 10, submitted on 12 September 2017.                                       ###
### Authors: Steven Jongerden, Huanghaotian Fu.                                                                                        ###         
##########################################################################################################################################

##########################################################################################################################################
############################################################ Libraries and Setup #########################################################

#Empty GLobal Environment
rm(list = ls())

#Libraries 
if (!require("readr")) install.packages("readr")
if (!require("kknn")) install.packages("kknn")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("corrplot")) install.packages("corrplot")
if (!require("caret")) install.packages("caret")
if (!require("robustHD")) install.packages("robustHD")
if (!require("scales")) install.packages("scales")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("foreach")) install.packages("foreach")
if (!require("doParallel")) install.packages("doParallel")
if (!require("robustHD")) install.packages("robustHD")
if (!require("lmtest")) install.packages("lmtest")
if (!require("car")) install.packages("car")
if (!require("MASS")) install.packages("MASS")
if (!require("lm.beta")) install.packages("lm.beta")
if (!require("glmnet")) install.packages("glmnet")
if (!require("gbm")) install.packages("gbm")

#Set the number of workers for parellel computing
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

#Se the location for the dataset and where the code should save the plots
filelocation <- "C:/Users/Steven Jongerden/Desktop/CapStone Project/Data/CarDataSet.csv"
PlotLocation <- "C:/Users/Steven Jongerden/Desktop/CapStone Project/EDA"

# filelocation = '/Users/huanghaotian/Desktop/Final Project/CarDataSet.csv'
# PlotLocation = '/Users/huanghaotian/Desktop/Final Project'

rm(cl)
##########################################################################################################################################

##########################################################################################################################################
########################################################## User Defined Functions ########################################################

#Determine crosscorrelation significance for crossplot
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

#Function to determine the number of clusters, improved with parallel computing
wssplot = function(data, nc = 15, seed = 0) {
  wss <- foreach (i = 2:nc, combine=c) %dopar% {
    set.seed(0)
    sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
  }
  
  plot(2:15, unlist(wss), type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
}

#Function to calculate the Adjusted R Squared for the test or train data set. 
AdjustedR <- function(model=model, testdata=testdata){
  Prediction = predict(model, newdata = testdata)
  TSS <- sum((testdata$CarPrice - mean(testdata$CarPrice, na.rm =  TRUE))^2)
  RSS <- sum((Prediction - testdata$CarPrice)^2, na.rm = TRUE)
  AdjR2 <- 1-((RSS/(length(testdata$CarPrice)-length(summary(model)$coefficients)-1))/(TSS/(length(testdata$CarPrice)-1)))
  AdjR2
}

RSquare <- function(model=model, testdata=testdata){
  Prediction = predict(model, newdata = testdata)
  TSS <- sum((testdata$CarPrice - mean(testdata$CarPrice, na.rm =  TRUE))^2)
  RSS <- sum((Prediction - testdata$CarPrice)^2, na.rm = TRUE)
  AdjR2 <- 1-RSS/TSS
  AdjR2
}

#Standardized Coefficients presented from most important to least important
StandardCoefficients <- function(model=model){
  StdCoef <- lm.beta(ManualAdjustedLinear)
  StdCoef <- data.frame(StdCoef[1])
  StdCoef$Variable <- rownames(StdCoef)
  rownames(StdCoef) <- NULL
  dplyr::select(StdCoef, Variable, coefficients) %>% arrange(desc(abs(coefficients)))
}

##########################################################################################################################################

##########################################################################################################################################
############################################################## Importing Data  ###########################################################

#Load in the dataset
CarDataSet <- read_csv(filelocation)
CarDataSet$X1 <- NULL
colnames(CarDataSet) <- c("CarName", "CarPrice", "WebRating", "Year", "CarType", "Miles", "Transmission", "EngineDiscplacement",
                          "FuelType", "UrbanMPG", "ExtraUrbanmpg", "Averagempg", "CO2emissions", "AnnualTax", "Enginepower",
                          "Enginesize", "BrochureEnginesize", "Acceleration", "Topspeed", "Drivetrain", "NrDoors", "NrSeats",
                          "Options")
rm(filelocation)
##########################################################################################################################################

##########################################################################################################################################
############################################################# Data Transformation ########################################################

#Remove the additionally printed headers in the data because of scraping script. 
CarDataSet <- CarDataSet[CarDataSet$CarName!="CarName",]

#Remove the rows that are complete NA, indicated as the car name to be NA
CarDataSet <- CarDataSet[!is.na(CarDataSet$CarName),]

#Drop not relevant columns:
CarDataSet$EngineDiscplacement <- NULL #Has a better alternative: Engine Size
CarDataSet$BrochureEnginesize <- NULL #Has a better alternative: Engine Size
CarDataSet$AnnualTax <- NULL #Only applicable for UK

#Create a new column "Brand" that is the brand name of the car
carlist <- c("Land Rover", "Mazda", "Audi", "Mercedes-Benz", "MINI", "Nissan", "Citroen", "Peugeot", "Renault",
  "Skoda", "Ford", "Hyundai", "Toyota", "Vauxhall", "Jaguar", "Volkswagen", "Volvo", "KIA", "Mitsubishi", "BMW", "Alpha Romeo")

CarDataSet$Brand <- 0
for (i in 1:length(carlist)){
CarDataSet$Brand <- ifelse(grepl(carlist[i], CarDataSet$CarName), carlist[i], CarDataSet$Brand)
}
CarDataSet$Brand <- ifelse(grepl("MAZDA3", CarDataSet$CarName), "Mazda", CarDataSet$Brand)
CarDataSet$Brand <- ifelse(grepl("MAZDA6", CarDataSet$CarName), "Mazda", CarDataSet$Brand)
CarDataSet$Brand <- ifelse(grepl("MAZDA2", CarDataSet$CarName), "Mazda", CarDataSet$Brand)
CarDataSet$Brand <- ifelse(grepl("MAZDA 2", CarDataSet$CarName), "Mazda", CarDataSet$Brand)
CarDataSet$Brand <- ifelse(grepl("Mitsubishi", CarDataSet$CarName), "Mitsubishi", CarDataSet$Brand)
CarDataSet$Brand <- factor(CarDataSet$Brand)
CarDataSet <- CarDataSet[!is.na(CarDataSet$Brand),] 

#Reformat the Car price column to integer format (price is in pounds)
CarDataSet$CarPrice <- ifelse(is.na(as.numeric(gsub(",", "",substr(CarDataSet$CarPrice,9,20)))),
                              as.integer(gsub(",", "",substr(CarDataSet$CarPrice,2,20))),
                              as.integer(gsub(",", "",substr(CarDataSet$CarPrice,9,20)))
                              )

#Convesrion of car price pound to dollar
CarDataSet$CarPrice <- round(CarDataSet$CarPrice * 1.29, 0)

#Convert the webrating to nummeric 
CarDataSet$WebRating <-as.numeric(substr(CarDataSet$WebRating,1,3))

#Convert Year to integer 
CarDataSet$Year <- as.integer(CarDataSet$Year)

#Convert CarType to factor
CarDataSet$CarType <- as.factor(CarDataSet$CarType)

##Convert miles to integer and remove the word miles
CarDataSet$Miles <- gsub("miles", "", CarDataSet$Miles)
CarDataSet$Miles <- as.integer(gsub(",", "", CarDataSet$Miles))

#Convert trasmission to factor
CarDataSet$Transmission <- as.factor(CarDataSet$Transmission)
CarDataSet[grepl("1",CarDataSet$Transmission),"Transmission"] <- NA
CarDataSet[grepl("2",CarDataSet$Transmission),"Transmission"] <- NA

#Convert Fuel type to factor
CarDataSet$FuelType <- as.factor(CarDataSet$FuelType)

#Convert Urban MPG to nummeric 
CarDataSet[CarDataSet$UrbanMPG=="No details available","UrbanMPG"] <- NA
CarDataSet$UrbanMPG <- as.numeric(gsub("mpg", "", CarDataSet$UrbanMPG))

#Convert Extra Urban MPG to nummeric 
CarDataSet[CarDataSet$ExtraUrbanmpg =="No details available","ExtraUrbanmpg"] <- NA
CarDataSet$ExtraUrbanmpg <- as.numeric(gsub("mpg", "", CarDataSet$ExtraUrbanmpg))

#Convert Average mpg
CarDataSet[CarDataSet$Averagempg=="No details available","Averagempg"] <- NA
CarDataSet$Averagempg <- as.numeric(gsub("mpg", "", CarDataSet$Averagempg))

#Convert C02 Emissions
CarDataSet[CarDataSet$CO2emissions =="No details available","CO2emissions"] <- NA
CarDataSet$CO2emissions <- as.numeric(gsub("g/km", "", CarDataSet$CO2emissions))

#Convert Engine power
CarDataSet[CarDataSet$Enginepower =="No details available","Enginepower"] <- NA
CarDataSet$Enginepower <- as.numeric(gsub("bhp", "", CarDataSet$Enginepower))

#Convert Engine size
CarDataSet[CarDataSet$Enginesize =="No details available","Enginesize"] <- NA
CarDataSet$Enginesize <- as.numeric(gsub("cc", "", CarDataSet$Enginesize))

#Convert Acceleration 0 - 6
CarDataSet[CarDataSet$Acceleration =="No details available","Acceleration"] <- NA
CarDataSet$Acceleration <- as.numeric(gsub("seconds", "", CarDataSet$Acceleration))

#Convert Maximum Speed
CarDataSet[CarDataSet$Topspeed =="No details available","Topspeed"] <- NA
CarDataSet$Topspeed <- as.numeric(gsub("mph", "", CarDataSet$Topspeed))

#Convert DriveChain
CarDataSet[CarDataSet$Drivetrain=="No details available","Drivetrain"] <- NA
CarDataSet$Drivetrain <- as.factor(CarDataSet$Drivetrain)

#Convert Nr Doors
CarDataSet[CarDataSet$NrDoors =="No details available","NrDoors"] <- NA
CarDataSet$NrDoors <- as.integer(CarDataSet$NrDoors)

#Convert Nr Seats
CarDataSet[CarDataSet$NrSeats =="No details available","NrSeats"] <- NA
CarDataSet$NrSeats <- as.integer(CarDataSet$NrSeats)

CarDataSet <- CarDataSet[CarDataSet$Brand!="Ferrari",]

rm(carlist)
rm(i)
##########################################################################################################################################

##########################################################################################################################################
############################################################# Missing Imputation #########################################################

#Missingness
sapply(CarDataSet, function(x) round(sum(is.na(x))/nrow(CarDataSet),3))

#Missing Imputation with KNN for WebRating based on Brand and CarPrice
factors <- c("CarPrice", "Brand", "CarType")
for (i in 1:21){
  if (sum(is.na(CarDataSet[,i]))>0){
      CarDataSet$Missing <- ifelse(is.na(CarDataSet[,i]),1,0)
      complete <- filter(CarDataSet, Missing==0)
      missing <- filter(CarDataSet, Missing==1)
      missing[,colnames(CarDataSet)[i]] <- NULL
      MissingImpute = kknn(as.formula(paste(colnames(CarDataSet)[i],"~", paste(factors, collapse="+"))), complete, missing, k = round(sqrt(nrow(complete)),0))
      missing[,colnames(CarDataSet)[i]] <- fitted(MissingImpute)
      CarDataSet <- rbind(complete, missing)
  }
}

#Remove datasets for KNN imputation
rm(complete)
rm(missing)

#Remove the missing column 
CarDataSet$Missing <- NULL

#Missingness after KNN imputation
sapply(CarDataSet, function(x) round(sum(is.na(x))/nrow(CarDataSet),3))

#Removal of rows that are "unlisted" (only a very small portion of the data)
CarDataSet <- CarDataSet[CarDataSet$CarType!="Unlisted",]
CarDataSet <- CarDataSet[CarDataSet$Transmission!="Unlisted",]
CarDataSet <- CarDataSet[CarDataSet$FuelType!="Unlisted",]

rm(factors)
rm(i)
rm(MissingImpute)

##########################################################################################################################################

##########################################################################################################################################
############################################################### Outlier removal ############################################################

#Reducing outliers by means of winsorization
CarDataSet$CarPrice = winsorize(CarDataSet$CarPrice, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$Year = squish(CarDataSet$Year, round(quantile(CarDataSet$Year, c(.05, .95))))
CarDataSet$UrbanMPG = winsorize(CarDataSet$UrbanMPG, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$ExtraUrbanmpg = winsorize(CarDataSet$ExtraUrbanmpg, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$Averagempg = winsorize(CarDataSet$Averagempg, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$CO2emissions = winsorize(CarDataSet$CO2emissions, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$Enginepower = winsorize(CarDataSet$Enginepower, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$Enginesize = winsorize(CarDataSet$Enginesize, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$Acceleration = winsorize(CarDataSet$Acceleration, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$Topspeed = winsorize(CarDataSet$Topspeed, probs = c(0.025, 0.975), na.rm = FALSE)
CarDataSet$Year = squish(CarDataSet$Year, round(quantile(CarDataSet$Year, c(.05, .95))))

##########################################################################################################################################

##########################################################################################################################################
############################################################# Data Manipulation ##########################################################

#Removing incorrect values from the dataset by means of imputation
CarDataSet$CarType = gsub(pattern = '10 miles', replacement = 'Others', x = CarDataSet$CarType)
CarDataSet$CarType = gsub(pattern = 'Combi Van', replacement = 'Others', x = CarDataSet$CarType)
CarDataSet$CarType = gsub(pattern = 'Pickup', replacement = 'Others', x = CarDataSet$CarType)
CarDataSet$CarType = as.factor(CarDataSet$CarType)
str(CarDataSet$CarType)

CarDataSet$Transmission = gsub(pattern = 'Petrol', replacement = 'Manual', x = CarDataSet$Transmission)
CarDataSet$Transmission = as.factor(CarDataSet$Transmission)
str(CarDataSet$Transmission)

CarDataSet$Drivetrain = gsub(pattern = 'Direct Drive', replacement = 'Front Wheel Drive', CarDataSet$Drivetrain)
CarDataSet$Drivetrain = as.factor(CarDataSet$Drivetrain)
str(CarDataSet$Drivetrain)

CarDataSet$FuelType = gsub(pattern = 'Electric', replacement = 'Electric/Hybird', x = CarDataSet$FuelType)
CarDataSet$FuelType = gsub(pattern = 'Hybrid', replacement = 'Electric/Hybird', x = CarDataSet$FuelType)
CarDataSet$FuelType = as.factor(CarDataSet$FuelType)
str(CarDataSet$FuelType)

##########################################################################################################################################

##########################################################################################################################################
########################################################## Determining Important Options #################################################

#Create additional variables that indicate if certain populair features are present in the car.
datalist <- c("EBD", "Cruise Control", "DSC", "TCS", "Power Steering", "ESC", "ABS", "HHA",
              "Air Conditioning", "Rain Sensor","Airbag", "Tyre Pressure Monitoring System", 
              "Parking Sensors", "StartStop System", "Navigation", "Electric Windows", "Reversing Camera")

data <- data.frame(foreach (i = 1:length(datalist), combine = cbind) %dopar% {
  if (!require("robustHD")) install.packages("robustHD")
  ifelse(grepl(datalist[i], CarDataSet$Options),"yes", "no")
})
colnames(data) <- gsub(" ", "", datalist)
CarDataSet <- cbind(CarDataSet, data)

rm(data)

#Drop the options variable
CarDataSet$Options <- NULL

for (i in 21:37) {
  CarDataSet[,i] <- factor(CarDataSet[[i]])
}

rm(datalist)
rm(i)
##########################################################################################################################################

##########################################################################################################################################
#################################################################### EDA ################################################################

#Distribtuin Categorical Types
ordinal_features <- names(CarDataSet)[sapply(CarDataSet, is.factor)]
foreach (i = 1:length(ordinal_features)) %dopar% {
  library(ggplot2)
  library(ggthemes)
  print(ggplot(CarDataSet, aes_string(ordinal_features[i]))+geom_bar(stat="count")+ 
          theme_economist() + scale_colour_economist()+ggtitle(ordinal_features[i]))
  ggsave(paste("EDA/Plot ", ordinal_features[i],".jpg"), width = 40, height = 20, units = "cm")
}

#Distribtuin Nummeric Types
numeric_features <- c("CarPrice", "WebRating", "Year", "Miles", "UrbanMPG", "ExtraUrbanmpg", "Averagempg",
                      "CO2emissions", "Enginepower", "Enginesize",
                      "Acceleration", "Topspeed", "NrDoors", "NrSeats")
bins <- c(30,20,2,30,30,20,20,30,20,15,30,30,4,6)

foreach (i = 1:length(numeric_features)) %dopar% {
  library(ggplot2)
  library(ggthemes)
  print(ggplot(CarDataSet, aes_string(numeric_features[i]))+geom_histogram(bins=bins[i]) + 
    xlab(numeric_features[i])+ theme_economist() + scale_colour_economist()+ggtitle(numeric_features[i]))
  ggsave(paste("EDA/Plot Nummeric",numeric_features[i],".jpg"), width = 40, height = 20, units = "cm")
}

###Brand specific information ###
#Boxplot
features <- c("CarPrice","Topspeed", "Acceleration", "Enginepower", "CO2emissions", "Averagempg")
xlab = c("Car Brand")
ylab = c("Car Price in dollar", "Top speed (mph)", "Acceleration (0-60mph)", "Horsepower (bhp)", "CO2 emissions (g/km)", "Average mpg")
ggtitle = c("The price distribution per car brand", "The speed distribution per car brand", "The Acceleration distribution per car brand", 
            "The Horsepower distribution per car brand", "The CO2 emissions distribution per car brand",
            "The Average mpg distribution per car brand")

foreach (g = 1:6) %dopar% {
  library(ggplot2)
  library(ggthemes)
  print(ggplot(CarDataSet, aes_string(paste("reorder(","Brand,", features[g], ",median)"), features[g]))+geom_boxplot() + 
    xlab(xlab) + ylab(ylab[g]) + ggtitle(ggtitle[g]) +
    theme_economist() + scale_colour_economist())
  ggsave(paste("EDA/Boxplot Brand",features[g],".jpg"), width = 40, height = 20, units = "cm")
}

#Density Plots
foreach (g = 1:6) %dopar% {
  library(ggplot2)
  library(ggthemes)
  print(ggplot(CarDataSet, aes_string(features[g], fill="Brand"))+geom_density(alpha=0.5) + 
  xlab(ylab[g]) + ggtitle(ggtitle[g]) +
  theme_economist() + scale_colour_economist())
  ggsave(paste("EDA/Density Plot",features[g],".jpg"), width = 40, height = 20, units = "cm")
}

###Price specific information ###
#Scatter plots
features <- c("Topspeed", "Acceleration", "Enginepower", "CO2emissions", "Averagempg", "Miles")
ylab = c("Car Price in dollar")
xlab = c("Top speed (mph)", "Acceleration (0-60mph)", "Horsepower (bhp)", "CO2 emissions (g/km)", "Average mpg", "Miles")
ggtitle = c("The price related to the topspeed", "The price related to the acceleration", "The price related to the Engine power",
            "The price related to the CO2 emissions", "The price related to the Average mpg", "Number of miles driven")

foreach (g = 1:6) %dopar% {
  library(ggplot2)
  library(ggthemes)
  print(ggplot(CarDataSet, aes_string(features[g],"CarPrice", color="Brand"))+geom_jitter() + 
  xlab(xlab[g]) + ylab(ylab) + ggtitle(ggtitle[g]) +
  theme_economist())
  ggsave(paste("EDA/Scatter Plot",features[g],".jpg"), width = 40, height = 20, units = "cm")
}

ordinal_features <- names(CarDataSet)[sapply(CarDataSet, is.factor)]
foreach (g = 1:length(ordinal_features)) %dopar% {
  library(ggplot2)
  library(ggthemes)
  print(ggplot(CarDataSet, aes_string(ordinal_features[g], "CarPrice"))+geom_boxplot() + ggtitle(ordinal_features[g]) + theme_economist())
  ggsave(paste("EDA/BoxPlot Ordinal",ordinal_features[g],".jpg"), width = 40, height = 20, units = "cm")
}

#Crosscorrelation plot
#Nummeric crosscorrelation plot to indentify relations with price 
numeric_features <- names(CarDataSet)[sapply(CarDataSet, is.numeric)]
res1 <- cor.mtest(CarDataSet[,numeric_features],0.95)
numeric_features <- names(CarDataSet)[sapply(CarDataSet, is.numeric)]
CrossCorrelationNum <- cor(CarDataSet %>% dplyr::select(one_of(numeric_features, "CarPrice")), method = "pearson", use = "pairwise.complete.obs")
png(height=1200, width=1200, pointsize=25, file="CrossCorrelationNum.png")
corrplot(CrossCorrelationNum,p.mat = res1[[1]], sig.level=0.05, method = "circle", order ="FPC", na.label = "o")
dev.off()

#Categorical crosscorrelation plot to indentify relations with price 
CarDataSetSmall <- sample_n(CarDataSet, 1000, replace = TRUE)
CarDataSetSmall$StartStopSystem <- NULL
ordinal_features <- numeric_features <- names(CarDataSetSmall)[sapply(CarDataSetSmall, is.factor)]
CrossCorrelationCat <- cor(data.matrix(CarDataSetSmall %>% dplyr::select(one_of(ordinal_features, "CarPrice"))), method = "kendall",
                           use = "pairwise.complete.obs")
png(height=1200, width=1200, pointsize=25, file="CrossCorrelationCat.png")
corrplot(CrossCorrelationCat, method = "circle", order ="FPC", na.label = "o")
dev.off()

rm(CarDataSetSmall)
rm(numeric_features)
rm(ordinal_features)
rm(CrossCorrelationCat)
rm(bins)
rm(features)
rm(ggtitle)
rm(res1)
rm(xlab)
rm(ylab)
rm(cor.mtest)
rm(CrossCorrelationNum)
rm(cl)
rm(datalist)
rm(i)
##########################################################################################################################################

##########################################################################################################################################
############################################################## Statistical Testing #######################################################

#Welch Two Sample t-test for categorical variables that have 2 levels. 
CategoricalVariable <- colnames(CarDataSet[, (sapply(CarDataSet, function(col) length(unique(col))) ==2)])
Ttestresults <- data.frame(Significance = unlist(foreach (i = 1:length(CategoricalVariable), combine = cbind) %dopar% {
  round(t.test(CarDataSet$CarPrice ~ CarDataSet[,CategoricalVariable[i]])$p.value,3)
}))
Ttestresults <- cbind(CategoricalVariable, Ttestresults)
Ttestresults$Model <- ifelse(Ttestresults$Significance<0.05, "use in model", "do not use in model")
Ttestresults

rm(CategoricalVariable)

#Anova Testing for categorical variables that have more than 2 levels.
AnovaVectors <-data.frame(Factor = sapply(CarDataSet[, (sapply(CarDataSet, function(col) length(unique(col))) >2)], is.factor))
AnovaVectors$Variable <- rownames(AnovaVectors)
AnovaVectors <- AnovaVectors[AnovaVectors$Factor==TRUE,"Variable"]

Kruskaltestresults <- data.frame(Significance = unlist(foreach (i = 1:length(AnovaVectors), combine = cbind) %dopar% {
  round(kruskal.test(CarDataSet$CarPrice ~ CarDataSet[,AnovaVectors[1]])$p.value,3)
}))
Kruskaltestresults <- cbind(AnovaVectors, Kruskaltestresults)
Kruskaltestresults$Model <- ifelse(Kruskaltestresults$Significance<0.05, "use in model", "do not use in model")
Kruskaltestresults

rm(AnovaVectors)

#Pearson Correlation for numerical variables. 
PearsonVariable <- colnames(CarDataSet[,sapply(CarDataSet, function(col) is.numeric(col))])

PearsonPvalue <- data.frame(Significance = unlist(foreach (i = 1: length(PearsonVariable), combine = cbind) %dopar% {
  round(cor.test(CarDataSet$CarPrice , CarDataSet[,PearsonVariable[i]])$p.value,3)}))
PearsonStatistic <- data.frame(Statistics = unlist(foreach (i = 1: length(PearsonVariable), combine = cbind) %dopar% {
  round(cor.test(CarDataSet$CarPrice , CarDataSet[,PearsonVariable[i]])$statistic,3)}))
Persontestresults <- cbind(PearsonVariable, PearsonStatistic, PearsonPvalue)
Persontestresults$Direction <- ifelse(Persontestresults$Statistics>0, "Postive Relation", "Negative Relation")
Persontestresults

rm(PearsonVariable)
rm(PearsonPvalue)
rm(PearsonStatistic)

#Brand specific influence from an increase in HP
BrandSpecificEngine <-levels(CarDataSet$Brand)[-4]
EngineCoeff <- data.frame(Relation = unlist(foreach (i = 1:length(BrandSpecificEngine), combine = cbind) %dopar% {
  summary(lm(CarDataSet[CarDataSet$Brand==BrandSpecificEngine[i], "CarPrice"] ~ 0 + CarDataSet[CarDataSet$Brand==BrandSpecificEngine[i], "Enginepower"]))$coefficients[1]
}))

DollarPerHpBrank <- cbind(BrandSpecificEngine, EngineCoeff)
DollarPerHpBrank <- DollarPerHpBrank[order(DollarPerHpBrank$Relation, decreasing = TRUE),]
DollarPerHpBrank

rm(BrandSpecificEngine)
rm(EngineCoeff)
##########################################################################################################################################

##########################################################################################################################################
################################################################# Clustering #############################################################

#Experimental Clustering Analysis to undescribed information about the cars. 

#Scale the data in order to make the distance to each centroid equally weighted. 
num <- sapply(CarDataSet, function(x) is.numeric(x))
CarDataSet.scale = as.data.frame(scale(CarDataSet[,num]))
# wssplot(CarDataSet.scale)

#Creating of clusters for the analysis 
km.test = kmeans(CarDataSet.scale, centers = 2)
CarDataSet$Clustering <- km.test$cluster
CarDataSet$Clustering <- factor(CarDataSet$Clustering)

#Idicentifcation of the cluster variable 
print(ggplot(CarDataSet, aes(CarPrice, Averagempg, color=Clustering))+geom_point())
ggsave(paste("ClusteringPlot1.jpg"), width = 40, height = 20, units = "cm")

print(ggplot(CarDataSet, aes(Clustering, CarPrice))+geom_boxplot())
ggsave(paste("ClusteringPlot2.jpg"), width = 40, height = 20, units = "cm")

#The groups seem to identify some information about the size of the car.
#The red class seems to be more expensive and have lower average mpg, which indicates something about the weight of the car. 

rm(km.test)
rm(num)
rm(wssplot)
rm(CarDataSet.scale)

#Clustering cannot be used out of sample properly
CarDataSet$Clustering <- NULL
##########################################################################################################################################

##########################################################################################################################################
########################################################### Linear Model Stepwise  #######################################################

#Randomly Sample the Dataset as the dataset was collected by brand type, so data will appear to be serial correlated.
CarDataSet <- CarDataSet[sample(1:13626, size = 13626, replace = FALSE),]

#Remove the values due to windsorization
CarDataSet <- CarDataSet[CarDataSet$CarPrice != max(CarDataSet$CarPrice) ,]
CarDataSet$StartStopSystem <- NULL

#Create a testing and training data set based on 5 folds random sampling and splitting the dataset 80% train, 20% test. 
set.seed(0)
folds = createFolds(CarDataSet$CarPrice, 5)
test = CarDataSet[folds[[1]], ]
train = CarDataSet[-folds[[1]], ]
rm(folds)

#Removing the Car Name Varialbe
train <- train[1:nrow(train),-1]

#Stepwise Imputation of the variables with both forward and backwise steps
model.empty = lm(CarPrice ~ 1, data = train)
model.full = lm(CarPrice ~ ., data = train)
scope = list(lower = formula(model.empty), upper = formula(model.full))
LinearStepWiseModel = step(model.empty, scope, direction = "both", k = 2)

#Model Results
summary(LinearStepWiseModel)

#Standardized Coefficients
StandardCoefficients(LinearStepWiseModel)

#Testing of model conditions
vif(LinearStepWiseModel)
bptest(LinearStepWiseModel)
bgtest(LinearStepWiseModel)
#plot(LinearStepWiseModel)
#Overall the test indicate the the conditions are met.
#The residuals seem to be heteroskedastic, but the plot seems to be ok. 

#Adjusted R Squared 
StepwiseLinearTrain <- AdjustedR(LinearStepWiseModel, train)
StepwiseLinearTest <- AdjustedR(LinearStepWiseModel, test)

#Adjusted the Linear Model to ensure that coefficients can be explained.
#For example, the addition of a feature should lead to a positive contribution in the car price, not negative. 
#Features that resulted in a lower price were removed from the model. 
ManualAdjustedLinear <- lm(formula = CarPrice ~ Brand + Enginepower + CarType + Year + 
                             FuelType  + Navigation  + Drivetrain + Miles + ParkingSensors + 
                             PowerSteering + Transmission + ReversingCamera + TCS + AirConditioning + 
                             TyrePressureMonitoringSystem + CruiseControl, data = train)

#Model Results
summary(ManualAdjustedLinear)

#Standardized Coefficients
StandardCoefficients(ManualAdjustedLinear)

#Adjusted R Squared 
ManAdjustedLinearTrain <- AdjustedR(ManualAdjustedLinear, train)
ManAdjustedLinearTest <- AdjustedR(ManualAdjustedLinear, test)

rm(model.empty)
rm(model.full)
rm(scope)
rm(LinearStepWiseModel)

##########################################################################################################################################

##########################################################################################################################################
########################################################### Box Cox Transformation #######################################################

#Determining Lambda based on the Box Cox transformation
BoxCoxTransformation <- boxCox(ManualAdjustedLinear)
lambda = BoxCoxTransformation$x[which(BoxCoxTransformation$y == max(BoxCoxTransformation$y))]

#Creating a transformed dependent variable
CarDataSet$CarPriceBC = (CarDataSet$CarPrice^lambda - 1)/lambda

rm(train)
rm(test)

#Create a testing and training data set based on 5 folds random sampling and splitting the dataset 80% train, 20% test. 
set.seed(0)
folds = createFolds(CarDataSet$CarPrice, 5)
test = CarDataSet[folds[[1]], ]
train = CarDataSet[-folds[[1]], ]
rm(folds)

#Removing the Car Name Varialbe
train <- train[1:nrow(train),-1]

#Estimating the Box Cox tranformed model
BoxCoxLinear <- lm(formula = CarPriceBC ~ Brand + Enginepower + CarType + Year + 
                             FuelType  + Navigation + Topspeed + 
                             Drivetrain + NrSeats + Miles  + ParkingSensors + 
                             PowerSteering + Transmission + ReversingCamera + TCS , data = train)

#Model Results
summary(BoxCoxLinear)

#Standardized Coefficients
StandardCoefficients(BoxCoxLinear)

#Testing of model conditions
vif(BoxCoxLinear)
bptest(BoxCoxLinear)
bgtest(BoxCoxLinear)
#plot(BoxCoxLinear)

#Adjusted R Squared 
Prediction = predict(BoxCoxLinear, newdata = train)
TSS <- sum((train$CarPriceBC - mean(train$CarPriceBC, na.rm =  TRUE))^2)
RSS <- sum((Prediction - train$CarPriceBC)^2, na.rm = TRUE)
ModelBoxCoxTrain <- 1-((RSS/(length(train$CarPriceBC)-length(summary(BoxCoxLinear)$coefficients)-1))/(TSS/(length(train$CarPriceBC)-1)))

Prediction = predict(BoxCoxLinear, newdata = test)
TSS <- sum((test$CarPriceBC - mean(test$CarPriceBC, na.rm =  TRUE))^2)
RSS <- sum((Prediction - test$CarPriceBC)^2, na.rm = TRUE)
ModelBoxCoxTest <- 1-((RSS/(length(test$CarPriceBC)-length(summary(BoxCoxLinear)$coefficients)-1))/(TSS/(length(test$CarPriceBC)-1)))

rm(train)
rm(test)
rm(BoxCoxLinear)
rm(lambda)
rm(BoxCoxTransformation)
rm(ManualAdjustedLinear)
rm(PlotLocation)
CarDataSet$CarPriceBC <- NULL
rm(TSS)
rm(RSS)
rm(Prediction)

##########################################################################################################################################

##########################################################################################################################################
################################################################### GLMNET ###############################################################

#Create Train and Testing data set
set.seed(0)
folds = createFolds(CarDataSet$CarPrice, 5)
test = CarDataSet[folds[[1]], ]
train = CarDataSet[-folds[[1]], ]
train$CarName <- NULL
rm(folds)

#Running Eleastic Net grid search for Lasso/Ridge Regression with different alpha and lambda with 10 folds cross validation
#This allows it to find the best combination between Lasso/Ridge regrssion and the best value
#to minimize the AIC for while minimizing the MSE and the number of residuals.
myControl <- trainControl(method = "cv", number = 10)
myGrid <- expand.grid(alpha = seq(0.0001, 1, length = 20), lambda = seq(0.0001, 1, length = 20))
model <- train(CarPrice ~., data = train, preProcess = c("center", "scale"), method = "glmnet", tuneGrid = myGrid, trControl = myControl)

#Plot Results of GLMNET
plot(model)
plot(model$finalModel)

#Model Results
predict(model$finalModel, type = 'coefficients')

#Model Fit 
GLMNETtrain <- RSquare(model, train)
GLMNETtest <- RSquare(model, test)

rm(test)
rm(train)
rm(myControl)
rm(myGrid)
rm(model)

##########################################################################################################################################

##########################################################################################################################################
################################################################ Tree Models #############################################################

set.seed(12345)
CarDataSet1 = CarDataSet[, -which(names(CarDataSet) %in% c("CarName"))]
CarDataSet1$StartStopSystem <- NULL
folds = createFolds(CarDataSet1$Brand, 5)
test = CarDataSet1[folds[[1]], ]
train = CarDataSet1[-folds[[1]], ]
rm(folds)

BoostingModel1 = gbm(CarPrice ~ ., data = train,
             distribution = "gaussian",
             n.trees = 10000,
             interaction.depth = 4)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(BoostingModel1)

# Let's make a prediction on the test set. With boosting, the number of trees is
# a tuning parameter; having too many can cause overfitting. In general, we should
# use cross validation to select the number of trees. Instead, we will compute the
# test error as a function of the number of trees and make a plot for illustrative
# purposes.
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(BoostingModel1, newdata = test, n.trees = n.trees)
dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
berr = with(train, apply((predmat - test$CarPrice)^2, 2, mean))
plot(n.trees, berr, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
BoostingModel2 = gbm(CarPrice ~ ., data = train,
             distribution = "gaussian",
             n.trees = 10000,
             interaction.depth = 4,
             shrinkage = 0.1)

#Predicting the car price by means of the gbm model
n.trees2 = seq(from = 100, to = 7000, by = 100)
predmat2 = predict(BoostingModel2, newdata = test, n.trees = n.trees2)

berr2 = with(train, apply((predmat2 - test$CarPrice)^2, 2, mean))
plot(n.trees2, berr2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Model Fit 
Prediction = predict(BoostingModel2, newdata = train, n.trees = 4000)
TSS <- sum((train$CarPrice - mean(train$CarPrice, na.rm =  TRUE))^2)
RSS <- sum((Prediction - train$CarPrice)^2, na.rm = TRUE)
BoostingTrain <- 1-RSS/TSS

Prediction = predict(BoostingModel2, newdata = test, n.trees = 4000)
TSS <- sum((test$CarPrice - mean(test$CarPrice, na.rm =  TRUE))^2)
RSS <- sum((Prediction - test$CarPrice)^2, na.rm = TRUE)
BoostingTest <- 1-RSS/TSS

rm(BoostingModel1)
rm(BoostingModel2)
rm(test)
rm(train)
rm(n.trees)
rm(RSS)
rm(TSS)
rm(berr2)
rm(Prediction)
rm(n.trees2)
rm(PlotLocation)
rm(predmat)
rm(predmat2)
rm(CarDataSet1)
rm(berr)

##########################################################################################################################################

################################################################## Results ###############################################################

results <- data.frame(Model = c("Linear", "Stepwise Linear", "Box Cox","GLMNet", "GBM"),
                      Train_Results = c(ManAdjustedLinearTrain, StepwiseLinearTrain, ModelBoxCoxTrain, GLMNETtrain, BoostingTrain),
                      Test_Results = c(ManAdjustedLinearTest, StepwiseLinearTest, ModelBoxCoxTest, GLMNETtest, BoostingTest))
results
