library(dplyr)
library(arules)
library(recommenderlab)
library(tidyr)

#Read in Processed Data from the RecommendData.csv
RecommendData <- read.csv("C:/Users/Steven Jongerden/Desktop/CapStone Project/RecommendData.csv")

#Transform the CarName to the car brand and the model name to create a more specific prediction
Data <- data.frame(CarName = RecommendData$CarName)
CarSplit <- separate (Data, CarName, paste0("X",1:10), sep = " ")
RecommendData$BrandModel <- paste(CarSplit$X1, CarSplit$X2)

#Select only the rows that will be used in the recommendation engine.
RecommendData <- dplyr::select(RecommendData, BrandModel, CarPrice, CarType, Transmission, FuelType, Enginepower, Drivetrain, CruiseControl)

#Transform numeric variables to intervals as the recommenderlab only works with binary groups. 
RecommendData$CarPrice <- cut(RecommendData$CarPrice, breaks=c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, Inf), labels=paste(1:8))
RecommendData$Enginepower <- cut(RecommendData$Enginepower , breaks=c(25, 50, 75, 100, 125, 150, 175, 200), labels=paste(1:7))

#Create A binary Matrix from the groups in order to ensure the data can be used by the recommenderlab 
CarPrice <-data.frame(model.matrix(~ . + 0, data=data.frame(CarPrice = RecommendData$CarPrice), contrasts.arg = lapply(data.frame(CarPrice = RecommendData$CarPrice), contrasts, contrasts=FALSE)))
Enginepower <-data.frame(model.matrix(~ . + 0, data=data.frame(Enginepower = RecommendData$Enginepower), contrasts.arg = lapply(data.frame(Enginepower = RecommendData$Enginepower), contrasts, contrasts=FALSE)))
CarType <-data.frame(model.matrix(~ . + 0, data=data.frame(CarType = RecommendData$CarType), contrasts.arg = lapply(data.frame(CarType = RecommendData$CarType), contrasts, contrasts=FALSE)))
Transmission <-data.frame(model.matrix(~ . + 0, data=data.frame(Transmission = RecommendData$Transmission), contrasts.arg = lapply(data.frame(Transmission = RecommendData$Transmission), contrasts, contrasts=FALSE)))
FuelType <-data.frame(model.matrix(~ . + 0, data=data.frame(FuelType = RecommendData$FuelType), contrasts.arg = lapply(data.frame(FuelType = RecommendData$FuelType), contrasts, contrasts=FALSE)))
Drivetrain <-data.frame(model.matrix(~ . + 0, data=data.frame(Drivetrain = RecommendData$Drivetrain), contrasts.arg = lapply(data.frame(Drivetrain = RecommendData$Drivetrain), contrasts, contrasts=FALSE)))

#Change the column names of the two created binary groups for the car price and the engine power. 
colnames(CarPrice) <- c("Price 0-5000","Price 5000-10000","Price 10000-15000","Price 15000-20000","Price 20000-25000",
                        "Price 25000-30000","Price 30000-35000","Price 35000+")
colnames(Enginepower) <- c("25 - 50 bph", "50 - 75 bph", "75 - 100 bph", "100 - 125 bph", "125 - 150 bph", "150 - 175 bph", "175 - 200 bph")

#Overwrite the old recommendata with the processed data set. 
RecommendData <- cbind(RecommendData$BrandModel, CarPrice, Enginepower, CarType, Transmission, FuelType, Drivetrain)

#Remove used variables to save memory
rm(CarPrice)
rm(Enginepower)
rm(CarType)
rm(Transmission)
rm(FuelType)
rm(Drivetrain)

#Transform the columns to factors
for (i in 1:length(colnames(RecommendData))) {
  RecommendData[,i] <- factor(RecommendData[[i]])
}

##############################################################################################
#Create a case to predict for! By changing these values a new rcommendation will be generated. 
##############################################################################################
RecommendData[1,] <- NA

#Set Price
RecommendData[1,"Price 0-5000"] <- 0
RecommendData[1,"Price 5000-10000"] <- 0
RecommendData[1,"Price 10000-15000"] <- 0
RecommendData[1,"Price 15000-20000"] <- 0
RecommendData[1,"Price 20000-25000"] <- 0
RecommendData[1,"Price 25000-30000"] <- 1
RecommendData[1,"Price 30000-35000"] <- 0
RecommendData[1,"Price 35000+"] <- 0

#Set Engine Power
RecommendData[1,"25 - 50 bph"] <- 0
RecommendData[1,"50 - 75 bph"] <- 0
RecommendData[1,"75 - 100 bph"] <- 0
RecommendData[1,"100 - 125 bph"] <- 0
RecommendData[1,"125 - 150 bph"] <- 0
RecommendData[1,"150 - 175 bph"] <- 1
RecommendData[1,"175 - 200 bph"] <- 0

#Set Car Type
RecommendData[1,"CarTypeConvertible"] <- 0
RecommendData[1,"CarTypeCoupe"] <- 0
RecommendData[1,"CarTypeEstate"] <- 1
RecommendData[1,"CarTypeHatchback"] <- 0
RecommendData[1,"CarTypeMPV"] <- 0
RecommendData[1,"CarTypeOthers"] <- 0
RecommendData[1,"CarTypeSaloon"] <- 0
RecommendData[1,"CarTypeSUV"] <- 0

#Select Transmission
RecommendData[1,"TransmissionAutomatic"] <- 1
RecommendData[1,"TransmissionManual"] <- 0

#Select Fuel Type
RecommendData[1,"FuelTypeDiesel"] <- 0
RecommendData[1,"FuelTypeElectric.Hybird"] <- 0
RecommendData[1,"FuelTypePetrol"] <- 1

#Select Drive
RecommendData[1,"DrivetrainFour.Wheel.Drive"] <- 1
RecommendData[1,"DrivetrainFront.Wheel.Drive"] <- 0
RecommendData[1,"DrivetrainRear.Wheel.Drive"] <- 0

##############################################################################################

#Transform the dataframe to a binaryRatingMatrix
RecommendationEngine <- as(RecommendData, "transactions") 
RecommendationEngine <- new("binaryRatingMatrix", data = RecommendationEngine )

#Train the Recommender
rec <- Recommender(RecommendationEngine, method = "UBCF")

#Recommend fof the first user, which is the changed case
pre <- predict(rec, RecommendationEngine[1], n = 100)
prediction <- data.frame(as(pre, "list"))
prediction <- filter(prediction, grepl("RecommendData",prediction$X1))
prediction <-separate (prediction, X1, paste0("X",1:2), sep = "=")

#Results from the recommendation
prediction[1:8, "X2"]












