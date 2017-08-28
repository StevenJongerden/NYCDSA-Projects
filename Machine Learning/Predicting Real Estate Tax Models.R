#######################################################################################################################################################################################
#######################################################################################################################################################################################
######################################################### Machine Learning Assignment for NYC Data Science Academy ####################################################################
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#Loading packages
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tabplot)
library(corrplot)
library(caret)
library(car)
library(lmtest)
library(tree)
library(randomForest)
library(glmnet)
library(gbm)

#######################################################################################################################################################################################
############################################################################### Opening Data sets #################################################################################### 
#######################################################################################################################################################################################

#Open Data Set ##Set your file location of the properties data set##
dataset <- fread("properties_2016.csv")
soldhouses <- fread("train_2016_v2.csv", header = TRUE)
dataset <- as.data.frame(left_join(soldhouses, dataset, by ="parcelid"))

#Missing Information In Data Set
MissingValues <- data.frame(missing = sapply(dataset, function(x) round(sum(is.na(x))/nrow(dataset),4)))
MissingValues$variable <- rownames(MissingValues)
MissingValues <- arrange(MissingValues, desc(missing)) %>% select(variable, missing)
ggplot(MissingValues, aes(x=reorder(variable, missing), y = missing))+geom_bar(stat = "identity") + coord_flip() +
  ylab("Percentage Missing") + xlab("Variable") + ggtitle("Percentage of missing in variables")

#######################################################################################################################################################################################
################################################################################ Data Cleaning ######################################################################################## 
#######################################################################################################################################################################################

#Remove completely missing rows from the dataset
dataset <- dataset[!is.na(dataset$regionidcounty),]
dataset <- dataset[dataset$regionidcounty==3101,]

#Remove rows that have empty taxvaluedollarcnt as these cannot be predicted
dataset <- dataset[!is.na(dataset$taxvaluedollarcnt),]

#Imputate missing values that cannot be imputated otherwise
dataset[is.na(dataset$fireplaceflag),"fireplaceflag"] <- 0
dataset[is.na(dataset$fullbathcnt),"fullbathcnt"] <- 0
dataset[is.na(dataset$garagecarcnt),"garagecarcnt"] <- 0
dataset[is.na(dataset$garagetotalsqft),"garagetotalsqft"] <- 0
dataset[is.na(dataset$hashottuborspa),"hashottuborspa"] <- 0
dataset[is.na(dataset$heatingorsystemtypeid),"heatingorsystemtypeid"] <- 0
dataset[is.na(dataset$latitude),"latitude"] <- 0
dataset[is.na(dataset$longitude),"longitude"] <- 0
dataset[is.na(dataset$lotsizesquarefeet),"lotsizesquarefeet"] <- 0
dataset[is.na(dataset$numberofstories),"numberofstories"] <- 0
dataset[is.na(dataset$poolcnt),"poolcnt"] <- 0
dataset[is.na(dataset$poolsizesum),"poolsizesum"] <- 0
dataset[is.na(dataset$pooltypeid10),"pooltypeid10"] <- 0
dataset[is.na(dataset$pooltypeid2),"pooltypeid2"] <- 0
dataset[is.na(dataset$pooltypeid7),"pooltypeid7"] <- 0
dataset[is.na(dataset$propertycountylandusecode),"propertycountylandusecode"] <- 0
dataset[is.na(dataset$propertylandusetypeid),"propertylandusetypeid"] <- 0
dataset[is.na(dataset$propertyzoningdesc),"propertyzoningdesc"] <- "Other"
dataset[is.na(dataset$roomcnt),"roomcnt"] <- 0
dataset[is.na(dataset$storytypeid),"storytypeid"] <- 0
dataset[is.na(dataset$typeconstructiontypeid),"typeconstructiontypeid"] <- 0
dataset[is.na(dataset$yardbuildingsqft17),"yardbuildingsqft17"] <- 0
dataset[is.na(dataset$yardbuildingsqft26),"yardbuildingsqft26"] <- 0
dataset[is.na(dataset$fireplacecnt),"fireplacecnt"] <- 0
dataset[is.na(dataset$airconditioningtypeid),"airconditioningtypeid"] <- 0
dataset[is.na(dataset$architecturalstyletypeid),"architecturalstyletypeid"] <- 0
dataset[is.na(dataset$basementsqft),"basementsqft"] <- 0
dataset[is.na(dataset$bathroomcnt),"bathroomcnt"] <- 0
dataset[is.na(dataset$bedroomcnt),"bedroomcnt"] <- 0
dataset[is.na(dataset$buildingqualitytypeid),"buildingqualitytypeid"] <- 0
dataset[is.na(dataset$buildingclasstypeid),"buildingclasstypeid"] <- 0
dataset[is.na(dataset$calculatedbathnbr),"calculatedbathnbr"] <- 0
dataset[is.na(dataset$decktypeid),"decktypeid"] <- 0
dataset[is.na(dataset$threequarterbathnbr),"threequarterbathnbr"] <- 0
dataset[is.na(dataset$finishedfloor1squarefeet),"finishedfloor1squarefeet"] <- 0
dataset[is.na(dataset$calculatedfinishedsquarefeet),"calculatedfinishedsquarefeet"] <- 0
dataset[is.na(dataset$finishedsquarefeet6),"finishedsquarefeet6"] <- 0
dataset[is.na(dataset$finishedsquarefeet12),"finishedsquarefeet12"] <- 0
dataset[is.na(dataset$finishedsquarefeet13),"finishedsquarefeet13"] <- 0
dataset[is.na(dataset$finishedsquarefeet15),"finishedsquarefeet15"] <- 0
dataset[is.na(dataset$finishedsquarefeet50),"finishedsquarefeet50"] <- 0
dataset[is.na(dataset$unitcnt),"unitcnt"] <- 0
dataset$hashottuborspa <- ifelse(dataset$hashottuborspa == "true", 1, 0)
dataset$taxdelinquencyflag <- ifelse(dataset$taxdelinquencyflag == "Y", 1, 0)
dataset[is.na(dataset$taxdelinquencyyear),"taxdelinquencyyear"] <- 0

#Imputating missing variables 
dataset[is.na(dataset$yearbuilt), "yearbuilt"] <- mean(dataset$yearbuilt, na.rm = TRUE)
dataset[is.na(dataset$taxamount), "taxamount"] <- 0.02001632 * dataset[is.na(dataset$taxamount), "taxvaluedollarcnt"]
dataset[is.na(dataset$structuretaxvaluedollarcnt),"structuretaxvaluedollarcnt"] <- dataset[is.na(dataset$structuretaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$structuretaxvaluedollarcnt),"landtaxvaluedollarcnt"]
dataset[is.na(dataset$landtaxvaluedollarcnt),"landtaxvaluedollarcnt"] <- dataset[is.na(dataset$landtaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$landtaxvaluedollarcnt),"structuretaxvaluedollarcnt"]

#Recoding variables from numbers to factors 
dataset[dataset$airconditioningtypeid==0,"airconditioningtypeid"] <- "None"
dataset[dataset$airconditioningtypeid==1,"airconditioningtypeid"] <- "Central"
dataset[dataset$airconditioningtypeid==9,"airconditioningtypeid"] <- "Central"
dataset[dataset$airconditioningtypeid==13,"airconditioningtypeid"] <- "Central"
dataset[dataset$heatingorsystemtypeid==2,"heatingorsystemtypeid"] <- "Central"
dataset[dataset$heatingorsystemtypeid==7,"heatingorsystemtypeid"] <- "Floor"
dataset[dataset$heatingorsystemtypeid==0,"heatingorsystemtypeid"] <- "Other"
dataset[dataset$heatingorsystemtypeid==20,"heatingorsystemtypeid"] <- "Other"
dataset[dataset$propertylandusetypeid==31, "propertylandusetypeid"] <-"Commercial/Office/Residential Mixed Used"
dataset[dataset$propertylandusetypeid==47, "propertylandusetypeid"] <-"Store/Office (Mixed Use)"
dataset[dataset$propertylandusetypeid==246, "propertylandusetypeid"] <-"Duplex"
dataset[dataset$propertylandusetypeid==247, "propertylandusetypeid"] <-"Triplex"
dataset[dataset$propertylandusetypeid==248, "propertylandusetypeid"] <-"Quadruplex"
dataset[dataset$propertylandusetypeid==260, "propertylandusetypeid"] <-"Residential General"
dataset[dataset$propertylandusetypeid==261, "propertylandusetypeid"] <-"Single Family Residential"
dataset[dataset$propertylandusetypeid==263, "propertylandusetypeid"] <-"Mobile Home"
dataset[dataset$propertylandusetypeid==264, "propertylandusetypeid"] <-"Townhouse"
dataset[dataset$propertylandusetypeid==266, "propertylandusetypeid"] <-"Condominium"
dataset[dataset$propertylandusetypeid==267, "propertylandusetypeid"] <-"Cooperative"
dataset[dataset$propertylandusetypeid==269, "propertylandusetypeid"] <-"Planned Unit Development"
dataset$propertyzoningdesc = as.character(dataset$propertyzoningdesc)
dataset$propertyzoningdesc = factor(dataset$propertyzoningdesc)
dataset$regionidcounty <- factor(dataset$regionidcounty)
dataset$regionidcity <- factor(dataset$regionidcity)
dataset$regionidzip <- factor(dataset$regionidzip)
dataset$regionidneighborhood <- factor(dataset$regionidneighborhood)
dataset$airconditioningtypeid <- factor(dataset$airconditioningtypeid)
dataset$architecturalstyletypeid <- factor(dataset$architecturalstyletypeid)
dataset$buildingclasstypeid <- factor(dataset$buildingclasstypeid)
dataset$decktypeid <- factor(dataset$decktypeid)
dataset$heatingorsystemtypeid <- factor(dataset$heatingorsystemtypeid)
dataset$pooltypeid10 <- factor(dataset$pooltypeid10)
dataset$pooltypeid2 <- factor(dataset$pooltypeid2)
dataset$pooltypeid7 <- factor(dataset$pooltypeid7)
dataset$storytypeid <- factor(dataset$storytypeid)
dataset$typeconstructiontypeid <- factor(dataset$typeconstructiontypeid)
dataset$transactiondate <- base::as.Date(dataset$transactiondate)
dataset$hashottuborspa <- factor(dataset$hashottuborspa)
dataset$propertycountylandusecode <- factor(dataset$propertycountylandusecode)
dataset$taxdelinquencyflag <- factor(dataset$taxdelinquencyflag)
dataset$airconditioningtypeid <- as.character(dataset$airconditioningtypeid)
dataset$heatingorsystemtypeid <- as.character(dataset$heatingorsystemtypeid)
dataset$heatingorsystemtypeid <- factor(dataset$heatingorsystemtypeid)
dataset$airconditioningtypeid <- factor(dataset$airconditioningtypeid)
dataset$propertylandusetypeid <- factor(dataset$propertylandusetypeid)
dataset <- dataset[!is.na(dataset$structuretaxvaluedollarcnt),]
dataset <- dataset[!is.na(dataset$landtaxvaluedollarcnt),]

#Remove columns that are empty, and have no information
dataset$architecturalstyletypeid <- NULL
dataset$basementsqft<- NULL
dataset$decktypeid<- NULL
dataset$finishedfloor1squarefeet<- NULL
dataset$finishedsquarefeet13<- NULL
dataset$finishedsquarefeet50<- NULL
dataset$finishedsquarefeet6<- NULL
dataset$fireplacecnt<- NULL
dataset$garagecarcnt<- NULL
dataset$poolsizesum<- NULL
dataset$pooltypeid2<- NULL
dataset$regionidcity<- NULL
dataset$regionidneighborhood<- NULL
dataset$roomcnt<- NULL
dataset$storytypeid<- NULL
dataset$threequarterbathnbr<- NULL
dataset$typeconstructiontypeid<- NULL
dataset$yardbuildingsqft17<- NULL
dataset$yardbuildingsqft26<- NULL
dataset$fips <-NULL
dataset$regionidcounty <- NULL
dataset$fireplaceflag <- NULL
dataset$propertyzoningdesc <- NULL

#Create housing dataset on rule roomtcount >0
housingdataset <- dataset[dataset$structuretaxvaluedollarcnt!=0,]
percentagehous <- data.frame(lapply(housingdataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

rm(percentagehous)
rm(dataset)
rm(soldhouses)
rm(MissingValues)

#######################################################################################################################################################################################
##################################################################################### Clustering####################################################################################### 
#######################################################################################################################################################################################

num <- sapply(housingdataset, function(x) is.numeric(x))
housingdataset.scale = as.data.frame(scale(housingdataset[,num]))
housingdataset.scale$garagetotalsqft <- NULL
housingdataset.scale$assessmentyear <- NULL
housingdataset.scale$censustractandblock <- NULL

km.test = kmeans(housingdataset.scale, centers = 8)
housingdataset$Clustering <- km.test$cluster
housingdataset$Clustering <- factor(housingdataset$Clustering)

#######################################################################################################################################################################################
##################################################################################### EDA ############################################################################################# 
#######################################################################################################################################################################################

#Investigate Level of variance 
nums <- sapply(housingdataset, is.numeric)
Variance <- data.frame(variance = sapply(housingdataset[,nums], function(x) round(sd(x),4)))
Variance$variable <- rownames(Variance)
Variance <- arrange(Variance, desc(variance)) %>% select(variable, variance)
Variance1 <- Variance[1:12,]
Variance2 <- Variance[13:26,]

ggplot(Variance1, aes(x=reorder(variable, variance), y = variance))+geom_bar(stat = "identity") + coord_flip() +
  ylab("Variance") + xlab("Variable") + ggtitle("Level of Variance")

ggplot(Variance2, aes(x=reorder(variable, variance), y = variance))+geom_bar(stat = "identity") + coord_flip() +
  ylab("Variance") + xlab("Variable") + ggtitle("Level of Variance")

#Create tableplot with all the variables for landtaxvaluedollarcnt
colMtx <- matrix(names(housingdataset)[1:length(housingdataset)-1], nrow = 3)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdataset, 
            select_string = c(colMtx[,i], "landtaxvaluedollarcnt"), 
            sortCol = "landtaxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create tableplot with all the variables for structuretaxvaluedollarcnt
colMtx <- matrix(names(housingdataset)[1:length(housingdataset)-1], nrow = 3)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdataset, 
            select_string = c(colMtx[,i], "structuretaxvaluedollarcnt"), 
            sortCol = "structuretaxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create corplots with all the numeric variables for landtaxvaluedollarcnt
numeric_features <- names(housingdataset)[sapply(housingdataset, is.numeric)]
corHousingLandTax <- cor(housingdataset %>% select(one_of(numeric_features, "landtaxvaluedollarcnt")), method = "pearson", use = "pairwise.complete.obs")
corHousingLandTax[is.na(corHousingLandTax)] = 0
corrplot(corHousingLandTax, method = "color", order="hclust")

#Create corplots with all the categorical variables for landtaxvaluedollarcnt
#Minimized to save computation time. 
housingdataset2 <- head(housingdataset, 10000)
ordinal_features <- c('airconditioningtypeid', 'heatingorsystemtypeid','pooltypeid10', 'pooltypeid7', 'propertylandusetypeid','regionidzip')
corHousingLandTax2 <- cor(data.matrix(housingdataset2 %>% select(one_of(ordinal_features, "landtaxvaluedollarcnt"))), method = "kendall", use = "pairwise.complete.obs")
corrplot(corHousingLandTax2, method = "color", order="hclust")

ordinal_features <- c('airconditioningtypeid', 'buildingqualitytypeid','buildingclasstypeid', 'heatingorsystemtypeid','pooltypeid10', 'pooltypeid7', 'propertylandusetypeid','regionidzip')
corHousinghouseTax2 <- cor(data.matrix(housingdataset %>% select(one_of(ordinal_features, "structuretaxvaluedollarcnt"))), method = "kendall", use = "pairwise.complete.obs")
corrplot(corHousingLandTax2, method = "color", order="hclust")

#######################################################################################################################################################################################
############################################################################# Hypotheses Testing ###################################################################################### 
#######################################################################################################################################################################################

### EDA for model as proof for hypothesized relationship. 
ggplot(housingdataset, aes(airconditioningtypeid, log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Airconditioning Type")
t.test(housingdataset$structuretaxvaluedollarcnt ~ housingdataset$airconditioningtypeid)
#Significant Difference 
ggplot(housingdataset, aes(bathroomcnt, log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Bathroom count")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$bathroomcnt)
#Significant relation 0.59
ggplot(housingdataset, aes(bedroomcnt, log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Bedroomcnt count")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$bedroomcnt)
#Significant relation 0.31
ggplot(housingdataset, aes(factor(buildingqualitytypeid), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Building Qualitytype")
cor.test(housingdataset$structuretaxvaluedollarcnt, housingdataset$buildingqualitytypeid)
#Significant, however, inverse as expected relationship
ggplot(housingdataset, aes(log(calculatedfinishedsquarefeet), log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Size of the house")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$calculatedfinishedsquarefeet)
#Significant relationship 0.58
ggplot(housingdataset, aes(factor(heatingorsystemtypeid), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Heating System")
kruskal.test(structuretaxvaluedollarcnt ~ heatingorsystemtypeid, data = housingdataset) 

ggplot(housingdataset, aes(factor(poolcnt), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Number of Pools")
t.test(housingdataset$structuretaxvaluedollarcnt ~ housingdataset$poolcnt)
#Significant Difference 
ggplot(housingdataset, aes(factor(yearbuilt), log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Year Build")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$yearbuilt)
#Significant relationship 0.41
ggplot(housingdataset, aes(factor(numberofstories), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Number of floors")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$numberofstories)
#Significant relationship 0.009
ggplot(housingdataset, aes(factor(propertylandusetypeid), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Housing Type")
kruskal.test(structuretaxvaluedollarcnt ~ propertylandusetypeid, data = housingdataset) 
#Significance 

ggplot(housingdataset, aes(unitcnt, log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("unitcnt")
housingdataset[housingdataset$unitcnt >50, "unitcnt"] <- 1
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$unitcnt)

#New Group Cluster
ggplot(housingdataset, aes(Clustering, log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Clusters")
ggplot(housingdataset, aes(log(calculatedfinishedsquarefeet), log(structuretaxvaluedollarcnt), color=Clustering))+geom_point()+ggtitle("Clusters") + facet_grid(. ~ Clustering)

#######################################################################################################################################################################################
############################################################################### Linear Regression ##################################################################################### 
#######################################################################################################################################################################################

set.seed(0)
folds = createFolds(housingdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]

#Model1 House tax 
model <- lm(structuretaxvaluedollarcnt~airconditioningtypeid + bathroomcnt + bedroomcnt + 
              calculatedfinishedsquarefeet + heatingorsystemtypeid + poolcnt + yearbuilt + 
              + propertylandusetypeid + Clustering, data = train)

TSS <- sum((test$structuretaxvaluedollarcnt - mean(test$structuretaxvaluedollarcnt, na.rm =  TRUE))^2)
summary(model)
vif(model)
BIC(model)
bptest(model)
bgtest(model)
predictedmodel1 = predict(model, newdata = test)
MSEModel1 <- mean((predictedmodel1 - test$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
RSSModel1 <- sum((predictedmodel1 - test$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model1 <- 1-((RSSModel1/(length(test$structuretaxvaluedollarcnt)-length(summary(model)$coefficients)-1))/(TSS/(length(test$structuretaxvaluedollarcnt)-1)))
AdjR2Model1

#Model2 House Tax
bc <- boxCox(model)
lambda = bc$x[which(bc$y == max(bc$y))]
structuretaxvaluedollarcnt.bc = (train$structuretaxvaluedollarcnt^lambda - 1)/lambda
model2 <- lm(structuretaxvaluedollarcnt.bc~airconditioningtypeid + bathroomcnt + bedroomcnt + 
               calculatedfinishedsquarefeet + heatingorsystemtypeid + poolcnt + yearbuilt  + 
               propertylandusetypeid  + Clustering , data = train)

TSS <- sum((test$structuretaxvaluedollarcnt - mean(test$structuretaxvaluedollarcnt, na.rm =  TRUE))^2)
summary(model2)
vif(model2)
BIC(model2)
bptest(model2)
bgtest(model2)
predictedmodel2 = ((predict(model2, newdata = test)*lambda)+1)^(1/lambda)
MSEModel2 <- mean((predictedmodel2 - test$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
RSSModel2 <- sum((predictedmodel2 - test$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model2 <- 1-((RSSModel2/(length(test$structuretaxvaluedollarcnt)-length(summary(model2)$coefficients)-1))/(TSS/(length(test$structuretaxvaluedollarcnt)-1)))
AdjR2Model2

#Model 3
train$landtaxvaluedollarcnt <- NULL
train$taxamount <- NULL
train$structuretaxvaluedollarcnt <- NULL
train$taxvaluedollarcnt <- NULL
train$logerror <- NULL
train$censustractandblock <- NULL

#library(MASS)
#model.empty = lm(structuretaxvaluedollarcnt.bc ~ 1, data = train)
#model.full = lm(structuretaxvaluedollarcnt.bc ~ ., data = train)
#scope = list(lower = formula(model.empty), upper = formula(model.full))
#model3 = step(model2, scope, direction = "both", k = 2)


#best model based on earlier estimation
model3 <- lm(structuretaxvaluedollarcnt.bc~airconditioningtypeid + bathroomcnt + calculatedfinishedsquarefeet + heatingorsystemtypeid +
               poolcnt + yearbuilt  + finishedsquarefeet15  + buildingqualitytypeid+ 
               lotsizesquarefeet + rawcensustractandblock + longitude + latitude + taxdelinquencyyear + Clustering, data = train)
summary(model3)
vif(model3)
BIC(model3)
bptest(model3)
bgtest(model3)
predictedmodel3 = ((predict(model3, newdata = test)*lambda)+1)^(1/lambda)
MSEModel3 <- mean((predictedmodel3 - test$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
RSSModel3 <- sum((predictedmodel3 - test$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model3 <- 1-((RSSModel3/(length(test$structuretaxvaluedollarcnt)-length(summary(model3)$coefficients)-1))/(TSS/(length(test$structuretaxvaluedollarcnt)-1)))
AdjR2Model3

# #Correction for heteroskedasticity:
# library(RCurl)
# # import the function from repository
# url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
# eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
#      envir=.GlobalEnv)
# 
# modelresultsHouseTaxes <- summary(model3, robust=T)
# modelresultsHouseTaxes
##### END MODEL BUILDING HOUSE TAXES ####

##### START MODEL BUILDING LAND TAXES #####
ggplot(housingdataset, aes(propertylandusetypeid, log(landtaxvaluedollarcnt)))+geom_boxplot()+ggtitle("propertylandusetypeid")
ggplot(housingdataset, aes(log(calculatedfinishedsquarefeet), log(landtaxvaluedollarcnt)))+geom_point()+ggtitle("calculatedfinishedsquarefeet")
cor.test(log(housingdataset$landtaxvaluedollarcnt), housingdataset$calculatedfinishedsquarefeet)
#Significant relationship 0.33

set.seed(0)
folds = createFolds(landdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]
TSS <- sum((test$landtaxvaluedollarcnt - mean(test$landtaxvaluedollarcnt, na.rm =  TRUE))^2)

#Model4 Land Tax
model4 <- lm(landtaxvaluedollarcnt ~ propertylandusetypeid + calculatedfinishedsquarefeet + Clustering, data = train)
summary(model4)
vif(model4)
BIC(model4)
bptest(model4)
bgtest(model4)
predictedmodel4 = predict(model4, newdata = test)
MSEModel4 <- mean((predictedmodel4 - test$landtaxvaluedollarcnt)^2)
RSSModel4 <- sum((predictedmodel4 - test$landtaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model4 <- 1-((RSSModel4/(length(test$landtaxvaluedollarcnt)-length(summary(model4)$coefficients)-1))/(TSS/(length(test$landtaxvaluedollarcnt)-1)))
AdjR2Model4

#Model5 Land Tax
bc2 <- boxCox(model4)
lambda2 = bc$x[which(bc2$y == max(bc2$y))]
landtaxvaluedollarcnt.bc = (train$landtaxvaluedollarcnt^lambda2 - 1)/lambda2

model5 <- lm(landtaxvaluedollarcnt.bc ~ propertylandusetypeid + calculatedfinishedsquarefeet + Clustering, data = train)
summary(model5)
vif(model5)
BIC(model5)
bptest(model5)
bgtest(model5)
predictedmodel5 = ((predict(model5, newdata = test)*lambda2)+1)^(1/lambda2)
MSEModel5 <- mean((predictedmodel5 - test$landtaxvaluedollarcnt)^2, na.rm = TRUE)
RSSModel5 <- sum((predictedmodel5 - test$landtaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model5 <- 1-((RSSModel5/(length(test$landtaxvaluedollarcnt)-length(summary(model5)$coefficients)-1))/(TSS/(length(test$landtaxvaluedollarcnt)-1)))
AdjR2Model5

# #Final Model Land Tax 
# modelresultsLandTaxes <- summary(model5, robust=T)
# modelresultsLandTaxes

#######################################################################################################################################################################################
############################################################################### LASSO Regression ###################################################################################### 
#######################################################################################################################################################################################

set.seed(0)
folds = createFolds(housingdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]

train$landtaxvaluedollarcnt <- NULL
train$taxamount <- NULL
train$taxvaluedollarcnt <- NULL
train$logerror <- NULL
train$censustractandblock <- NULL

#House Tax Estimation
train <-train[complete.cases(train),]
test <-test[complete.cases(test),]
TSS <- sum((train$structuretaxvaluedollarcnt - mean(train$structuretaxvaluedollarcnt, na.rm =  TRUE))^2)

x = model.matrix(structuretaxvaluedollarcnt ~ ., train)[, -1] 
y = train$structuretaxvaluedollarcnt
grid = 10^seq(5, -2, length = 100)
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)

plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")
cv.lasso.out = cv.glmnet(x, y, lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min

x = model.matrix(structuretaxvaluedollarcnt ~ ., train)[, -1] 
lassopredictmodel1 = predict(cv.lasso.out, s = bestlambda.lasso, newx = x)

MSEModel6 <- mean((lassopredictmodel1 - train$structuretaxvaluedollarcnt)^2)
RSSModel6 <- sum((lassopredictmodel1 - train$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model6 <- 1-(RSSModel6/TSS)
AdjR2Model6

#Land tax Estimation
folds = createFolds(housingdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]

train$structuretaxvaluedollarcnt <- NULL
train$taxamount <- NULL
train$taxvaluedollarcnt <- NULL
train$logerror <- NULL
train$censustractandblock <- NULL

train <-train[complete.cases(train),]
test <-test[complete.cases(test),]
TSS <- sum((train$landtaxvaluedollarcnt - mean(train$landtaxvaluedollarcnt, na.rm =  TRUE))^2)

x = model.matrix(landtaxvaluedollarcnt ~ ., train)[, -1] 
y = train$landtaxvaluedollarcnt
grid = 10^seq(5, -2, length = 100)
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)

plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")
cv.lasso.out = cv.glmnet(x, y, lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min

x = model.matrix(landtaxvaluedollarcnt ~ ., train)[, -1] 
lassopredictmodel2 = predict(cv.lasso.out, s = bestlambda.lasso, newx = x)

MSEModel7 <- mean((lassopredictmodel2 - train$landtaxvaluedollarcnt)^2)
RSSModel7 <- sum((lassopredictmodel2 - train$landtaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model7 <- 1-(RSSModel7/TSS)
AdjR2Model7

#######################################################################################################################################################################################
################################################################################ Random Forest ######################################################################################## 
#######################################################################################################################################################################################

#=========================
# Structure Tax 
#=========================


#### Extracting data set for Random Forest ######

# Extract housing sub
drop_col <- c('parcelid', 'logerror', 'transactiondate', 'hashottuborspa', 'propertyzoningdesc',
              'unitcnt', 'fireplaceflag', 'taxvaluedollarcnt', 'landtaxvaluedollarcnt', 'taxamount',
              'taxdelinquencyflag', 'taxdelinquencyyear', 'censustractandblock')


housing_sub <- housingdataset[ , -which(names(housingdataset) %in% drop_col)]


##Give engineering feature to 'regionidzip'

# 1. Group by regionidzip and mutate 

# >500	1
# 400 ~ 500	2
# 300~400	3
# 200 ~ 300	4
# 200	5

region_feature = housing_sub %>% 
  group_by(., regionidzip) %>%
  summarise(., population = n()) %>%
  arrange(., desc(population)) %>%
  mutate(., population_level = ifelse(population > 500, '1', ifelse(population > 400 & population < 500, '2', ifelse(population > 300 & population < 400, '3', ifelse(population > 200 & population < 300, '4', ifelse(population < 200, '5', 'NA'))))))

housing_sub <- merge(housing_sub, region_feature, by = 'regionidzip')

# Transfer population_level into factor
housing_sub$population_level = as.factor(housing_sub$population_level)


# Take out columns
drop_col <- c('transactionyear', "regionidzip",'population', 'propertycountylandusecode')
housing_sub <- housing_sub[ , -which(names(housing_sub) %in% drop_col)]


#Impute NA
housing_sub$yearbuilt[is.na(housing_sub$yearbuilt)] <- mean(housing_sub$yearbuilt, na.rm = TRUE)





#### Run Randomforest ######


#Set up trainig data set
folds = createFolds(housingdataset$parcelid, 5)
test = housing_sub[folds[[1]], ]
train = housing_sub[-folds[[1]], ]
train <-train[complete.cases(train),]



#Reduce sample size to reduce computation time.
trainForest <- train[1:10000,]
TSS <- sum((trainForest$structuretaxvaluedollarcnt - mean(trainForest$structuretaxvaluedollarcnt, na.rm =  TRUE))^2)



#Cross validation of the number of variables tried at each split
# R2ModelTree <- numeric(24)
# for (i in 1:24) {
#   fit = randomForest(structuretaxvaluedollarcnt ~ ., data = trainForest, mtry = i)
#   RSSRandom <- sum((fit$predicted - trainForest$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
#   R2Random <- 1 - RSSRandom/TSS
#   R2ModelTree[i] <- R2Random
#   cat("We're performing iteration", i,',','R2Random', R2ModelTree[i], "\n")
# }
# R2ModelTree
# plot(R2ModelTree, type = 'line')

# We're performing iteration 1 , R2Random 0.4919092 
# We're performing iteration 2 , R2Random 0.6877127 
# We're performing iteration 3 , R2Random 0.7165541 
# We're performing iteration 4 , R2Random 0.7171827 
# We're performing iteration 5 , R2Random 0.7189479 
# We're performing iteration 6 , R2Random 0.7098864 
# We're performing iteration 7 , R2Random 0.7119715 
# We're performing iteration 8 , R2Random 0.7103388 
# We're performing iteration 9 , R2Random 0.7132998 
# We're performing iteration 10 , R2Random 0.7029462 
# We're performing iteration 11 , R2Random 0.708106 
# We're performing iteration 12 , R2Random 0.7079995 
# We're performing iteration 13 , R2Random 0.7111537 
# We're performing iteration 14 , R2Random 0.7079781 
# We're performing iteration 15 , R2Random 0.7078888 
# We're performing iteration 16 , R2Random 0.7025998 
# We're performing iteration 17 , R2Random 0.7054904 
# We're performing iteration 18 , R2Random 0.7054278 
# We're performing iteration 19 , R2Random 0.6974952 
# We're performing iteration 20 , R2Random 0.7054704 
# We're performing iteration 21 , R2Random 0.697569 
# We're performing iteration 22 , R2Random 0.699348 
# We're performing iteration 23 , R2Random 0.7008241 
# We're performing iteration 24 , R2Random 0.6986068



# Result 4
fit2 = randomForest(structuretaxvaluedollarcnt ~ ., data = trainForest, ntree=100, mtry = 4)
plot(fit2)
Importance_structure <- data.frame(fit2$importance)
Importance_structure$variables <- rownames(fit2$importance)
arrange(Importance_structure, desc(IncNodePurity))

# IncNodePurity                    variables
# 1   1.682437e+14 calculatedfinishedsquarefeet
# 2   1.578341e+14         finishedsquarefeet12
# 3   1.241357e+14                   Clustering
# 4   6.491415e+13                  fullbathcnt
# 5   6.305624e+13            calculatedbathnbr
# 6   5.260954e+13                  bathroomcnt
# 7   4.624577e+13        buildingqualitytypeid
# 8   3.652348e+13                    yearbuilt
# 9   3.646627e+13                    longitude
# 10  2.986470e+13       rawcensustractandblock
# 11  2.894585e+13            lotsizesquarefeet
# 12  2.721532e+13                     latitude
# 13  2.251569e+13                   bedroomcnt
# 14  1.452765e+13        heatingorsystemtypeid
# 15  1.415855e+13             population_level
# 16  1.334030e+13        airconditioningtypeid
# 17  8.727491e+12        propertylandusetypeid
# 18  7.176848e+12                      poolcnt
# 19  5.603006e+12         finishedsquarefeet15
# 20  5.481410e+12                  pooltypeid7
# 21  1.427563e+12                 pooltypeid10
# 22  4.775033e+09              numberofstories
# 23  2.802091e+09          buildingclasstypeid
# 24  0.000000e+00              garagetotalsqft
# 25  0.000000e+00               assessmentyear



# Ploting Importances
arrange(Importance_structure, desc(IncNodePurity)) %>%
  filter(., IncNodePurity != 0) %>%
  ggplot(aes(y = log(IncNodePurity) , x = reorder(variables, IncNodePurity))) +
  geom_col( fill = c('skyblue')) + coord_flip() + ylab('IncNodePurity') + xlab('Variables')+
  ggtitle('Sturcture Tax randomforest Importance')


fit2
# Call:
#   randomForest(formula = structuretaxvaluedollarcnt ~ ., data = trainForest,      ntree = 100, mtry = 6) 
# Type of random forest: regression
# Number of trees: 100
# No. of variables tried at each split: 6
# 
# Mean of squared residuals: 33226149963
# % Var explained: 67.92





#=========================
# Land Tax 
#=========================


#### Extracting data set for Random Forest ######

# Extract landsataset_sub
drop_col <- c('parcelid', 'logerror', 'transactiondate', 'hashottuborspa', 'propertyzoningdesc',
              'unitcnt', 'fireplaceflag', 'taxvaluedollarcnt', 'structuretaxvaluedollarcnt', 'taxamount',
              'taxdelinquencyflag', 'taxdelinquencyyear', 'censustractandblock')


land_sub <- landdataset[ , -which(names(landdataset) %in% drop_col)]


##Give engineering feature to 'regionidzip'

region_feature = land_sub %>% 
  group_by(., regionidzip) %>%
  summarise(., population = n()) %>%
  arrange(., desc(population)) %>%
  mutate(., population_level = ifelse(population > 500, '1', ifelse(population > 400 & population < 500, '2', ifelse(population > 300 & population < 400, '3', ifelse(population > 200 & population < 300, '4', ifelse(population < 200, '5', 'NA'))))))

land_sub <- merge(land_sub, region_feature, by = 'regionidzip')

# Transfer population_level into factor
land_sub$population_level = as.factor(land_sub$population_level)


# Take out columns
drop_col <- c('transactionyear', "regionidzip",'population', 'propertycountylandusecode')
land_sub <- land_sub[ , -which(names(land_sub) %in% drop_col)]


#Impute NA
land_sub$yearbuilt[is.na(land_sub$yearbuilt)] <- mean(land_sub$yearbuilt, na.rm = TRUE)





#### Extracting data set for Random Forest ######


#Create sample dataset

folds = createFolds(landdataset$parcelid, 5)
test = land_sub[folds[[1]], ]
train = land_sub[-folds[[1]], ]

train <-train[complete.cases(train),]

#Reduce sample size to reduce computation time.
trainforest <- train[1:10000,]


#Cross validation of the number of variables tried at each split
# TSS <- sum((trainforest$landtaxvaluedollarcnt - mean(trainforest$landtaxvaluedollarcnt, na.rm =  TRUE))^2)
# R2ModelTree_land <- numeric(24)
# for (i in 1:24) {
#   fit = randomForest(landtaxvaluedollarcnt ~ ., data = trainforest, mtry = i)
#   RSSRandom <- sum((fit$predicted - trainforest$landtaxvaluedollarcnt)^2, na.rm = TRUE)
#   R2randomf_land  <- 1 - RSSRandom/TSS
#   R2ModelTree_land[i] <- R2randomf_land 
#   cat("We're performing iteration", i,',','R2random_land', R2ModelTree_land[i], "\n")
# }

# We're performing iteration 1 , R2random_land 0.3039953 
# We're performing iteration 2 , R2random_land 0.4636541 
# We're performing iteration 3 , R2random_land 0.4673539 
# We're performing iteration 4 , R2random_land 0.4668156 
# We're performing iteration 5 , R2random_land 0.4570625 
# We're performing iteration 6 , R2random_land 0.4586552 
# We're performing iteration 7 , R2random_land 0.4513764 
# We're performing iteration 8 , R2random_land 0.4448501 
# We're performing iteration 9 , R2random_land 0.4363606 
# We're performing iteration 10 , R2random_land 0.4420529 
# We're performing iteration 11 , R2random_land 0.4517525 
# We're performing iteration 12 , R2random_land 0.432252 
# We're performing iteration 13 , R2random_land 0.4369612 
# We're performing iteration 14 , R2random_land 0.4342191 
# We're performing iteration 15 , R2random_land 0.4272493 
# We're performing iteration 16 , R2random_land 0.4262058 
# We're performing iteration 17 , R2random_land 0.424455 
# We're performing iteration 18 , R2random_land 0.4308587 
# We're performing iteration 19 , R2random_land 0.4265588 
# We're performing iteration 20 , R2random_land 0.427004 
# We're performing iteration 21 , R2random_land 0.4233856 
# We're performing iteration 22 , R2random_land 0.4238724 
# We're performing iteration 23 , R2random_land 0.4211816 
# We're performing iteration 24 , R2random_land 0.4283781 



R2ModelTree_land
plot(R2ModelTree_land, type = 'line')


#Result 3 mtry

fit2 = randomForest(landtaxvaluedollarcnt ~ ., data = trainforest, ntree=100, mtry = 3)
plot(fit2)
Importance <- data.frame(fit2$importance)
Importance$variables <- rownames(Importance)

arrange(Importance, desc(IncNodePurity))


# IncNodePurity                    variables
# 1   4.981586e+14         finishedsquarefeet12
# 2   3.587071e+14 calculatedfinishedsquarefeet
# 3   2.413164e+14            calculatedbathnbr
# 4   2.238717e+14                     latitude
# 5   2.211429e+14                    longitude
# 6   1.864368e+14                  bathroomcnt
# 7   1.737103e+14                  fullbathcnt
# 8   1.666156e+14            lotsizesquarefeet
# 9   1.401025e+14                    yearbuilt
# 10  1.389370e+14       rawcensustractandblock
# 11  1.386354e+14        buildingqualitytypeid
# 12  7.909497e+13                   bedroomcnt
# 13  7.226142e+13             population_level
# 14  5.164224e+13        propertylandusetypeid
# 15  5.116017e+13        heatingorsystemtypeid
# 16  4.936830e+13                  pooltypeid7
# 17  4.184932e+13                      poolcnt
# 18  2.369594e+13        airconditioningtypeid
# 19  2.360564e+13         finishedsquarefeet15
# 20  2.286256e+12                 pooltypeid10
# 21  2.439305e+10              numberofstories
# 22  1.898407e+10          buildingclasstypeid
# 23  0.000000e+00              garagetotalsqft
# 24  0.000000e+00               assessmentyear


#Generate importance plot
arrange(Importance, desc(IncNodePurity)) %>%
  filter(., IncNodePurity != 0) %>%
  ggplot(aes(y = log(IncNodePurity) , x = reorder(variables, IncNodePurity))) +
  geom_col( fill = c('orange')) + coord_flip() + ylab('IncNodePurity') + xlab('Variables')+
  ggtitle('Land Tax RF Importance')

fit2

# Call:
#   randomForest(formula = landtaxvaluedollarcnt ~ ., data = trainforest,      ntree = 100, mtry = 3) 
# Type of random forest: regression
# Number of trees: 100
# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 174307313329
# % Var explained: 49.02


#######################################################################################################################################################################################
################################################################################## Boosting ########################################################################################### 
#######################################################################################################################################################################################

#Real Estate Taxes
folds = createFolds(housingdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]

TSS <- sum((train$structuretaxvaluedollarcnt - mean(train$structuretaxvaluedollarcnt, na.rm =  TRUE))^2)

Model10 = gbm(structuretaxvaluedollarcnt ~ 
                + finishedsquarefeet12 
              + calculatedfinishedsquarefeet 
              + buildingqualitytypeid 
              + yearbuilt + bathroomcnt 
              + lotsizesquarefeet 
              + bedroomcnt + propertylandusetypeid 
              + poolcnt + airconditioningtypeid 
              + pooltypeid7 + taxdelinquencyyear 
              + pooltypeid10 + hashottuborspa 
              + heatingorsystemtypeid 
              + finishedsquarefeet15 + unitcnt + Clustering, 
              data = train,
              distribution = "gaussian",
              n.trees = 5000,
              interaction.depth = 10, 
              shrinkage = 0.01)

RSSModel10 <- sum((predict(Model10, newdata = train, n.trees = 5000) - train$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model10 <- 1-(RSSModel10/TSS)
AdjR2Model10

TSStest <- sum((test$structuretaxvaluedollarcnt - mean(test$structuretaxvaluedollarcnt, na.rm =  TRUE))^2)
RSSModel10test <- sum((predict(Model10, newdata = test, n.trees = 5000) - test$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
AdjR2Model10test <- 1-(RSSModel10/TSStest)
AdjR2Model10test

#######################################################################################################################################################################################
############################################################################### End of modelling ###################################################################################### 
#######################################################################################################################################################################################
