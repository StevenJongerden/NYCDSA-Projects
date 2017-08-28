


housingdata = read.csv('housingdata.csv', na.strings = '')
setwd('/NYCDA/Assigment/Project 3/datasets/')


library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tabplot)
library(corrplot)
library(glmnet)
library(caret)
library(tree)
library(ISLR)
library(MASS)
library(VIM)
library(randomForest)


#Open Data Set ##Set your file location of the properties data set##
dataset <- fread("properties_2016.csv")
soldhouses <- fread("train_2016_v2.csv", header = TRUE)
dataset <- as.data.frame(left_join(soldhouses, dataset, by ="parcelid"))

#Remove completely missing rows from the dataset
dataset <- dataset[!is.na(dataset$regionidcounty),]
dataset <- dataset[dataset$regionidcounty==3101,]

#Remove rows that have empty taxvaluedollarcnt as these cannot be predicted
dataset <- dataset[!is.na(dataset$taxvaluedollarcnt),]

#Create table with comparison on missing values 
precentageall <- data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

#Imputate missing values and transformation of variables 
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

dataset[is.na(dataset$structuretaxvaluedollarcnt),"structuretaxvaluedollarcnt"] <- dataset[is.na(dataset$structuretaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$structuretaxvaluedollarcnt),"landtaxvaluedollarcnt"]
dataset[is.na(dataset$landtaxvaluedollarcnt),"landtaxvaluedollarcnt"] <- dataset[is.na(dataset$landtaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$landtaxvaluedollarcnt),"structuretaxvaluedollarcnt"]

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

dataset <- dataset[!is.na(dataset$structuretaxvaluedollarcnt),]
dataset <- dataset[!is.na(dataset$landtaxvaluedollarcnt),]

#Compute missing values in the cleaned data set  
precentageallclean <- data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))
classtest <- data.frame(lapply(dataset, function(x) class(x)))

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


#Create housing dataset on rule roomtcount >0
housingdataset <- dataset[dataset$structuretaxvaluedollarcnt!=0,]

#Create land dataset on rule ....
landdataset <- dataset[!is.na(dataset$regionidzip) & !is.na(dataset$yearbuilt),]



############################
###### Simple Tree ##########
############################

### Use All variable to predict structure tax


# 1.  Select columns sutiable for predicting 'structuretaxvaluedollarcnt' -----------

#1.1 Extract Year from transactiondate
housingdataset$transactionyear = substr(housingdataset$transactiondate,1,4)

#1.2 Filter unable to used columns
drop_col <- c('parcelid', 'logerror', 'transactiondate', 'hashottuborspa', 'propertyzoningdesc',
              'unitcnt', 'fireplaceflag', 'taxvaluedollarcnt', 'landtaxvaluedollarcnt', 'taxamount',
              'taxdelinquencyflag', 'taxdelinquencyyear', 'censustractandblock')


housing_sub <- housingdataset[ , -which(names(housingdataset) %in% drop_col)]

write.csv(housing_sub, file = 'housing_sub_tree')

#Double check no columns are factors
names(Filter(is.factor, housing_sub))

#rehionidzip over 32 levels, transfer to numeric

housing_sub$regionidzip <- as.numeric(housing_sub$regionidzip)


#Found column still involve na
colnames(housing_sub)[colSums(is.na(housing_sub)) > 0]
#[1] "yearbuilt"  
sum(is.na(housing_sub$yearbuilt))
#[1] 183
# Replace NA with mean of yearbuilt
housing_sub$yearbuilt[is.na(housing_sub$yearbuilt)] <- mean(housing_sub$yearbuilt)

# Remove yearbuilt = 0
# housing_sub <- housing_sub %>%
#               filter(., yearbuilt != 0)



####################################
### END OF VARIABLESM PREPARATION ##
####################################


# 2. Tree Modeling (Regressional)  --------------------------------------------------------




#column yearbuilt still involve na

# set 70-30 Training and test data 

set.seed(0)
train = sample(1:nrow(housing_sub), 7*nrow(housing_sub)/10)

#Training the tree to predict the structuretaxvaluedollarcnt.
tree.housing_sub = tree(structuretaxvaluedollarcnt ~ ., housing_sub, subset = train)
summary(tree.housing_sub)


# Regression tree:
#   tree(formula = structuretaxvaluedollarcnt ~ ., data = housing_sub, 
#        subset = train)
# Variables actually used in tree construction:
# [1] "finishedsquarefeet12"         "bathroomcnt"                 
# [3] "buildingqualitytypeid"        "calculatedfinishedsquarefeet"
# [5] "regionidzip"                 
# Number of terminal nodes:  9 
# Residual mean deviance:  1.951e+10 = 7.933e+14 / 40670 
# Distribution of residuals:
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -2809000   -58890   -14960        0    36490  4460000 

# Predict tree to test model

yhat = predict(tree.housing_sub, newdata = housing_sub[-train,])
housing_sub.test = housing_sub[-train, 'structuretaxvaluedollarcnt']
mean((yhat - housing_sub.test)^2)
#1st try -  22748242390
# 2nd try (keep factor) - 20315676094

plot(tree.housing_sub)
text(tree.housing_sub, pretty = 0)



#Take out transactionyear
housing_sub$transactionyear <- NULL




# Find distinct 
#sapply(housing_sub, function(x) length(unique(x)))





##############################################
### Engineering Feature and Random Forest ####
##############################################


##########################
## Property Tax ##########
##########################

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




#########################
## Simple Tree ##########
#########################


set.seed(0)
train = sample(1:nrow(housing_sub), 7*nrow(housing_sub)/10)

#Training the tree to predict the structuretaxvaluedollarcnt.
tree.housing_sub = tree(structuretaxvaluedollarcnt ~ ., housing_sub, subset = train)
summary(tree.housing_sub)

# Regression tree:
#   tree(formula = structuretaxvaluedollarcnt ~ ., data = housing_sub, 
#        subset = train)
# Variables actually used in tree construction:
#   [1] "finishedsquarefeet12"         "calculatedfinishedsquarefeet"
# [3] "buildingqualitytypeid"        "yearbuilt"                   
# Number of terminal nodes:  8 
# Residual mean deviance:  2.017e+10 = 8.229e+14 / 40800 
# Distribution of residuals:
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3315000   -57560   -13650        0    35000  4396000 


yhat = predict(tree.housing_sub, newdata = housing_sub[-train,])
housing_sub.test = housing_sub[-train, 'structuretaxvaluedollarcnt']
mean((yhat - housing_sub.test)^2)
# 23960564657

#########################################
##### RandomForest With Optimized Data###
#########################################


folds = createFolds(housingdataset$parcelid, 5)
test = housing_sub[folds[[1]], ]
train = housing_sub[-folds[[1]], ]

train <-train[complete.cases(train),]

#Reduce sample size to reduce computation time.
trainForest <- train[1:10000,]
TSS <- sum((trainForest$structuretaxvaluedollarcnt - mean(trainForest$structuretaxvaluedollarcnt, na.rm =  TRUE))^2)

#Setting the number of variables
#Cross validation of the number of variables tried at each split
R2ModelTree <- numeric(24)
for (i in 1:24) {
  fit = randomForest(structuretaxvaluedollarcnt ~ ., data = trainForest, mtry = i)
  RSSRandom <- sum((fit$predicted - trainForest$structuretaxvaluedollarcnt)^2, na.rm = TRUE)
  R2Random <- 1 - RSSRandom/TSS
  R2ModelTree[i] <- R2Random
  cat("We're performing iteration", i,',','R2Random', R2ModelTree[i], "\n")
}
R2ModelTree
plot(R2ModelTree, type = 'line')

# We're performing iteration 1 , R2Random 0.445505 
# We're performing iteration 2 , R2Random 0.6485277 
# We're performing iteration 3 , R2Random 0.6711023 
# We're performing iteration 4 , R2Random 0.6810039 
# We're performing iteration 5 , R2Random 0.6722897 
# We're performing iteration 6 , R2Random 0.6811156 
# We're performing iteration 7 , R2Random 0.6733767 
# We're performing iteration 8 , R2Random 0.6787255 
# We're performing iteration 9 , R2Random 0.6758664 
# We're performing iteration 10 , R2Random 0.6751908 
# We're performing iteration 11 , R2Random 0.6717451 
# We're performing iteration 12 , R2Random 0.6785003 
# We're performing iteration 13 , R2Random 0.6777858 
# We're performing iteration 14 , R2Random 0.676632 
# We're performing iteration 15 , R2Random 0.6743703 
# We're performing iteration 16 , R2Random 0.6705594 
# We're performing iteration 17 , R2Random 0.6693529 
# We're performing iteration 18 , R2Random 0.6688056 
# We're performing iteration 19 , R2Random 0.6640529 
# We're performing iteration 20 , R2Random 0.6727304 
# We're performing iteration 21 , R2Random 0.6644601 
# We're performing iteration 22 , R2Random 0.6683077 
# We're performing iteration 23 , R2Random 0.6714336 
# We're performing iteration 24 , R2Random 0.6706189 

# Result 6
fit2 = randomForest(structuretaxvaluedollarcnt ~ ., data = trainForest, ntree=100, mtry = 6)
plot(fit2)
Importance_structure <- data.frame(fit2$importance)
Importance_structure$variables <- rownames(Importance)
arrange(Importance_structure, desc(IncNodePurity))

#IncNodePurity                    variables
# 1   2.234421e+14         finishedsquarefeet12
# 2   2.015072e+14 calculatedfinishedsquarefeet
# 3   6.936239e+13            calculatedbathnbr
# 4   6.428646e+13        buildingqualitytypeid
# 5   5.804674e+13                  fullbathcnt
# 6   5.686276e+13                  bathroomcnt
# 7   5.136768e+13                    yearbuilt
# 8   5.084987e+13                    longitude
# 9   4.021109e+13            lotsizesquarefeet
# 10  3.640721e+13                     latitude
# 11  3.329138e+13       rawcensustractandblock
# 12  2.219249e+13        heatingorsystemtypeid
# 13  1.811932e+13                   bedroomcnt
# 14  1.364945e+13             population_level
# 15  1.143806e+13        airconditioningtypeid
# 16  7.943382e+12                      poolcnt
# 17  7.700602e+12                  pooltypeid7
# 18  7.684870e+12        propertylandusetypeid
# 19  6.310312e+12         finishedsquarefeet15
# 20  2.146370e+12                 pooltypeid10
# 21  6.155043e+09              numberofstories
# 22  6.814713e+08          buildingclasstypeid
# 23  0.000000e+00              garagetotalsqft
# 24  0.000000e+00               assessmentyear

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



############################################
###### Randomforest - LAND TAX #############
############################################=======================================================


# Extract housing sub
drop_col <- c('parcelid', 'logerror', 'transactiondate', 'hashottuborspa', 'propertyzoningdesc',
              'unitcnt', 'fireplaceflag', 'taxvaluedollarcnt', 'structuretaxvaluedollarcnt', 'taxamount',
              'taxdelinquencyflag', 'taxdelinquencyyear', 'censustractandblock')


land_sub <- landdataset[ , -which(names(landdataset) %in% drop_col)]


##Give engineering feature to 'regionidzip'

# 1. Group by regionidzip and mutate 

# >500	1
# 400 ~ 500	2
# 300~400	3
# 200 ~ 300	4
# 200	5

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



#########################################
##### RANDOM  FOREST  FOR  LAND TAX #####
####        MODEL            ############
#########################################

folds = createFolds(landdataset$parcelid, 5)
test = land_sub[folds[[1]], ]
train = land_sub[-folds[[1]], ]

train <-train[complete.cases(train),]

#Reduce sample size to reduce computation time.
trainforest <- train[1:10000,]


#Setting the number of variables
#Cross validation of the number of variables tried at each split
TSS <- sum((trainforest$landtaxvaluedollarcnt - mean(trainforest$landtaxvaluedollarcnt, na.rm =  TRUE))^2)
R2ModelTree_land <- numeric(24)
for (i in 1:24) {
  fit = randomForest(landtaxvaluedollarcnt ~ ., data = trainforest, mtry = i)
  RSSRandom <- sum((fit$predicted - trainforest$landtaxvaluedollarcnt)^2, na.rm = TRUE)
  R2randomf_land  <- 1 - RSSRandom/TSS
  R2ModelTree_land[i] <- R2randomf_land 
  cat("We're performing iteration", i,',','R2random_land', R2ModelTree_land[i], "\n")
}

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
# > 
