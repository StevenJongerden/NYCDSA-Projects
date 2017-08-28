# zillow dataset modeling part II
# By Jing Wang 
# 2017-08

# load data
library(caret)
library(dplyr)
load('../data/df_tax_str.Rda')
# reload the data
data = df_tax1
# brief check

str(data)
names(data)
dim(data)
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) length(unique(x)))
sapply(data, class)


# drop some columns

data$assessmentyear = NULL

# data = subset(data[1:10000, ]) # test 1000 row before





# set factors
# data$a = as.factor(data$a)



# partition the data
set.seed(0)
indexes1 = createDataPartition(data$structuretaxvaluedollarcnt,
                              times = 1,
                              p = 0.6,
                              list = FALSE)
df.train = data[indexes1, ]
df2 = data[-indexes1, ]
indexes2 = createDataPartition(df2$structuretaxvaluedollarcnt,
                              times = 1,
                              p = 0.5,
                              list = FALSE)
df.valid = df2[indexes2, ]
df.test  = df2[-indexes2, ]

y.train = df.train[, 40]
y.valid = df.valid[,40]
y.test= df.test[, 40]

print(nrow(df.train)/nrow(data))
print(nrow(df.valid)/nrow(data))
print(nrow(df.test)/nrow(data))

# modeling_1: Regression Tree
# modeling_2: Random Forest/Bagging

# modeling_3: Boosting
library(gbm)

# step.1 generate an overfit model first
set.seed(0)
boost.model = gbm(structuretaxvaluedollarcnt ~ ., 
                  data = df.train,
                  distribution = "gaussian",
                  n.trees = 5000,
                  interaction.depth = 4) # also need set lambda

# par(mfrow = c(1, 1))
summary(boost.model)

# step.2 make a series of prediction based on different tree numbers
n.trees = seq(from = 100, to = 5000, by = 100)
predmat = predict(boost.model, 
                  newdata = df.valid, 
                  n.trees = n.trees,
                  shrinkage = 0.01)

#Produces 100 different predictions for each of the observations in our
#test set.
dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
boo.err = with(df.valid, apply((predmat - y.valid)^2, 2, mean))
plot(n.trees, boo.err, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Include the best OOB error from the random forest.
abline(h = min(boo.err), col = "red")
min(boo.err)


#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
set.seed(0)
boost.model.2 = gbm(structuretaxvaluedollarcnt ~ ., 
                    data = df.train,
                    distribution = "gaussian",
                    n.trees = 5000,
                    interaction.depth = 4,  #10
                    #cv.folds = 3,
                    shrinkage = 0.01) # make shrinkage big (default is 0.01)

predmat.2 = predict(boost.model.2, 
                    newdata = df.valid,
                    n.trees = n.trees)

boo.err.2 = with(df.valid, apply((predmat.2 - y.valid)^2, 2, mean))
min(boo.err.2)

plot(n.trees, boo.err.2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

gbm.perf(boost.model.2)


print(relative.influence(boost.model.2))
vars = summary(boost.model.2)

vars_selected = vars[vars[2]$rel.inf>0, 1]
right = paste(vars_selected, collapse = " + ")
form = paste('structuretaxvaluedollarcnt ~ ', right)

# train pruned model with train dateset =================
boost.model.pruned = gbm(structuretaxvaluedollarcnt ~ 
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
                       + finishedsquarefeet15 + unitcnt, 
                       
                    data = df.train,
                    distribution = "gaussian",
                    n.trees = 5000,
                    interaction.depth = 10, #4>5>6>7>8>9>*10>15>20
                    # cv.folds = 3,
                    shrinkage = 0.01) #>1,0.1,*0.01, >>0.001

# R-squred: 
#   n.trees
#   depth: 0.6277704>0.6314801> 0.6292806 > 0.6360735 > 0.6363856 > *0.6418633
#   cv.folds = 
#   shrinkage = 0.4719414 >...*0.6363856>0.5765003


# validate using valide dataset ========================

predmat.3 = predict(boost.model.pruned, 
                    newdata = df.valid,
                    n.trees = n.trees)

boo.err.valid = with(boost.model.pruned, apply((predmat.3 - y.valid)^2, 2, mean))
min(boo.err.valid)

plot(n.trees, boo.err.valid, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")
abline(h = min(boo.err.valid), col = "red")


y_bar = mean(y.valid)
ssr = with(df.valid, apply((predmat.3 - y.valid)^2, 2, sum))
sst = with(df.valid, apply((predmat.3 - y_bar)^2, 2, sum))
max(1 - (ssr/sst))

# save the model
model.best = boost.model.pruned


# predict using test set ================================

predmat.4 = predict(model.best, 
                    newdata = df.test,
                    n.trees = n.trees)

boo.err.test = with(model.best, apply((predmat.4 - y.test)^2, 2, mean))
min(boo.err.valid)

plot(n.trees, boo.err.test, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

y_hat = mean(y.test)
ssr = with(df.test, apply((predmat.4 - y.test)^2, 2, sum))
sst = with(df.test, apply((predmat.4 - y_hat)^2, 2, sum))
max(1 - (ssr/sst))
