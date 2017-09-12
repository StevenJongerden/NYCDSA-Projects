library(jpeg)
library(grid)
library(gridExtra)
library(plotrix)
library(readr)
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("foreach")) install.packages("foreach")
if (!require("doParallel")) install.packages("doParallel")
if (!require("ggplot2")) install.packages("ggplot2")

ColorFinder <- function(number=number){
  img <- readJPEG(paste('C:/Users/Steven Jongerden/Desktop/CapStone Project/Images/car',number,'.jpg', sep = ""))
  img <- img[75:175,150:300,1:3]
  df = data.frame(  red = matrix(img[,,1], ncol=1),  green = matrix(img[,,2], ncol=1),  blue = matrix(img[,,3], ncol=1))
  K = kmeans(df,1)
  df$label = K$cluster
  colors = data.frame(  label = 1:nrow(K$centers),   R = K$centers[,"red"],  G = K$centers[,"green"],  B = K$centers[,"blue"])
  df$order = 1:nrow(df)
  df = merge(df, colors)
  df = df[order(df$order),]
  df$order = NULL
  R = matrix(df$R, nrow=dim(img)[1])
  G = matrix(df$G, nrow=dim(img)[1])
  B = matrix(df$B, nrow=dim(img)[1])
  img.segmented = array(dim=dim(img))
  img.segmented[,,1] = R
  img.segmented[,,2] = G
  img.segmented[,,3] = B
  img <-round(img.segmented,1)
  
  color = unlist(lapply(rgb(img[runif(1, 0, 101),runif(1, 0, 151),1], img[runif(1, 0, 101),runif(1, 0, 151),2], img[runif(1, 0, 101),runif(1, 0, 151),3], maxColorValue = 1), color.id))
  color[1]
}

#Set the number of workers for parellel computing
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

TestData <- foreach (i = 1:1000, combine = cbind) %dopar% {
  library(jpeg)
  library(grid)
  library(gridExtra)
  library(plotrix)
  ColorFinder(i)
}

ColorData <- data.frame(color = unlist(TestData))

ggplot(ColorData, aes(color))+geom_bar(stat="count") +coord_flip()


CarDataSet <- read_csv("C:/Users/Steven Jongerden/Desktop/CapStone Project/Images/PictureData.csv")
CarDataSet$X1 <- NULL
CarDataSet$CarPrice <- ifelse(is.na(as.numeric(gsub(",", "",substr(CarDataSet$CarPrice,9,20)))),
                              as.integer(gsub(",", "",substr(CarDataSet$CarPrice,2,20))),
                              as.integer(gsub(",", "",substr(CarDataSet$CarPrice,9,20))))

ColorData <- cbind(ColorData, CarDataSet[1:846,"CarPrice"])

ggplot(ColorData, aes(color, CarPrice))+geom_boxplot()+coord_flip()
