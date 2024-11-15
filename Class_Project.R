## Louis ATALLAH

#########################################################################################

# Libraries
library(ggplot2)
library(corrplot)
library(GGally)
library(e1071)
library(dplyr)
library(randomForest)

# 1

# a & b 

housing <- read.csv("housing.csv")

summary(housing)

# c
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

str(housing)

summary(housing$ocean_proximity)
# <1H OCEAN     INLAND     ISLAND   NEAR BAY NEAR OCEAN 
#      9136       6551          5       2290       2658 

# 2

# a

head(housing)

tail(housing)

# b

summary(housing)

# c
par(mfrow=c(1,1))
corrplot(cor(housing[sapply(housing,is.numeric)], use = "complete.obs"))

## After visualizing the corrplot matrix, we can observe that many numeric features are correlated to each other. 
## We notice a strong positive pairwise correlation (close to 1) between the number of rooms (total_rooms & total_bedrooms), 
## the population and the households. Hence; for example if we take total_rooms and the population:
## the more bedrooms the houses of the area have, the more people live in the area.
## we can see as well a positive correlation between the median_house_value and the median income (correlation coefficient of 0.6).
## While the longitude and the latitude are strongly negatively correlated, 
## which means the higher is the latitude, the lower the longitude is.

# d

par(mfrow = c(3,3))

for (i in seq(ncol(housing))){

  if (is.numeric(housing[,i])) {
  
    hist(housing[,i]
         , main = as.character(names(housing)[i])
         , xlab = as.character(names(housing)[i])
         , col = "lightblue")
  }
}



# e


for (i in seq(ncol(housing))){
  
  if (is.numeric(housing[,i])) {
    
    boxplot(housing[,i]
         , main = as.character(names(housing)[i])
         , xlab = as.character(names(housing)[i])
         , col = "lightblue")
  }
}


# f

par(mfrow = c(1,3))

num_boxplot <- function(x) {
  for (i in x){
    k <- grep(i,colnames(housing))
    boxplot(housing[,k]~housing$ocean_proximity
            , main = i
            , xlab = i
            , ylab = "value"
            , col = "lightblue")}
}

num_boxplot(c("housing_median_age","median_income","median_house_value"))


# 3

# a

housing <- impute(housing,"median")

housing <- data.frame(housing)

# casting as numeric variables
for (i in seq(9)){
  housing[,i] <- as.numeric(housing[,i])
  
}

# b

housing$H_OCEAN <- ifelse(housing$ocean_proximity == "<1H OCEAN",1,0)

housing$INLAND <- ifelse(housing$ocean_proximity == "INLAND",1,0)

housing$ISLAND <- ifelse(housing$ocean_proximity == "ISLAND",1,0)

housing$NEAR_BAY <- ifelse(housing$ocean_proximity == "NEAR BAY",1,0)

housing$NEAR_OCEAN <- ifelse(housing$ocean_proximity == "NEAR OCEAN",1,0)

# removing ocean_proximity
housing <- select(housing,-ocean_proximity)

# casting as factor variables
for (i in seq(10,14)){
  housing[,i] <- as.factor(housing[,i])
  
}


# c

housing$mean_number_rooms <- housing$total_rooms/housing$households

housing$mean_number_bedrooms <- housing$total_bedrooms/housing$households

# removing total_rooms & total_bedrooms
housing <- select(housing,-total_bedrooms,-total_rooms)

# d

varnames <- c("median_house_value","H_OCEAN","INLAND","ISLAND","NEAR_BAY","NEAR_OCEAN")

index <- names(housing) %in% varnames

housing[,!index] <- as.data.frame(scale(housing[,!index]))


# e

cleaned_housing <- data.frame(housing)


# 4

# a
set.seed(123)
sampleIndex <- sample(nrow(cleaned_housing),nrow(cleaned_housing)*0.7, replace=FALSE)

# b

train <- cleaned_housing[sampleIndex,]

# c

test <- cleaned_housing[-sampleIndex,]


# 5

train_x <- select(train,-median_house_value)

train_y <- train[,7]                    

rf = randomForest(x=train_x, y=train_y ,
                  ntree=500, importance=TRUE)

names(rf)

# [1] "call"            "type"            "predicted"       "mse"             "rsq"             "oob.times"       "importance"     
# [8] "importanceSD"    "localImportance" "proximity"       "ntree"           "mtry"            "forest"          "coefs"          
# [15] "y"               "test"            "inbag"          


# 6

# a

rf$mse

MSE <- rf$mse[500]

RMSE <- sqrt(MSE)
RMSE

# [1] 431.0554


# b

test_x <- select(test,-median_house_value)

test_y <- test[,7]

pred <- predict(rf,test_x)

# c

rmse <- function(y_hat, y)
{
  return(sqrt(mean((y_hat-y)^2)))
}

actual <- test_y

rmse(pred,actual)

# [1] 426.5007


# d


## Between the rmse on the train and the test sets there is only less than 5 points of difference with a slight improvement
## for the test set. This means that the model is not overfitting and confirm good predictions.

# e

varImpPlot(rf)

## It displays the importance of each variable to make predictions. The median income is the most important 
## feature, while ISLAND (whether the neighbourhood is in the inland part) is the less.
## Let's try to compute the model again but without the 4 least important features, which are H_OCEAN,
## NEAR_OCEAN, NEAR_BAY, ISLAND

# Removing features
train2_x <- select(train_x,-H_OCEAN,-ISLAND,-NEAR_BAY,-NEAR_OCEAN)
test2_x <- select(test_x,-H_OCEAN,-ISLAND,-NEAR_BAY,-NEAR_OCEAN)

rf2 = randomForest(x=train2_x, y=train_y ,
                  ntree=500, importance=TRUE)

rf2$mse[500]
# [1] 187965

# The RMSE on the train set
sqrt(rf2$mse[500])
# [1] 433.5493

pred2 <- predict(rf2,test2_x)

# RMSE on the test set
rmse(pred2,actual)
# [1] 427.1518

## Based on the RMSE, the first model with every feature is a bit more performant. It has a RMSE on
## the test set of 426.5007 against 427.1518 for the second lightened model.