library(ggplot2)
library(randomForest)
library(dplyr)
library(readr)
library(ggcorrplot)
library(class)

#Import Dataset
Wine_data <- read_csv("C:/Users/nbern/Downloads/Wine_data.csv")
#str(Wine_data)

hist(Wine_data$quality)

min(Wine_data[,12])
max(Wine_data[,12])
#Minimum quality score is 3, maximum is 8

#Convert 'quality' column into binary:split into "poor" and "good" categories

#[3,6,8] bins


#Find correlation between variables
tmp<-cor(Wine_data)
ggcorrplot((tmp))


#Remove highly correlated and near zero correlated variables
df_new<-Wine_data[,-c(3,6,7,8,9)]

tmp2<-cor(df_new)
ggcorrplot((tmp2))

# Split data
ran = sample(1:nrow(df_new), size = 0.9 * nrow(df_new)) 

# Create training and test data
df_train = df_new[ran,1:6]
df_test = df_new[-ran,1:6] 

#Create target category for quality
df_target_category = df_new[ran,7]
df_target_category = as.integer(df_target_category$quality)

#Creat test category for quality
df_test_category = df_new[-ran,7]
df_test_category = as.integer(df_test_category$quality)

#Creat random forest model
df_RF = randomForest(x = df_train, 
                     y = df_target_category, ntree = 100, importance=TRUE)

#Predict quality using random forest
pred_1 = predict(df_RF, df_test)
table(pred_1)

#Find mean error
mean_err = mean(abs(pred_1-df_test_category))

#Make variable importance plot
varImpPlot(df_RF)

# Find r squared
rsquared_test = (cor(pred_1,df_test_category))^2

#Plot prediction and apply line of best fit
plot(df_test_category,pred_1,main="Prediction Scatterplot") #email Perkins why not working
abline(lm(df_test_category~pred_1),col="Green")



