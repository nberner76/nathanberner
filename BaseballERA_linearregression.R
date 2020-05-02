library(readr)
library(ggcorrplot)
library(caret)
library(randomForest)

#Import Dataset
Pitching <- read_csv("C:/Users/nbern/Downloads/baseballdatabank/Pitching.csv")

#Clean data
data<-na.omit(Pitching)
nearZeroVar(data)
data_new<-data[,-c(1,2,10,11,12,24)]

#Coversions of columns to all numeric
data_new$teamID<-as.factor(data_new$teamID)
data_new$lgID<-as.factor(data_new$lgID)
#Convert factor columns above to numeric 
data_new$lgID<-as.numeric(data_new$lgID)
data_new$teamID<-as.numeric(data_new$teamID)
# Convert Boolean to numeric
data_new$IBB<-as.numeric(data_new$IBB)
data_new$SH<-as.numeric(data_new$SH)
data_new$SF<-as.numeric(data_new$SF)
data_new$GIDP<-as.numeric(data_new$GIDP)

#Correlation Plot/Analysis
tmp<-cor(data_new)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
# Remove any variables above 0.75 correlation
data.new1 <- data_new[,!apply(tmp,2,function(x) any(x > 0.85))]
#Correlation Plot
ggcorrplot(tmp)


#Set up training and test data for ERA target column
ran=sample(1:nrow(data.new1),size=0.9*nrow(data.new1))
ERA_train=data.new1[ran,c(1:10,12:20)]
ERA_test=data.new1[-ran,c(1:10,12:20)]

ERA_target_category = data.new1[ran,11]
ERA_target_category = as.integer(ERA_target_category$ERA)


ERA_test_category = data.new1[-ran,11]
ERA_test_category = as.integer(ERA_test_category$ERA)


#Create random forest to make prediciton

RF=randomForest(x=ERA_train,y=ERA_target_category, trees=75, importance=TRUE)

#Prediction from random forest
predictERA=predict(RF,ERA_test)

# Plot linear regression
plot(ERA_test_category,predictERA)
abline(lm(ERA_test_category~predictERA,col="Purple"))

# Find R^2 and Mean error
mean_error=mean(abs(predictERA-ERA_test_category))
r_squared=(cor(predictERA,ERA_test_category))^2

#Variable Importance Plot OF Random Forest
varImpPlot(RF)

# Opponent bating average is the most important factor for ERA prediciton
