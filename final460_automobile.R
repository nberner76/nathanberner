# Project 3

library(dplyr)
library(readr)
library(ggcorrplot)
library(class)
library(readxl)
library(ggplot2)
library(pdp)
library(randomForest)

# Read data files
data<-read_csv("C:/Users/nbern/Downloads/Automobile_dataneqq.csv")

#Remove NAs
data<-na.omit(data)

#Import Other data files

What_if_mpg <- read_csv("C:/Users/nbern/Downloads/What_if_mpg.csv")

What_if_mpg2 <- read_excel("C:/Users/nbern/Downloads/What_if_mpg2.xlsx")

#Convert character column to factors

data$make<-as.factor(data$make)
data$`fuel-type`<-as.factor(data$`fuel-type`)
data$aspiration<-as.factor(data$aspiration)
data$`num-of-doors`<-as.factor(data$`num-of-doors`)
data$`body-style`<-as.factor(data$`body-style`)
data$`drive-wheels`<-as.factor(data$`drive-wheels`)
data$`engine-location`<-as.factor(data$`engine-location`)
data$`engine-type`<-as.factor(data$`engine-type`)
data$`num-of-cylinders`<-as.factor(data$`num-of-cylinders`)
data$`fuel-system`<-as.factor(data$`fuel-system`)

#Convert the factor inputs to numeric to run correlation plot
data$`num-of-doors`<-as.numeric(data$`num-of-doors`)
data$`fuel-type`<-as.numeric(data$`fuel-type`)
data$aspiration<-as.numeric(data$aspiration)
data$make<-as.numeric(data$make)
data$`body-style`<-as.numeric(data$`body-style`)
data$`drive-wheels`<-as.numeric(data$`drive-wheels`)
data$`engine-location`<-as.numeric(data$`engine-location`)
data$`engine-type`<-as.numeric(data$`engine-type`)
data$`num-of-cylinders`<-as.numeric(data$`num-of-cylinders`)
data$`fuel-system`<-as.numeric(data$`fuel-system`)

# Find and plot correlation map
df_corr<-cor(data)
ggcorrplot(df_corr)

#Remove highly correlated
data<-data[,-c(4,24,25)]

# Define training data
##Generate a random number that is 90% of the total number of rows in dataset.
## we'll use this for splitting the training/test data sets later
ran = sample(1:nrow(data), size = 0.9 * nrow(data)) 

##extract training set - random 90% of data rows (ran was the random numbers generated above)
mpg_train = data[ran,c(1:23)] # df[vals,] pulls out the 'vals' rows of the dataframe

##extract testing set (not ran rows)
mpg_test = data[-ran,c(1:23)] 

##extract 5th column of train dataset because it will be used as the regression target
mpg_target_category = data[ran,24]
mpg_target_category = as.integer(mpg_target_category$AVG_mpg)

##extract 5th column of test dataset to measure the accuracy
mpg_test_category = data[-ran,24]
mpg_test_category = as.integer(mpg_test_category$AVG_mpg)

# Train random forest model for regression
mirkwood = randomForest(x = mpg_train, 
                        y = mpg_target_category, ntree = 50, importance=TRUE)

# Predict targets for testing data points
test_data = predict(mirkwood, mpg_test)

#Plot linear regression
plot(mpg_test_category,test_data)
abline(lm(mpg_test_category~test_data),col="Green")

#Find R^2 value and mean error
mean_err = mean(abs(test_data-mpg_test_category))
rsquared_test = (cor(test_data,mpg_test_category))^2

#Create a histogram
hist(data$AVG_mpg, main="AVG mpg Histogram", xlab="AVG mpg")

#Create variable importance plot
varImpPlot(mirkwood)

#Define new testing data and predicitons

What_if_mpg_test1 = What_if_mpg[,c(1:23)]

mpg_if_predict1 = predict(mirkwood,What_if_mpg_test1)

What_if_mpg_test2 = What_if_mpg2[,c(1:23)]

mpg_if_predict2 = predict(mirkwood,What_if_mpg_test2)

# Partial Dependence Plots regrading AVG MPG

mpgPDP = partial(mirkwood, train = mpg_train, pred.var = c(17), chull = TRUE)
autoplot(mpgPDP, contour = TRUE)

mpgPDP = partial(mirkwood, train = mpg_train, pred.var = c(18), chull = TRUE)
autoplot(mpgPDP, contour = TRUE)

mpgPDP = partial(mirkwood, train = mpg_train, pred.var = c(21), chull = TRUE)
autoplot(mpgPDP, contour = TRUE)
#*********************************************************************************

#Create 3 variable plots to optimize values
DAP1 = What_if_mpg_test1
DAP1$AVG_mpg = mpg_if_predict1
DAP1 %>% group_by(horsepower) %>% ggplot(aes(x=horsepower, y=AVG_mpg, color=bore)) + geom_point() + 
ggtitle("Effect of AVG_mpg with horsepower and bore") + labs(x = "horsepower", y = "AVG_mpg") 

DAP2 = What_if_mpg_test2
DAP2$AVG_mpg = mpg_if_predict2
DAP2 %>% group_by(horsepower) %>% ggplot(aes(x=horsepower, y=AVG_mpg, color=`fuel-system`)) + geom_point()
+ ggtitle("Effect of AVG_mpg with horsepower and fuel system") + labs(x = "horsepower", y = "AVG_mpg") 

