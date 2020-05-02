## Import the sonar data

library(dplyr)
library(reshape2)
library(ggplot2)
library(class)
library(ggcorrplot)

# setup a list for the names of the dataframe columns - ignore if data already is labeled in the file
columnNames = c()
# for loop to generate the names (Freq1, Freq2, Freq3, ..., R_or_M)
for (i in c(1:61)) {
  if (i < 61) {
    columnNames[i] = paste("Freq",toString(i), sep="")
  } else {
    columnNames[i] = "R_or_M"
  }
}

# import the dataframe ( read.csv(file=filename, header=areThereColumnLabels?, col.names=newColumnNames) )
sonar = read.csv(file="sonar.all-data", header=FALSE, col.names=columnNames)


# create a correlation heatmap
sonar2 = sonar # making a copy we can mess with
sonar2$R_or_M = as.numeric(sonar$R_or_M)
corr = cor(sonar2)
ggcorrplot(corr, type='upper')

# Box Plot
ggplot(sonar, aes(x=R_or_M, y=Freq1, color=R_or_M)) + geom_boxplot()

# example melt to reshap the data if needed
df_melted = melt(sonar, id=61, measure=1:60, variable.name = "Dimension", value.name = "Value")
ggplot(df_melted, aes(x=Dimension, y=Value, color=R_or_M)) + geom_boxplot()

##the normalization function is created - not applied to df yet, just a function we're writing
nor = function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns (c(1,2,3,4)) of dataset because they are the predictors
## lapply applies the "nor" function we created earlier to each of the columns we specify
sonar_norm = as.data.frame(lapply(sonar[,c(1:60)], nor))

head(sonar_norm)
summary(sonar_norm)

# setup the KNN classifier
##Generate a random number that is 90% of the total number of rows in dataset.
## we'll use this for splitting the training/test data sets later
ran = sample(1:nrow(sonar), size = 0.9 * nrow(sonar)) 

##extract training set - random 90% of data rows (ran was the random numbers generated above)
sonar_train = sonar_norm[ran,] # df[vals,] pulls out the 'vals' rows of the dataframe

##extract testing set (not ran rows)
sonar_test = sonar_norm[-ran,] 

##extract column of train dataset because it will be used as 'cl' argument in knn function.
sonar_target_category = sonar[ran,61]

##extract column of test dataset to measure the accuracy
sonar_test_category = sonar[-ran,61]

# run the KNN classifier
pr = knn(sonar_train,sonar_test,cl=sonar_target_category,k=3) # use 13 nearest neighbors

##create confusion matrix
tab = table(pr,sonar_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}


# test and score it
## run the accuracy function on the confusion matrix
accuracy(tab)





