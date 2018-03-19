#-----------------------------------------SVR----------------------------------------#


#Importing Data
dataset <- read.csv('/Users/mertcelik/Desktop/Position_Salaries.csv')
dataset = dataset[ , 2:3]


#Splitting the dataset to Test set and Training set
install.packages('caTools')
library('caTools')

# ?set.seed(123)
# split = sample.split(dataset$Profit, SplitRatio = 0.8)  #olu?turulan set'in dependent de?eri ?zerinden split yap?l?r.
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

#Feature Scaling
#training_set[,2:3] = scale(training_set[:2,3])
#test_set[,2:3] = scale(test_set[:2,3])

#Fitting the SVR model to fit dataset
install.packages('e1071')
library('e1071')

regressor = svm(formula = Salary ~ ., data = dataset, type = 'eps-regression') 

#Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))


#Visualising the SVR results
library(ggplot2)
ggplot() + geom_point(aes(x=dataset$Level,y=dataset$Salary),color = 'red')+ geom_line(aes(x=dataset$Level,y=predict(regressor , newdata = dataset)),color='blue') + ggtitle('Truth or Bluff') + xlab('Level') + ylab('Salary')



