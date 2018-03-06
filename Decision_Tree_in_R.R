#-----------------------------------------Decision Tree Regression----------------------------------------#
        
#Importing Data
dataset <- read.csv('/Users/mertcelik/Desktop/Position_Salaries.csv')
dataset = dataset[ , 2:3]


#Splitting the dataset to Test set and Training set
#install.packages('caTools')
#library('caTools')

# ?set.seed(123)
# split = sample.split(dataset$Profit, SplitRatio = 0.8)  #oluşturulan set'in dependent değeri üzerinden split yapilir.
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

#Feature Scaling
#training_set[,2:3] = scale(training_set[:2,3])
#test_set[,2:3] = scale(test_set[:2,3])

#Fitting the Decision Tree Regression model to fit dataset
install.packages('rpart')
library('rpart')

regressor = rpart(formula = Salary ~ ., data = dataset, control = rpart.control(minsplit = 1)) 


#Visualising the Decision Tree Regression results
library('ggplot2')

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)

ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary),colour = 'red') 
         + geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),colour = 'blue')
         + ggtitle('Truth or Bluff (Regression Model)') +
         + xlab('Level') +
         + ylab('Salary')


#Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))




