#-----------------------------------------SVR----------------------------------------#


#Importing Data
dataset <- read.csv('C:/Users/mertc/Desktop/Position_Salaries.csv')
dataset = dataset[ , 2:3]


#Splitting the dataset to Test set and Training set
install.packages('caTools')
library('caTools')

# ?set.seed(123)
# split = sample.split(dataset$Profit, SplitRatio = 0.8)  #oluþturulan set'in dependent deðeri üzerinden split yapýlýr.
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
ggplot() + geom_point(aes(x=dataset$Level,y=dataset$Salary),color = 'red')
+ geom_line(aes(x=dataset$Level,y=predict(regressor , newdata = dataset)),color='blue')
+ ggtitle('Truth or Bluff')
+ xlab('Level')
+ ylab('Salary')



#Predicting a new result  with Linear Regression
y_pred = predict(lin_reg, data.frame(Level = 6.5) )
#Predicting a new result  with Polynomial Regression
y_pred = predict(poly_reg, data.frame(Level = 6.5, Level2=6.5^2, Level3=6.5^3) )



