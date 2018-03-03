#-----------------------------------------Polynomial Regression----------------------------------------#


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
 
#Fitting the linear regression model to fit dataset
lin_reg = lm(formula = Salary ~ ., data = dataset)
summary(lin_reg)
#regressor = lm(formula = Profit ~ ., data = training_set) (Hepsinin ayrý ayrý + ile yapabildiðin gibi "." ile de yapabilirsin.)

#Fitting the polynomial regression model to fit dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Levels^3
#dataset$Level4 = dataset$Levels^4 (duruma göre kaçýncý level olmasý gerektiðini belirleyebilirsin.)
poly_reg = lm(formula = Salary ~ ., data = dataset)
summary(poly_reg)

#Visualising the Linear Regression results
ggplot() + geom_point(aes(x=dataset$Level,y=dataset$Salary),color = 'red')
         + geom_line(aes(x=dataset$Level,y=predict(lin_reg, newdata = dataset)),color='blue')
         + ggtitle('Truth or Bluff')
         + xlab('Level')
         + ylab('Salary')

#Visualising the Polynomial Regression results
ggplot() + geom_point(aes(x=dataset$Level,y=dataset$Salary),color = 'red')
         + geom_line(aes(x=dataset$Level,y=predict(poly_reg, newdata = dataset)),color='blue')
         + ggtitle('Truth or Bluff')
         + xlab('Level')
         + ylab('Salary')

#Predicting a new result  with Linear Regression
y_pred = predict(lin_reg, data.frame(Level = 6.5) )
#Predicting a new result  with Polynomial Regression
y_pred = predict(poly_reg, data.frame(Level = 6.5, Level2=6.5^2, Level3=6.5^3) )




?data.frame()