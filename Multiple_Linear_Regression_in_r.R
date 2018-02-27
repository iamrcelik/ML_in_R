#-----------------------------------------Multiple Linear Regressio----------------------------------------#

#Importing Data
dataset <- read.csv('50_Startups.csv')

#Encoding categorical data
dataset$State = factor(dataset$State, levels=c('New York','California','Florida'), labels=c(1,2,3))


#Splitting the dataset to Test set and Training set
install.packages('caTools')
library('caTools')

?set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)  #oluşturulan set'in dependent değeri üzerinden split yapılır.
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
#training_set[,2:3] = scale(training_set[:2,3])
#test_set[,2:3] = scale(test_set[:2,3])

#Fitting the multiple linear regression model to fit training set
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = training_set)
#regressor = lm(formula = Profit ~ ., data = training_set) (Hepsinin ayrı ayrı + ile yapabildiğin gibi "." ile de yapabilirsin.)

#summary(regressor) (Coefficientleri,t-value ve p-value görebilirsin.)

#Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)


ggplot() + geom_point(aes(x=test_set$R.D.Spend, y=test_set$Profit), color='red') 
         + geom_line(aes(x=test_set$R.D.Spend, y=predict(regressor,newdata = test_set)),color='blue') 
         + ggtitle('R.D.Spend vs Profit') 
         + xlab('R.D.Spend') 
         + ylab('Profit') 
         

 


