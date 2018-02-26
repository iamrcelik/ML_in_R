#Simple Linear Regression

dataset <- read.csv('/Users/mertcelik/Downloads/Simple_Linear_Regression/Salary_Data.csv')


install.packages('caTools')
library('caTools')

split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience, data = training_set) 

#Training the test set results
y_pred = predict(regressor, newdata = test_set)


#virtualising the training set results 
ggplot() + geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),color='red') + geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),color='blue') + ggtitle('Salary vs Experience(Training set)') + xlab('Years of Experience') + ylab('Salary')


#virtualising the test set results 
ggplot() + geom_point(aes(x=test_set$YearsExperience,y=test_set$Salary),color='red') + geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),color='blue') + ggtitle('Salary vs Experience(Test set)') + xlab('Years of Experience') + ylab('Salary')
