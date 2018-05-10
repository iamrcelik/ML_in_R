#----------------------------------------- NLP ----------------------------------------#


# Importing the dataset
dataset_original = read.delim('/Users/mertcelik/Desktop/yorumlar.tsv', quote = '', stringsAsFactors = FALSE)
a <- c("Bugün hava çok güzel")
b <- c("Bugün hava kötü",0)
c <- c("Bugün hava çok kötü gibi")
dataset_original <- rbind(dataset_original,a)
dataset_original <- rbind(dataset_original,b)
dataset_original <- rbind(dataset_original,c)
dataset_original[14,2] <- NA
library(tm)
library(SnowballC) # stopWord() fonksiyonu için

corpus = VCorpus(VectorSource(dataset_original$Review))
corpus = tm_map(corpus, content_transformer(tolower))  # corpus dakileri lowercase yapıyor.
# as.character(corpus[[1]])
corpus = tm_map(corpus, removeNumbers) # corpus daki numaraları kaldırıyor
# as.character(corpus[[2]])
corpus = tm_map(corpus, removePunctuation) # corpus daki noktalamaları kaldırıyor..
# as.character(corpus[[1]])
corpus = tm_map(corpus, removeWords, stopwords()) # corpus daki alakasız kelimeleri stopword() ile bulup kaldırıyoruz.
# as.character(corpus[[1]])
corpus = tm_map(corpus, stemDocument) # kelimenin root haline gidiyor (loved --> love)
# as.character(corpus[[3]])
corpus = tm_map(corpus, stripWhitespace) # corpustaki düzeltmeler nedeniyle oluşan extra spaceleri kaldırıyoruz.

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)

classifier = randomForest(x = training_set[-ncol(training_set)], y = training_set$Liked, ntree = 110)

library(e1071)
classifier = svm(formula = training_set$Liked ~., data = training_set[-ncol(training_set)], type = 'C-classification', kernel = 'linear')

classifier = naiveBayes( x = training_set[-ncol(training_set)], y = training_set$Liked )


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-ncol(test_set)])



# Making the Confusion Matrix
cm = table(test_set[, ncol(test_set)], y_pred)
matrix <- data.frame(cm)
matrix1 <- data.matrix(matrix)

slice <- c(matrix1[1,3] + matrix1[4,3], matrix1[2,3] + matrix1[3,3])
labels <- c("Correct Predict","Wrong Predict")
#install.packages('plotrix')
library(plotrix)
piepercent<- round(100*slice/sum(slice), 1)
lbls <- paste(labels,piepercent)
lbls <- paste(lbls, "%", seq="")
pie3D(slice, labels = lbls, col= rainbow(length(labels)), main="Sentiment Analysis with Suppor Vector Machine (SVM)")

# 149 51 ----> %74,5 doğruluk oranı random forest (n=10)
# 101 99 ----> %50,5 doğruluk oranı naive bayes
# 152 48 ----> %76 doğruluk oranı random forest (n=50)
# 155 45 ----> %77,5 doğruluk oranı random forest (n=110)

