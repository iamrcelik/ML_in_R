#----------------------------------------- Twitter Connection ----------------------------------------#

install.packages ("twitteR")
install.packages ("ROAuth")
install.packages("RCurl")

library(twitteR)
library(ROAuth)
library(RCurl)

#Gerekli connectionlar sağlandı.
setup_twitter_oauth(consumer_key = "yz47uxCURPWd6fkG7PFGp87zT",
                    consumer_secret = "JQRJFaIAcvjTbIODGddo7FEe1uoGLXEFh7D76rejCq5jS4zM4J",
                    access_token = "341754297-xiqcxeU5sXS29ltJLG1X4GCGOjSpNDWqkr8tMmMB",
                    access_secret = "bK3vou3QSeORRSOpNqG1ywojDsnc4JxvuIuShD8WRlvO5")

#Twitter üzerinden 5 adet tweet çekildi.
hashtag <- searchTwitter("#facebook", n=50, lang="en")

#çekilen tweetlerin özelliklerinden oluşan bir tablo yaratıldı.
x <- twListToDF(hashtag)

#Shiny paketinin eklenerek bir uygulama gerçeklenmesi
library(shiny)

ui <- fluidPage(
        titlePanel("Twitter Seniment Analysis"),
        
        sidebarLayout(
                sidebarPanel(
                        textInput("term","Enter Search Term:","#example"),
                        sliderInput("i", "Select no. of Tweets:", 0, 1500, 100, step = 50, round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE),
                        radioButtons("pType", "Select a Plot type:",
                                     list("Sentiment Trends"='a', "Sentiment Scores"='b', "Word Cloud"='c')),
                        submitButton("Analyze!"),
                        print(h6("  Be Patient, Good Things Take Time!"))
                        ),

                mainPanel(
                        dataTableOutput("table"),
                        plotOutput("plot"),
                )
        ) )

server <- function(input, output) {

        output$plot<-renderPlot({ 
                if(input$pType=='c')
                {
                        searchterm<-input$term
                        num<-input$i
                        
                        list <- searchTwitter(searchterm, n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10)
                        x <- twListToDF(list)
                        library(tm)
                        library(SnowballC) # stopWord() fonksiyonu için
                        
                        
                        l <- sapply(list, function(x) x$getText())
                        
                        l <- iconv(l, "latin1", "ASCII//TRANSLIT")
                        
                        
                        l <- iconv(l, to='ASCII//TRANSLIT')
                        
                        l <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", l)
                        l <- gsub("[^0-9A-Za-z///' ]", "", l)
                        
                        
                        
                        
                        
                        #create corpus
                        lc <- Corpus(VectorSource(l))
                        
                        #clean up
                        lc <- tm_map(lc, content_transformer(tolower)) 
                        lc <- tm_map(lc, removePunctuation)
                        lc <- tm_map(lc, stripWhitespace)
                        lc = tm_map(lc, removeWords, stopwords())
                        
                        
                        library(RColorBrewer)
                        library(wordcloud)
                        pal2 <- brewer.pal(8,"Dark2")
                        wordcloud(lc,min.freq=num/200,max.words=500, random.order=T, colors=pal2)      
                }
                if(input$pType=='b'){
                        
                        searchterm<-input$term
                        num<-input$i
                        list <- searchTwitter(searchterm, n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10)
                        x <- twListToDF(list)
                        x$text <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", x$text)
                        x$text <- gsub("[^0-9A-Za-z///' ]", "", x$text)
                        library(tm)
                        library(SnowballC) # stopWord() fonksiyonu için


                        
                        x1 <- cbind(x$text,NA)
                        x2 <- as.data.frame(x1)
                        
                        colnames(x2) <- c("Review","Liked")
                        dataset_original = read.delim('/Users/mertcelik/Desktop/Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)
                        x3 <- rbind(dataset_original,x2)
                        #tail(x3)
                        
                        corpus = VCorpus(VectorSource(x3$Review))
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
                        dataset$Liked = x3$Liked
                        
                        dataset$Liked = factor(dataset$Liked, levels = c(0, 1))
                        library(caTools)
                        split = sample.split(dataset$Liked, SplitRatio = 0.9)
                        training_set = subset(dataset, split == TRUE)
                        test_set = subset(dataset, split == FALSE)
                        ncol(training_set)
                        
                        library(e1071)
                        classifier = svm(formula = training_set$Liked ~., data = training_set[-ncol(training_set)], type = 'C-classification', kernel = 'linear')
                        
                        
                        y_pred = predict(classifier, newdata = test_set[-ncol(test_set)])
                        cm = table(test_set[, ncol(test_set)], y_pred)
                        matrix <- data.frame(cm)
                        matrix1 <- data.matrix(matrix)
                        
                        h <- data.frame(y_pred)
                        h$y_pred[110:150]
                        a <- (h$y_pred[110:150] == 1)
                        b <- (h$y_pred[110:150] == 0)
                                
                        
                        slice <- c(sum(a == "TRUE"), sum(b == "TRUE"))
                        labels <- c("Positive Sentences","Negative Sentences")
                        library(plotrix)
                        piepercent<- round(100*slice/sum(slice), 1)
                        lbls <- paste(labels,piepercent)
                        lbls <- paste(lbls, "%", seq="")
                        pie3D(slice, labels = lbls, col= rainbow(length(labels)), main="Sentiment Analysis with Suppor Vector Machine (SVM)")
                
                        
                }
        }
        )

}

shinyApp(ui, server)