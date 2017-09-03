library(tm) #for text mining
library(SnowballC) #for stemmer
library(ggplot2) #for visualization
library(wordcloud) #for visualization

# Import data (the Quran and the Bible texts)
corpus <- Corpus(DirSource("/Users/nora/Desktop/R/Text/All"))


STOP_WORDS = c("yahweh","allah","god","lord","us","into","them","such","their","then","these","him","were","will","you","unto",
               "can","re","there","all","we","one","the","an","of","or","in","for","on","how","but",
               "is","not","with","as","was","whereas","if","they","are","this","and","it","have","from",
               "at","my","be","by","not","that","to","so","about","am","any","he","his","she","some",
               "than","until","what","when","where","which","while","who","whom","why", "thou", "your", 
               "yours","yourself","yourselves","again","against","arent", "because", "been","before",
               "being","between", "both","cant","cannot","could","couldnt","did","didnt","do","does",
               "doesnt","doing","dont","down","during","each","few","further","had","hadnt","has","hasnt",
               "havent","having","he","her","here","heres","hers","herself","himself","hows","isnt",
               "it's","its","itself","let's","me","more","most","myself","no","nor","off","once","only",
               "other","ought","our","ours","ourselves","out","over","own","same","should","shouldnt",
               "thats","theirs","themselves","therefore","theres","among","those","through","too","under","up","very","wasn't",
               "whats","whens","whos","wont","would","wouldnt","upon","shall","said","also","may","even")


#===================================================================================================
#Cleaning
corpus <- tm_map(corpus, tolower) # Convert text to lower cases
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, STOP_WORDS) # Remove my own stopwords list
corpus <- tm_map(corpus, stemDocument) # Stemming
corpus <- tm_map(corpus, stripWhitespace)

#Remove special characters
removeSpecChar <- function(x) gsub("[^[:alnum:]///' ]|*\\{.*?\\} *", " ", x)
corpus <- tm_map(corpus, removeSpecChar)

# Create Term-document matrix containing the frequency of the words 
dtm <-DocumentTermMatrix(corpus)

# Convert dtm to matrix
mat <- as.matrix(dtm)

# Defie 'Series x' and 'Series Y'.
cb <- data.frame(Quran = mat['Quran-en.maududi.txt',], Bible = mat['World English Bible.txt',])

# To balance the frequency between both texts
# frequency of the words converted to the percentage of word occurrences in each text 
cb$Quran <- cb$Quran/sum(cb$Quran)*100
cb$Bible <- cb$Bible/sum(cb$Bible)*100

# Only words with percentages higher than 0.3 are shown in the plot
cb <- subset(cb, cb$Quran > 0.3 | cb$Bible > 0.3)

# Create plot for Comparison of frequent terms usage between the Quran and the Bible
png(file="MyPlot.jpg",width=2000,height=1700,res=300)
ggplot(cb, aes(Quran, Bible)) +
  geom_text(label = rownames(cb),
            position=position_jitter())+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

dev.off()



#===================================================================================================
#Cleaning
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, STOP_WORDS) # Remove my own stopwords list
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

#Remove special characters
removeSpecChar <- function(x) gsub("[^[:alnum:]///' ]|*\\{.*?\\} *", " ", x)
corpus <- tm_map(corpus, removeSpecChar)

#convert to a plain text file
corpus <- tm_map(corpus, PlainTextDocument)

#Create a term document matrix, remove words with fewer than 3 characters
tdm1 <- TermDocumentMatrix(corpus, control=list(wordLengths=c(3,Inf)))

#Create document term matrix, remove words with fewer than 3 characters
#dtm1 <- DocumentTermMatrix(corpus, control=list(wordLengths=c(3,Inf)))

#organize by frequency
#dtmsort <- sort(colSums(as.matrix(dtm1)), decreasing=TRUE)
#most frequent
#head(dtmsort)
#least frequent
#tail(dtmsort)
#the twenty most frequent words:
#dtmsort[1:20]

# Obtain the most frequent words
freq.terms <- findFreqTerms(tdm1, lowfreq=1500)
term.freq <- rowSums(as.matrix(tdm1))
term.freq <- subset(term.freq, term.freq>=1500)
df1 <- data.frame(term=names(term.freq),freq=term.freq)

#Create plot of top frequent words 
ggplot(df1, aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() + theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))

barplot(df1[1:30,]$freq, las = 2, names.arg = df1[1:30,]$term, horiz= TRUE,
        col ="lightblue", main ="Most frequent words",
        xlab = "Word frequencies")  

#word cloud
m1 <- as.matrix(tdm1)
word.freq <- sort(rowSums(m1), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=100,random.order=F, scale=c(10, .1),
          colors=brewer.pal(8, "Dark2"))

wordcloud(words=names(word.freq), freq=word.freq, min.freq = 150,
          max.words=200, random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))

#Remove zero rows
#dtm1 <- dtm1[apply(dtm1[,-1], 1, function(x) !all(x==0)),]
#lda <- LDA(dtm1, k=5) #find 5 topics
#(term <- terms(lda, 8)) #first 8 terms of every topic
