# Import all required libraries
library(data.table)
library(h2o)
library(ROCR)
library(gplots)

# Start connect to H2O
h2o.init(nthreads = -1)

# Importing training data
ts1 <- fread("QuranTrain-m.csv", select=c("id","Text1","Text2","is_similar"))


# Cleaning
# ===================================================================================================== #

# Remove !" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
ts1[,":="(Text1=gsub("[[:punct:]]", " ", Text1),
          Text2=gsub("[[:punct:]]", " ", Text2))]

ts1[,":="(Text1=gsub("\n", "", Text1),
          Text2=gsub("\n", "", Text2))]

# Remove extra whitspaces
ts1[,":="(Text1=gsub("  ", " ", Text1),
         Text2=gsub("  ", " ", Text2))]

# Term categorization
# ===================================================================================================== #
#Words: (Lord,allah,Yahweh) all grouped as God
ts1[,":="(Text1=gsub("allah|lord|lords|gods|yahweh", "god", Text1, ignore.case = TRUE),
          Text2=gsub("allah|lord|lords|gods|yahweh", "god", Text2, ignore.case = TRUE))]
#Words: (Satan,devil,evil,jinn,demon) all grouped as Evil
ts1[,":="(Text1=gsub("satan|satans|devil|devils|evils|jinn|demon|demons", "evil", Text1, ignore.case = TRUE),
         Text2=gsub("satan|satans|devil|devils|evils|jinn|demon|demons", "evil", Text2, ignore.case = TRUE))]
#Words: (Surah) converted to Verses
ts1[,":="(Text1=gsub("surah", "verses", Text1, ignore.case = TRUE),
          Text2=gsub("surah", "verses", Text2, ignore.case = TRUE))]
#Words: (zakah,zakat) converted to charity
ts1[,":="(Text1=gsub("zakah|zakat", "charity", Text1, ignore.case = TRUE),
          Text2=gsub("zakah|zakat", "charity", Text2, ignore.case = TRUE))]
#Words: (salat) converted to prayer
ts1[,":="(Text1=gsub("salat", "prayer", Text1, ignore.case = TRUE),
        Text2=gsub("salat", "prayer", Text2, ignore.case = TRUE))]


# Get list of unique verses
texts <- as.data.table(rbind(ts1[,.(verse=Text1)], ts1[,.(verse=Text2)]))
texts <- unique(texts)
texts.hex <- as.h2o(texts, destination_frame = "texts.hex", col.types=c("String"))

STOP_WORDS = c("us","i","into","them","such","their","then","these","him","were","will","you","unto",
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
               "thats","theirs","themselves","theres","those","through","too","under","up","very","wasn't",
               "whats","whens","whos","wont","would","wouldnt","upon","shall","also","may","even")


# Tokenization
# ===================================================================================================== #
tokenize <- function(sentences, stop.words = STOP_WORDS)
{
  tokenized <- h2o.tokenize(sentences, "\\\\W+")
  
  # convert to lower case
  tokenized.lower <- h2o.tolower(tokenized)
  # remove short words (less than 2 characters)
  tokenized.lengths <- h2o.nchar(tokenized.lower)
  tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  # remove words that contain numbers
  tokenized.words <- tokenized.lower[h2o.grep("[0-9]", tokenized.lower, invert = TRUE, output.logical = TRUE),]
  
  # remove stop words
  tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}

# Break verses into sequence of words
words <- tokenize(texts.hex$verse)


# Build word2vec model of training data
# ===================================================================================================== #
vectors <- 50 
e <- 20 
w2v.model <- h2o.word2vec(words
                          , model_id = "w2v_model"
                          , vec_size = vectors # The dimensionality of vector representation 
                          , min_word_freq = 5
                          , window_size = 5
                          , init_learning_rate = 0.025
                          , sent_sample_rate = 0
                          , epochs = e) # The number of training iterations to run

h2o.rm('texts.hex') # Remove the h2o big data object to empty memory (no longer needed)

# Sanity check: find synonyms for the word 'lord'
h2o.findSynonyms(w2v.model, "lord", count = 5)

# Transform verses to vectors by averaging the words vectors, so we get vector for each verse
text_all.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")

# Convert to data.table and merge results
text_all.vecs <- as.data.table(text_all.vecs)
texts_all <- cbind(texts, text_all.vecs)
ts1 <- merge(ts1, texts_all, by.x="Text2", by.y="verse", all.x=TRUE, sort=FALSE)
ts1 <- merge(ts1, texts_all, by.x="Text1", by.y="verse", all.x=TRUE, sort=FALSE)
colnames(ts1)[5:ncol(ts1)] <- c(paste0("t1_vec_C", 1:vectors), paste0("t2_vec_C", 1:vectors))

# Output verses vectors for training data
fwrite(ts1, "./train_vectors.csv")


# Until here we get vector for each row in the traing dataset
# Now we need to train GBM model to get the results of the prediction


# ====================================================================================================== #
# Training GBM
# ====================================================================================================== #

ts1<-as.data.frame(ts1)
localH2O<-h2o.init()
h2o_data<-as.h2o(ts1)
h2o_data <- h2o.na_omit(h2o_data) # Important since GBM does not support NA values

# GBM trained on 80% of data and validated on 20%.
data.split <- h2o.splitFrame(h2o_data, ratios = 0.8)

# Specify the columns of vectors representation
gbm <- subset(ts1, select=c(5:104))

# Build a basic GBM model
gbm.model <- h2o.gbm(x = names(gbm), y = "is_similar",
                     training_frame = data.split[[1]], validation_frame = data.split[[2]])


# Predict response values based on GBM model
fitted.results <- h2o.predict(gbm.model, newdata = h2o_data)
fitted.results <- as.data.frame(fitted.results)


# Evaluatin
#===========================================================================================================

# Create a prediction object
pred <- prediction(fitted.results, as.data.frame(h2o_data$is_similar)) 

# Evaluation measures
tp <- as.numeric(unlist(pred@tp)) #true positive
fp <- as.numeric(unlist(pred@fp)) #false positive
fn <- as.numeric(unlist(pred@fn)) #false negative
cutoffs <- as.numeric(unlist(pred@cutoffs)) #array of different possible thresholds
f1 <- (2*tp)/(2*tp+fp+fn) #F1-score

# Obtain the threshold for predicted response values based on optimum F1-score
threshold <- cutoffs[which.max(f1)]

# If predicted values > threshold, set it to 1, else: set it to 0
fitted.results <- ifelse(fitted.results > threshold,1,0)

# Compare prediction with actual values (labels)
misClasificError <- mean(fitted.results != as.data.frame(h2o_data$is_similar))

# Show accuracy
print(paste('GBM Training Accuracy',1-misClasificError))

# Precision
precision <- tp[which.max(f1)]/(tp[which.max(f1)]+fp[which.max(f1)])
print(paste('GBM Precision',precision))

# Recall
recall <- tp[which.max(f1)]/(tp[which.max(f1)]+fn[which.max(f1)])
print(paste('GBM Recall',recall))

# F1 score
print(paste('GBM F1 score',f1[which.max(f1)]))

# Function to create performance object for ROC curve
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr") 
plot(roc.perf) #Show plot of ROC
abline(a=0, b= 1, lty = 2)

# AUC value
perf <- performance(pred, measure = "auc")
print(paste('AUC: ',perf@y.values))



# ====================================================================================================== #

# Prediction on test data
# ====================================================================================================== #

# Importing test data
ts2 <- fread("Q1.csv", select=c("id","Text1","Text2","is_similar"))


# Cleaning
# ===================================================================================================== #

#Remove !" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
ts2[,":="(Text1=gsub("[[:punct:]]", " ", Text1),
          Text2=gsub("[[:punct:]]", " ", Text2))]

ts1[,":="(Text1=gsub("\n", "", Text1),
          Text2=gsub("\n", "", Text2))]

# Remove extra whitspaces
ts1[,":="(Text1=gsub("  ", " ", Text1),
          Text2=gsub("  ", " ", Text2))]


# Term categorization
# ===================================================================================================== #
#Words: (Lord,allah,Yahweh) all grouped as God
ts2[,":="(Text1=gsub("allah|lord|lords|gods|yahweh", "god", Text1, ignore.case = TRUE),
          Text2=gsub("allah|lord|lords|gods|yahweh", "god", Text2, ignore.case = TRUE))]
#Words: (Satan,devil,evil,jinn,demon) all grouped as Evil
ts2[,":="(Text1=gsub("satan|satans|devil|devils|evils|jinn|demon|demons", "evil", Text1, ignore.case = TRUE),
          Text2=gsub("satan|satans|devil|devils|evils|jinn|demon|demons", "evil", Text2, ignore.case = TRUE))]
#Words: (Surah) converted to Verses
ts2[,":="(Text1=gsub("surah", "verses", Text1, ignore.case = TRUE),
          Text2=gsub("surah", "verses", Text2, ignore.case = TRUE))]
#Words: (zakah,zakat) converted to charity
ts2[,":="(Text1=gsub("zakah|zakat", "charity", Text1, ignore.case = TRUE),
          Text2=gsub("zakah|zakat", "charity", Text2, ignore.case = TRUE))]
#Words: (salat) converted to prayer
ts2[,":="(Text1=gsub("salat", "prayer", Text1, ignore.case = TRUE),
          Text2=gsub("salat", "prayer", Text2, ignore.case = TRUE))]


# Get list of unique verses
texts.test <- as.data.table(rbind(ts2[,.(verse=Text1)], ts2[,.(verse=Text2)]))
texts.test <- unique(texts.test)
texts.test.hex <- as.h2o(texts.test, destination_frame = "texts.test.hex", col.types=c("String"))

# Tokenization: break verses into sequence of words
words.test <- tokenize(texts.test.hex$verse)

# Build word2vec model of test data
# ===================================================================================================== #
w2v.test.model <- h2o.word2vec(words.test
                               , model_id = "w2v_model"
                               , vec_size = vectors # The dimensionality of vector representation 
                               , min_word_freq = 5
                               , window_size = 5
                               , init_learning_rate = 0.025
                               , sent_sample_rate = 0
                               , epochs = e) # The number of training iterations to run

h2o.rm('texts.test.hex') # Remove the h2o big data object to empty memory (no longer needed)

# Sanity check: find synonyms for the word 'lord'
print(h2o.findSynonyms(w2v.test.model, "lord", count = 5))

# Transform verses to vectors by averaging the words vectors, so we get vector for each verse
text_all.test.vecs <- h2o.transform(w2v.test.model, words.test, aggregate_method = "AVERAGE")

# Convert to data.table and merge results
text_all.test.vecs <- as.data.table(text_all.test.vecs)
texts_all_test <- cbind(texts.test, text_all.test.vecs)
ts2 <- merge(ts2, texts_all_test, by.x="Text2", by.y="verse", all.x=TRUE, sort=FALSE)
ts2 <- merge(ts2, texts_all_test, by.x="Text1", by.y="verse", all.x=TRUE, sort=FALSE)
colnames(ts2)[5:ncol(ts2)] <- c(paste0("t1_vec_C", 1:vectors), paste0("t2_vec_C", 1:vectors))
ts2 <- na.omit(ts2)

h2o_test_data <- as.h2o(ts2)
h2o_test_data <- h2o.na_omit(h2o_test_data) # Important since GBM does not support NA values

# Predict response values based on GBM model
predicted.results <- h2o.predict(gbm.model, newdata=h2o_test_data[,5:104])
predicted.results <- as.data.frame(predicted.results)

# If predicted values > threshold, set it to 1, else: set it to 0
predicted.results <- ifelse(predicted.results > threshold,1,0)

ts2<- cbind(ts2, predicted.results)
colnames(ts2)[105] <- c("predection")

# Output verses vectors and prediction for test data
fwrite(ts2, "./test_vectors.csv")


# Evaluatin
#===========================================================================================================

# Create a prediction object
pred <- prediction(predicted.results, as.data.frame(h2o_test_data$is_similar))

# Evaluation measures
tp <- as.numeric(unlist(pred@tp)) #true positive
fp <- as.numeric(unlist(pred@fp)) #false positive
fn <- as.numeric(unlist(pred@fn)) #false negative
f1 <- (2*tp)/(2*tp+fp+fn) #F1-score

# Compare prediction with actual values (labels)
misClasificError <- mean(predicted.results != as.data.frame(h2o_test_data$is_similar))

# Show accuracy
print(paste('GBM Training Accuracy',1-misClasificError))

# Precision
precision <- tp[which.max(f1)]/(tp[which.max(f1)]+fp[which.max(f1)])
print(paste('GBM Precision',precision))

# Recall
recall <- tp[which.max(f1)]/(tp[which.max(f1)]+fn[which.max(f1)])
print(paste('GBM Recall',recall))

# F1 score
print(paste('GBM F1 score',f1[which.max(f1)]))

# Precision - Recall Curve or ROC curve
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1, lty = 2)

# AUC value
perf <- performance(pred, measure = "auc")
print(paste('AUC: ',perf@y.values))





