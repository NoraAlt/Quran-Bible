library(data.table)
library(h2o)
library(ROCR)
library(gplots)

h2o.init(nthreads = -1)

ts1 <- fread("World English Bible-No TR.csv", select=c("Text"))
#ts1 <- fread("Quran-maududi.csv", select=c("Text"))


print("Some verses cleanup")
# It is important to remove "\n" -- it appears to cause a parsing error when converting to an H2OFrame
#ts1[,":="(Text=gsub("'|\"|'|???|???|\"|\n|,|\\.|???|\\?|\\+|\\-|\\/|\\=|\\(|\\)|???", "", Text))]
#Remove !" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
ts1[,":="(Text=gsub("[[:punct:]]", " ", Text))]

ts1[,":="(Text=gsub("  ", " ", Text))]



#ts1[,":="(Text1=gsub("[][!#$%()*,.:;<=>@^_`|~.{}]", "", Text1), Text2=gsub("[][!#$%()*,.:;<=>@^_`|~.{}]", "", Text2))] 

print("get list of unique verses")
texts <- as.data.table(ts1)
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
               "havent","having","he","her","furthermore","here","heres","hers","herself","himself","hows","isnt",
               "it's","its","itself","let's","me","more","indeed","most","myself","no","nor","off","once","only",
               "other","ought","our","ours","ourselves","out","over","own","same","should","shouldnt",
               "thats","theirs","themselves","therefore","theres","those","through","too","under","up","very","wasn't",
               "whats","whens","whos","wont","would","wouldnt","upon","shall","also")


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

print("Break verses into sequence of words")
words <- tokenize(texts.hex$Text)


print("Build word2vec model")

w2v.model <- h2o.word2vec(words
                          , model_id = "w2v_model"
                          , vec_size = 100
                          , min_word_freq = 5
                          , window_size = 5
                          , init_learning_rate = 0.025
                          , sent_sample_rate = 0
                          , epochs = 20) 



#the number of training iterations to run, only one epoch to save time

h2o.rm('texts.hex') # Remove the h2o Big Data object to empty memory, no longer needed

print("Find most related word:")
h2o.findSynonyms(w2v.model, "lord", count = 5)


#======================================================================================
#Heatmap drawing
#======================================================================================

library(ggplot2)
library(plyr)
library(arm)
library(reshape2)
nba <- fread("heat.csv")
nba$V1 <- with(nba, reorder(Term, PTS))
nba.m <- melt(nba)
nba.m <- ddply(nba.m, .(variable), transform,
               rescale = value)
(p <- ggplot(nba.m, aes(variable, Term)) + geom_tile(aes(fill = rescale),
                                                     colour = "white") + scale_fill_gradient(low = "white",
                                                                                             high = "steelblue")+geom_text(aes(label=round(rescale,2))))

