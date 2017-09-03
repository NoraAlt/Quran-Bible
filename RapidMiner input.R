
library(data.table)

# create dataframe containing the text
mycsvdata <- read.csv("QB data- RMU.csv")

# combine into one long character string for each row
mytxtconcat <- apply(mycsvdata[-(1:1)], 1, paste, collapse=" ")

# make a dataframe with the file names and texts
mytxtdf <- data.frame(filename = csv1[,1], # get the first col for the text file names
                        fulltext = mytxtsconcat1)

# Write one text file for each row of the csv
setwd("/Users/nora/Desktop/R/verses/sim")
invisible(lapply(1:nrow(mytxtdf), function(i) write.table(mytxtdf[i,2],
                                                          file = paste0(mytxtdf[i,1], ".txt"),
                                                          row.names = FALSE, col.names = FALSE,
                                                          quote = FALSE)))



	

