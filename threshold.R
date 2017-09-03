#Calculate threshold for LSA results from SEMILAR
#==========================================================

library(data.table)
library(ROCR)
library(gplots)

# Read actual values 
similar <- fread("similarities_Q2.csv", select=c("class"))

# Read predictions values 
results1 <- fread("similarities_Q2.csv", select=c("LSAw-NONE-FREQUENCY"))
results2 <- fread("similarities_Q2.csv", select=c("LSAw-NONE-LOG_FREQUENCY"))
results3 <- fread("similarities_Q2.csv", select=c("LSAw-NONE-NONE"))


#results2 <- fread("similarities_BCR.csv", select=c("LSAw-IDF-FREQUENCY"))
#results4 <- fread("similarities_BCR.csv", select=c("LSAw-IDF-LOG_FREQUENCY"))

#============Method 1============================

# Create predection object
pred <- prediction(results1, similar)

# Evaluation measures
tp <- as.numeric(unlist(pred@tp)) #true positive
fp <- as.numeric(unlist(pred@fp)) #false positive
fn <- as.numeric(unlist(pred@fn)) #false negative
cutoffs <- as.numeric(unlist(pred@cutoffs)) #cutoffs: array of different possible thresholds
f1 <- (2*tp)/(2*tp+fp+fn) #F1-score

# Obtain the threshold for predicted response values based on optimum (maximum) F1-score
threshold <- cutoffs[which.max(f1)]

# If predicted values > threshold, set it to 1, else: set it to 0
fitted.results1 <- ifelse(results1 > threshold,1,0)

# Compare predictions with labels 
misClasificError <- mean(fitted.results1 != similar)

# Show accuracy
print(paste('Accuracy of method 1: ',1-misClasificError))

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
abline(a=0, b= 1)

# AUC value
perf <- performance(pred, measure = "auc")
print(paste('AUC: ',perf@y.values))

fitted.results1 <- as.data.frame(fitted.results1)
#fwrite(fitted.results1, "./Method 1.csv")


#============Method 2============================

pred <- prediction(results2, similar)
tp <- as.numeric(unlist(pred@tp)) #true positive
fp <- as.numeric(unlist(pred@fp)) #false positive
fn <- as.numeric(unlist(pred@fn)) #false negative
cutoffs <- as.numeric(unlist(pred@cutoffs)) #cutoffs: array of different possible thresholds
f1 <- (2*tp)/(2*tp+fp+fn) #F1-score

# Obtain the threshold for predicted response values based on optimum (maximum) F1-score
threshold <- cutoffs[which.max(f1)]
threshold <- threshold +0.2

# If predicted values > threshold, set it to 1, else: set it to 0
fitted.results2 <- ifelse(results2 > threshold,1,0)

# Compare predictions with labels 
misClasificError <- mean(fitted.results2 != similar)

# Show accuracy
print(paste('Accuracy of method 2: ',1-misClasificError))

fitted.results2 <- as.data.frame(fitted.results2)

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
abline(a=0, b= 1)
perf <- performance(pred, measure = "auc")
print(paste('AUC: ',perf@y.values))

#fwrite(fitted.results2, "./Method 2.csv")


#============Method 3============================

pred <- prediction(results3, similar)
tp <- as.numeric(unlist(pred@tp)) #true positive
fp <- as.numeric(unlist(pred@fp)) #false positive
fn <- as.numeric(unlist(pred@fn)) #false negative
cutoffs <- as.numeric(unlist(pred@cutoffs)) #cutoffs: array of different possible thresholds
f1 <- (2*tp)/(2*tp+fp+fn) #F1-score

# Obtain the threshold for predicted response values based on optimum (maximum) F1-score
threshold <- cutoffs[which.max(f1)]
threshold <- threshold +0.3

# If predicted values > threshold, set it to 1, else: set it to 0
fitted.results3 <- ifelse(results3 > threshold,1,0)

# Compare predictions with labels 
misClasificError <- mean(fitted.results3 != similar)

# Show accuracy
print(paste('Accuracy of method 3: ',1-misClasificError))

fitted.results3 <- as.data.frame(fitted.results3)

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
abline(a=0, b= 1)
perf <- performance(pred, measure = "auc")
print(paste('AUC: ',perf@y.values))

#fwrite(fitted.results3, "./Method 3.csv")


#============Method 5============================

pred <- prediction(results4, similar)
tp <- as.numeric(unlist(pred@tp)) #true positive
fp <- as.numeric(unlist(pred@fp)) #false positive
fn <- as.numeric(unlist(pred@fn)) #false negative
cutoffs <- as.numeric(unlist(pred@cutoffs)) #cutoffs: array of different possible thresholds
f1 <- (2*tp)/(2*tp+fp+fn) #F1-score

# Obtain the threshold for predicted response values based on optimum (maximum) F1-score
threshold <- cutoffs[which.max(f1)]
threshold <- threshold + 0.2

# If predicted values > threshold, set it to 1, else: set it to 0
fitted.results4 <- ifelse(results4 > threshold,1,0)

# Compare predictions with labels 
misClasificError <- mean(fitted.results4 != similar)

# Show accuracy
print(paste('Accuracy of method 4: ',1-misClasificError))

fitted.results4 <- as.data.frame(fitted.results4)

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
abline(a=0, b= 1)
perf <- performance(pred, measure = "auc")
print(paste('AUC: ',perf@y.values))


#fwrite(fitted.results4, "./Method 4.csv")



