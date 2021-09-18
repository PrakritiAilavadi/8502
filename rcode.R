library(tidyverse)
library(ggplot2)
library(mice)
library(corrplot)
library(car)
library(pROC)
library(caret)

# read in file
#df_imp <- read.csv2('bank-additional-full-1.csv', sep = ';')


#cross-tab analysis
df_cat <- select_if(bank.MICE, is.factor)

sapply(df_cat, function(x) chisq.test(table(x,df_cat$y)))


# linear model
df_pred <- bank.MICE[,1:15]
df_pred["y"] <- bank.MICE["y"]
glm1 <- glm(y ~., data = df_pred, family = 'binomial')
exp(coef(glm1))



#logistic regression with group 1-3 as predictors
df_lr <- df_imp[,1:15]
df_lr["y"] <- df_imp["y"]
df_lr["pdays"] <- NULL
df_lr["loan"] <- NULL
glm2 <- glm(y ~., data = df_lr, family = 'binomial')
vifs <- vif(glm2)
# found loanunknown causing perfect collinearility, 
#so I have deleted the loan variable as it was not 
#significant at first place
#with the vif, pdays have 11.47

#psudo r^2
1-glm2$deviance/glm2$null.deviance

#roc
prob <- predict(glm2, type = c("response"))
df_lr["prob"] <- prob
plot(roc(y ~ prob, data = df_lr))
auc(df_lr$y, df_lr$prob)
#0.9146

#confusion matrix
pred <- as.factor(ifelse(predict(glm2, df_lr, type="response")>0.5,"yes","no"))
confusionMatrix(pred, df_lr$y)

#change cutoff
cutoffs <- (1:99)/100
df_cutoff <- data.frame(cutoffs)
accuracies <- 0/(1:99)
for (i in 1:length(cutoffs)){
  pred_result <- as.factor(ifelse(predict(glm2, df_lr, type="response")>cutoffs[i],"yes","no"))
  cm <- confusionMatrix(pred_result, df_lr$y)
  accuracies[i] <- cm$overall['Accuracy']
}
df_cutoff['accuracy'] <- accuracies
#highest accuracy found at cutoff=0.38
#calculate AUC with cutoff=0.38
final_pred <- as.factor(ifelse(predict(glm2, df_lr, type="response")>0.38,"yes","no"))
confusionMatrix(final_pred, df_lr$y)
#accuracy:0.9081
#no info rate: 0.887
#sensitivity: 0.9645
#specificity: 0.4648
#balanced accuracy: 0.7147
#this also has high sensitivity which is desired


#add group 4
df_4 <- df_imp
df_4["loan"] <- NULL
df_4["pdays"] <- NULL
df_4["nr.employed"] <- NULL
df_4["emp.var.rate"] <- NULL
glm3 <- glm(y ~., data = df_4, family = 'binomial')
vifs_new <- vif(glm3)

#check performance with the new model
#psudo r^2
1-glm3$deviance/glm3$null.deviance
#0.40347

#roc
prob_new <- predict(glm3, type = c("response"))
df_4["prob"] <- prob_new
plot(roc(y ~ prob, data = df_4))
auc(df_4$y, df_4$prob)
#0.9324

#confusion matrix
pred_4 <- as.factor(ifelse(predict(glm3, df_4, type="response")>0.5,"yes","no"))
confusionMatrix(pred_4, df_4$y)
#optimal accuracy
accuracies_4 <- 0/(1:99)
for (i in 1:length(cutoffs)){
  pred_result <- as.factor(ifelse(predict(glm3, df_4, type="response")>cutoffs[i],"yes","no"))
  cm <- confusionMatrix(pred_result, df_4$y)
  accuracies_4[i] <- cm$overall['Accuracy']
}
df_cutoff['accuracy_g4'] <- accuracies_4
#best accuracy:0.9116, not worth



# read in file
df <- read.csv2('bank-knn.csv', sep = ',')


