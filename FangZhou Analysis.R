library(tidyverse)
library(ggplot2)
library(mice)
library(corrplot)
library(car)
library(pROC)
library(caret)

# read in file
df <- read.csv2('bank-additional-full-1.csv', sep = ';')

#convert group 4 columns to numeric
df[16:20] <- sapply(df[16:20],as.numeric)

#correlation matrix
corrplot(cor(df[sapply(df,is.numeric)], method = "pearson"))

#add row index
#df <- tibble::rowid_to_column(df,"ID")

#check NAs
na_table <- sort(sapply(df, function(x) sum(is.na(x))))
#no numeric NAs, check labeled as "unknown"
sort(sapply(df, function(x) sum(x=="unknown")))

#mice imputation as an alternative
df_clean <- df
df_clean[df  == "unknown"] <- NA
df_clean <- mutate_if(df_clean, is.character, as.factor)

md.pattern(df_clean)
mi_imp <- mice(df_clean, method = 'pmm', m = 5, seed = 123)
df_imp <- complete(mi_imp,1)


#cross-tab analysis
df_cat <- select_if(df_imp, is.factor)

sapply(df_cat, function(x) chisq.test(table(x,df_cat$y)))

# campaign & sales
ggplot(df_imp, aes(x=campaign, color=y))+
  geom_density()+coord_cartesian(xlim = c(0,15))

# linear model
df_pred <- df_imp[,1:15]
df_pred["y"] <- df_imp["y"]
glm1 <- glm(y ~., data = df_pred, family = 'binomial')
exp(coef(glm1))



#redo with new treatment for missing values
df_new <- df
df_new$na_count <- rowSums(df_new == "unknown")
#delete rows with 3+ nas
df_new <- df_new[df_new[,"na_count"]<3,]
df_new$na_count <- NULL

#cross-tab analysis
df_new <- mutate_if(df_new, is.character, as.factor)
df_cat <- select_if(df_new, is.factor)
sapply(df_cat, function(x) chisq.test(table(x,df_cat$y)))

# density plot of sale by campaign
ggplot(df_new, aes(x=campaign, y=..scaled.., group=y, fill=y))+
  geom_density(alpha=0.5)+coord_cartesian(xlim = c(0,15))


#logistic regression with group 1-3 as predictors
df_lr <- df_new[,1:15]
df_lr["y"] <- df_new["y"]
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
df_4 <- df_new
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


