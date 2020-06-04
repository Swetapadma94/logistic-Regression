library(reard)
affairs<-r
ead.csv("E:\\Assignment\\logistic regression\\affairs.csv")
View(affairs)
install.packages("AER")
library(AER)
install.packages("car")
library(car)
library(plyr)
install.packages("ROCR")
library(ROCR)
affairs<-data("Affairs")
affairs1<-Affairs
View(affairs1)
summary(affairs1)
is.na(affairs)
sum(is.na(affairs))
dim(affairs)
colnames(affairs)
attach(affairs)
table(affairs1$affairs)
affairs1$ynaffairs[affairs1$affairs > 0] <- 1
affairs1$ynaffairs[affairs1$affairs == 0] <- 0
affairs1$gender <- as.factor(revalue(Affairs$gender,c("male"=1, "female"=0)))
affairs1$children <- as.factor(revalue(Affairs$children,c("yes"=1, "no"=0)))
# sum(is.na(claimants))
# claimants <- na.omit(claimants) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
View(affairs1)
class(affairs1)
attach(affairs1)

colnames(affairs1)
## preparing a linear regression
model_lm<- lm(ynaffairs~.,data=affairs1)
summary(model_lm)
pred<-predict(model_lm,affairs1)
pred
plot(affairs1,pred)
# We can no way use the linear regression technique to classify the data
plot(pred)
# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(ynaffairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+
               education+occupation+rating, data = affairs1,family = "binomial")
summary(model)
# To calculate the odds ratio manually we  r going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table 
prob <- predict(model,affairs1,type="response")
summary(model)
confusion<-table(prob>0.5,affairs1$ynaffairs)
confusion
## Model Accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy### 76.53
# Creating empty vectors to store predicted classes based on threshold value
pred_values<-NULL
yes_no<-NULL
pred_values<-ifelse(prob>0.5,1,0)
yes_no<-ifelse(prob>0.5,"yes","no")
# Creating new column to store the above values
affairs1[,"prob"] <- prob
affairs1[,"pred_values"] <- pred_values
affairs1[,"yes_no"] <- yes_no

View(affairs1[,c(1,9:11)])

table(affairs1$ynaffairs,affairs1$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,affairs1$ynaffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))      
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
