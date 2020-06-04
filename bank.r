bank<-read.csv("E:\\Assignment\\logistic regression\\bankdata.csv")
View(bank)
EDU_Data <- read.csv(file.choose())
attach(bank)
bank <- cbind(age,default,balance,housing,loan,duration,campaign,pdays,
                    previous,poutfailure,poutother,poutsuccess,poutunknown,con_cellular,con_telephone
                    ,con_unknown,divorced,married,single,joadmin.,joblue.collar,joentrepreneur,
                    johousemaid,jomanagement,joretired,joself.employed,joservices,jostudent,jotechnician
                    ,jounemployed,HasFD =y)
View(bank)
colnames(bank)
bank<-as.data.frame(bank)
class(bank)
# Preparing a linear regression 
mod_lm <- lm(HasFD~.,data=bank)
pred1 <- predict(mod_lm,bank)
plot(age,pred1)
plot(pred1)
# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(HasFD~.,data=bank,family = "binomial")
exp(coef(model))
prob <- predict(model,bank,type="response")
summary(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank$HasFD)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no

# View(bank_data1[,c(1,31,36:38)])

table(bank$HasFD,bank$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

library(ROCR)
rocrpred<-prediction(prob,bank$HasFD)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
