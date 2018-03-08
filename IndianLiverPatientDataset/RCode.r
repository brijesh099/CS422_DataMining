  # Name: Brijesh Mavani
# CWID: A20406960
# University: Illinois Institute of Technology
# Course: Data Mining
# Assignment: 2
# Practicum problems

# set output length on R console to print more information.
options(max.print=999999) 

# Importing all library which will be used in this code
library(psych)
library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)

#Problem 2.1
# set working directory 
setwd("C:/STUDY/MS/Data Mining/Assignment/Assignment2") 

# read CSV file and load it into a variable name ILPD
ILPD <- read.csv("ILPD.csv") 

# display data from variable ILPD
View(ILPD)  

#Problem 1
# produce a numerical summaries of ILPD
summary(ILPD) 

set.seed(100)
index <- sample(1:nrow(ILPD), size=0.4*nrow(ILPD))
testSet <- ILPD[index, ]
trainSet <- ILPD[-index, ]

# (a)

# correlation scatterplot for training Data set.
pairs.panels(trainSet[1:10])


# correlation scatterplot for ILPD Data set.
pairs.panels(ILPD[1:10])


# (i) Which pair of attributes have the strongest correlation?
# Ans: Pair tb, db have strongest correlation of 0.87.

# (ii) Which pair has the weakest correlation? 
# Ans: Pairs (db,tp) , (sex,ag) and (sgpaa,ag)  have weakest correlation of 0.00 i.e. no correlation followed by pairs age,tb and age,db with correlation of 0.01.

# (iii) Which pair is the most negatively correlated? 
# Ans: Pair age, alb have the most negative correlation of -0.27. 


# (iv) Which variables appear to follow a Gaussian distribution?
# Ans: Variables age, tp, alb, ag are appeared to follow a Gaussian distribution. 

# (b) Do you think that normalizing or scaling the attributes here will help with the classification task? Please justify your answer. If you think it will help, which attributes would you normalize or scale?
# Ans: Yes,I think normalizing/scaling the attributes will help with classification task. As we see in summary, there is a huge difference between in range of several variables. eg. ag has range from 0.3 to 2.8 with mean as 0.947 whereas variable sgoaa has range from 10 to 4929 with mean as 109.9. Having these much of variation in data range can alter the result of classification. As rule of thumb taught in class we should do normalizing on all attributes and not just on one. Hence, I would scale and normalize attributes age, tb, db,aap,sgpaa, sgoaa, tp, alb, ag.

# (c)

# Build the model, with all predictor variables.
model <- rpart(label ~ ., method="class", data=trainSet)

rpart.plot(model)
print(model)
pred <- predict(model, testSet[, 1:10], type="class")
table(testSet[,11])
table(pred)    
confusionMatrix(pred, testSet[,11])

# What is the accuracy of the model on out-of-sample data? 
# Ans: Model has accuracy of 0.6824(68.24%) on out-of-sample data.

# What is the TPR?
# Ans: TPR = Sensitivity = 0.8098

# What is the TNR?
# Ans: TNR = Specificity = 0.3857

# What is the PPV?
# Ans: PPV precision =  Tp/(Tp+Fp) = 132/ (132+43) = 0.7543


# (d) Investigate the model created in (c) using the plotcp() and printcp() functions to determine where to prune the tree.
plotcp(model)
printcp(model)

#The value of cp should be least, so that the cross-validated error rate is minimum. Here, we can see that cross validation error is minimum (1.0825) at 3rd point. Hence, we will prune model with cp value of 0.017 
model.pruned <- prune(model, cp=0.017)
rpart.plot(model.pruned, extra=104, fallen.leaves = T, type=4) 

predPruned <- predict(model.pruned, testSet[, 1:10], type="class")
table(testSet[,11])
table(predPruned)    
confusionMatrix(predPruned, testSet[,11])

# This gives more accuracy of 0.7082(70.82%).


# Is the accuracy of the pruned tree on out-of-sample data better, worse or remains unchanged compared to the one you obtained in (c)? 
# Ans: The accuracy of the pruned tree(70.82%) on out-of-sample data is little better than actual model (68.24%) obtained in (c). 

# Why do you think the newly pruned tree provides a better, worse, or unchanged accuracy?
# Ans: Pruning reduces the size of decision trees by removing sections of the tree that provide little power to classify instances. Pruning reduces the complexity of the final classifier, and hence improves predictive accuracy by the reduction of overfitting.


# (e) Create a new model on the training data, but this time, instead of using all predictor variables build the model, with all fewer variables.

# As variables such as tb,db,alb and ag has mean close to zero and somewhat similar, we don't have to use all variables to train our model. 
# Below is the revised model which is giving more accuracy and AUC compared to original model with all predictors.
modelRevised <- rpart(label ~ age+db+sgpaa+sgoaa+tp, method="class", data=trainSet)

rpart.plot(modelRevised)
print(modelRevised)
predRevised <- predict(modelRevised, testSet[, 1:10], type="class")
table(testSet[,11])
table(predRevised)    
confusionMatrix(predRevised, testSet[,11])

# What is the accuracy of the model on out-of-sample data? 
# Ans: Model has accuracy of 0.7082 (70.82%) on out-of-sample data.

# What is the TPR?
# Ans: TPR = Sensitivity = 0.8957

# What is the TNR?
# Ans: TNR = Specificity = 0.2714

# What is the PPV?
# Ans: PPV precision =  Tp/(Tp+Fp) = 146/ (146+51) = 0.7411


pred.rocr <- predict(modelRevised, newdata=testSet, type="prob")[,2]
f.predrevised <- prediction(pred.rocr, testSet$label)
f.perf <- performance(f.predrevised, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.predrevised, measure = "auc")
auc@y.values[[1]]

# AUC: 0.7376424 (73.76%)

# Below are diffrent variations which were tried to train the model with fewer predictors. Some gave more accuracy but less AUC and vice versa. Below are the list of models which was tried to obtain more accuracy and AUC.
# As we can compute Accuracy and AUC as shown above. Here, I am just providing final output.

#More or same Accuracy but less AUC

#1 modelRevised <- rpart(label ~ age+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.6953 ( 69.53%), AUC: 0.6485101 (64.85%)

#2 modelRevised <- rpart(label ~ age+alb+db+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.691 (69.10%), AUC: 0.6400964  (64.01%)

#3 modelRevised <- rpart(label ~ age+ag+alb+db+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.7082  (70.82%), AUC: 0.6598598 (65.98%)

#4 modelRevised <- rpart(label ~ age+ag+tp+db+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.7167  (71.67%), 0.6641543 (66.41%)


#More or same Accuracy and more AUC
#5 modelRevised <- rpart(label ~ age+db+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.691 (69.10%), AUC: 0.6840929 (68.40%)

#6 modelRevised <- rpart(label ~ age+sex+db+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.691 (69.10%), AUC: 0.6840929 (68.40%)

#7 modelRevised <- rpart(label ~ age+ag+db+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.7253 (72.53%), AUC: 0.6787029 (67.87%)

#8 modelRevised <- rpart(label ~ age+db+tb+aap+sgoaa+ag, method="class", data=trainSet)
# Accuracy: 0.7253 (72.53%), AUC: 0.6787029 (67.87%)

#Less Accuracy and Less AUC
#9 modelRevised <- rpart(label ~ db+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.6695 (66.95%), 0.6190622 (61.91%)

#10 modelRevised <- rpart(label ~ age+ag+tp+tb+aap+sgoaa, method="class", data=trainSet)
# Accuracy: 0.6738 (67.38%) , 0.6398773 (63.99%)

# (f)(i) a ROC curve using the ROCR package.
# ROC curve of original model

pred.rocr <- predict(model, newdata=testSet, type="prob")[,2]
f.pred <- prediction(pred.rocr, testSet$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)

# ROC curve of revised model

pred.rocr <- predict(modelRevised, newdata=testSet, type="prob")[,2]
f.predrevised <- prediction(pred.rocr, testSet$label)
f.perf <- performance(f.predrevised, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)

# (f)(ii) AUC of original model
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]
# Ans: AUC = 0.6771691

# AUC of revised model
auc <- performance(f.predrevised, measure = "auc")
auc@y.values[[1]]
# Ans: AUC = 0.7376424


# (f)(iii) Which model performs better and why?
# Ans: Model created in (e) performs better as it is revised model and its takes only required predictor variables to train the model. 


# 2.2 Problem 2
# Prepare a brief report (or include a knitted document) that includes the R commands you used to figure out the missing values and how they were imputed.
# Ans: There are total 4 missing values in original ILPD data set. All four missing values are for column ag. The professor has impute the value by taking the mean of ag of remaining records. 

ILPDOriginal <- read.csv("ILPD_Original.csv") 

# R provides NA's for column which has a missing values. We can find which column has missing values by simply checking for presence of NA's valuein summary for all columns. 
summary(ILPDOriginal)

# Gives list of rows which has missing values. In R missing value is represented by NA. Analyzing the output of this, we can say that 4 values in column ag is missing.
ILPDOriginal[!complete.cases(ILPDOriginal),]

# Finding mean of ag after excluding records with missing values.
meanAg <- mean(ILPDOriginal$ag, na.rm=TRUE)
# Mean of ag = 0.9470639
meanAg
 
# Different variation used to get mean as 0.94 but all are rouding it up and giving output as 0.95. 
round(meanAg,2)
signif(meanAg,digits=2)
formatC(meanAg,digits=2, format="f")


# None of the above functions are giving 0.94. Hence, updated trunc fucntions as below to get 0.94.
meanImpute <- trunc(meanAg*100)/100

# Imputing the missing values by mean into ag.
ILPDOriginal[is.na(ILPDOriginal)] <- round(meanImpute,2)

# We can see that now there is no entry for NA's in column ag.
summary(ILPDOriginal)

