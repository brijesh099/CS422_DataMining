# Name: Brijesh Mavani
# CWID: A20406960
# University: Illinois Institute of Technology
# Course: Data Mining
# Assignment: 3
# Practicum problems

# set output length on R console to print more information.
options(max.print=999999) 

# Importing all library which will be used in this code
#library(psych)
#library(rpart)
library(caret)
#library(rpart.plot)
library(ROCR)
library(e1071)
library(arules)
library(factoextra)
library(NbClust)
library(cluster)
library(FunCluster)

#Problem 2.1

# set working directory 
setwd("C:/STUDY/MS/Data Mining/Assignment/Assignment3") 

# read CSV file and load it into a variable name ILPD
ILPD <- read.csv("ILPD.csv",header=T) 

# display data from variable ILPD
View(ILPD)  


# Problem 1: Naive Bayes classification (2.5 points)
# produce a numerical summaries of ILPD
summary(ILPD) 

set.seed(100)
index <- sample(1:nrow(ILPD), size=0.4*nrow(ILPD))
testSet <- ILPD[index, ]
trainSet <- ILPD[-index, ]
summary(testSet)

# Best model from assignment 2

# (e) Create a new model on the training data, but this time, instead of using all predictor variables build the model, with all fewer variables.
# 
# As variables such as tb,db,alb and ag has mean close to zero and somewhat similar, we don't have to use all variables to train our model. 
# Below is the revised model which is giving more accuracy and AUC compared to original model with all predictors.
# modelRevised <- rpart(label ~ age+db+sgpaa+sgoaa+tp, method="class", data=trainSet)
# 
# rpart.plot(modelRevised)
# print(modelRevised)
# predRevised <- predict(modelRevised, testSet[, 1:10], type="class")
# table(testSet[,11])
# table(predRevised)    
# confusionMatrix(predRevised, testSet[,11])
# accuracy = 0.7082 (70.82%) 
# pred.rocr <- predict(modelRevised, newdata=testSet, type="prob")[,2]
# f.predrevised <- prediction(pred.rocr, testSet$label)
# f.perf <- performance(f.predrevised, "tpr", "fpr")
# plot(f.perf, colorize=T, lwd=3)
# abline(0,1)
# auc <- performance(f.predrevised, measure = "auc")
# auc@y.values[[1]]
# AUC: 0.7376424 (73.76%)


# Take a look at 2.1(f) from Homework 2, which asked you to choose the best model. Using the same predictor
# variables you used for the best model in Homework 2, create a Naive Bayes classification model.
nbmodel <- naiveBayes(as.factor(label) ~ age+db+sgpaa+sgoaa+tp, method="class", data=trainSet)

# Information about the model
print(nbmodel)  

# Running the prediction on the test dataset.
prednb <- predict(nbmodel, testSet, type="class")

table(testSet[,11])  # Show real class distribution
table(prednb)       # Show predicted class distribution
table(prednb, testSet[,11]) # Manual confusion matrix

# Create the confusion matrix
confusionMatrix(prednb, testSet[, 11])

# Accuracy: 0.5193

# (a) ROC curve using the ROCR package
pred.rocr <- predict(nbmodel, newdata=testSet, type="raw") # Posterior probabilities
f.predRevised <- prediction(pred.rocr[,2], testSet$label)
f.perf <- performance(f.predRevised, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)

# (b) the AUC
auc <- performance(f.predRevised, measure = "auc")
auc@y.values[[1]]

#AUC: 0.7314636


# Does the Naive Bayes classifier perform better or worse than your best tree-based classifier from Homework 2?
# Ans: Naive Bayes classifier has poor accuracy(0.5193 or 51.93%) compare to the original tree based classifier which has Accuracy of 0.7082 (70.82%). However, the AUC is almost similar in both cases. The Naive Bayes classifer has AUC of 0.7314636 (73.14%) while tree based classifier has little bit more AUC with 0.7376424 (73.76%). Hence, Naive Bayes classifier performance worst than model built in Homework #2. This is because Naive Bayes considers variable to be conditionally independent whereas that is not a case in this data set.



# 2.2 Problem 2: Association rules

Groceries <- read.transactions("groceries.csv", sep=',') 
summary(Groceries)
data(Groceries)
# inspect (Groceries)
itemFrequencyPlot(Groceries,topN=10, type="absolute",main='Item frequency, absolute')
table <- crossTable(Groceries)
table['whole milk','whole milk']

# (a) Which item is the most frequently bought and what is its frequency?
# Ans: Whole milk bought frequently and its frequency is 2513

barplot(sort(table(unlist(LIST(Groceries))))[1:3],las=1, horiz=F,
        main='Frequency, absolute')

table['baby food','baby food']
table['sound storage medium','sound storage medium']
# (b) Which item is the least frequently bought and what is its frequency?
# Ans: Baby food and Sound storage medium bought least frequently and its frequency is 1.

rules <- apriori(Groceries, parameter = list(confidence=0.01,support=0.01))
# This creates 610 rules but it has several rules which has antecedent as null. Therefore, rules created here are not optimal. 

rules <- apriori(Groceries, parameter = list(support=0.004))
# This doesn't create any rules.

rules <- apriori(Groceries, parameter = list(support=0.003))
# This creates only single rule.

rules <- apriori(Groceries, parameter = list(support=0.002))
# This creates 11 rules. But this rule count is too small for any analysis.

rules <- apriori(Groceries, parameter = list(support=0.001))
# This creates 410 rules. This rule count is optimal for doing an analysis. Hence, I will use rules created with missup=0.001. 


# (c) At what level of support are the rules generated?
# At minsup = 0.003 rules are getting generated. As seen above, there are 0 rules generated when using minsup as 0.004. The single rule is generated at support of 0.003 which is not very much useful. Similarly with minsup=0.002 there are only 11 rules generated. Hence, missup is increased for generating more rules. minsup=0.001 generates 410 rules. This can be useful for performing any analysis.   


# (d) What are the top 5 rules, sorted by support?
top.support <- sort(rules, decreasing = TRUE, by = "support")
inspect(head(top.support,5))

# Ans: Top 5 rules, sorted by support are: 
#      lhs                   rhs                support 
# [1] {citrus fruit,                                       
#      tropical fruit,                                     
#      root vegetables,                                    
#      whole milk}       => {other vegetables}  0.003152008 
# [2] {other vegetables,                                   
#      curd,                                               
#      domestic eggs}    => {whole milk}        0.002846975 
# [3] {hamburger meat,                                     
#      curd}             => {whole milk}        0.002541942 
# [4] {herbs,                                              
#      rolls/buns}       => {whole milk}        0.002440264 
# [5] {tropical fruit,                                     
#      herbs}            => {whole milk}        0.002338587 


# (e) What are the top 5 rules, sorted by confidence?
top.confidence <- sort(rules, decreasing = TRUE, by = "confidence")
inspect(head(top.confidence,5))
# Ans: Top 5 rules, sorted by confidence are: 

#      lhs                     rhs              confidence 
# [1] {rice,                                        
#      sugar}              => {whole milk}         1
# [2] {canned fish,                                 
#      hygiene articles}   => {whole milk}         1
# [3] {root vegetables,                             
#      butter,rice}        => {whole milk}         1
# [4] {root vegetables,                             
#      whipped/sour cream,                          
#      flour}              => {whole milk}         1
# [5] {butter,                                      
#      soft cheese,                                 
#      domestic eggs}      => {whole milk}         1


# (f) What are the top 5 rules, sorted by lift?
top.lift <- sort(rules, decreasing = TRUE, by = "lift")
inspect(head(top.lift,5))

# Ans: Top 5 rules, sorted by lift are:

#      lhs                        rhs               lift 		
# [1] {liquor,                                                 
#      red/blush wine}        => {bottled beer}     11.235269   
# [2] {citrus fruit,                                           
#      other vegetables,                                       
#      soda,                                                   
#      fruit/vegetable juice} => {root vegetables}  8.340400   
# [3] {tropical fruit,                                         
#      other vegetables,                                       
#      whole milk,                                             
#      yogurt,oil}            => {root vegetables}  8.340400   
# [4] {citrus fruit,                                           
#      grapes,                                                 
#      fruit/vegetable juice} => {tropical fruit}   8.063879   
# [5] {other vegetables,                                       
#      whole milk,                                             
#      yogurt,rice}           => {root vegetables}  7.951182   


# (g) What are the bottom 5 rules, sorted by support?
bottom.support <- sort(rules, decreasing = FALSE, by = "support")
inspect(head(bottom.support,5))
# Ans: Bottom 5 rules, sorted by support are: 

#      lhs                   rhs              support    
# [1] {curd,cereals}      => {whole milk}   0.001016777 
# [2] {butter,jam}        => {whole milk}   0.001016777 
# [3] {pastry,                                       
#      sweet spreads}     => {whole milk}   0.001016777 
# [4] {root vegetables,                              
#      butter,rice}       => {whole milk}   0.001016777 
# [5] {tropical fruit,                               
#      other vegetables,                             
#      rice}              => {whole milk}   0.001016777 


# (h) What are the bottom 5 rules, sorted by confidence?
bottom.confidence <- sort(rules, decreasing = FALSE, by = "confidence")
inspect(head(bottom.confidence,5))
# Ans: Bottom 5 rules, sorted by confidence are: 

#      lhs                        rhs                   confidence 
# [1] {turkey,curd}            => {other vegetables}        0.8
# [2] {herbs,                                                 
#     fruit/vegetable juice}   => {other vegetables}        0.8
# [3] {herbs,                                                 
#      rolls/buns}             => {whole milk}              0.8
# [4] {onions,waffles}         => {other vegetables}        0.8
# [5] {turkey,                                                
#      tropical fruit,                                        
#      root vegetables}        => {other vegetables}        0.8

# (i) What are the bottom 5 rules, sorted by lift?
bottom.lift <- sort(rules, decreasing = FALSE, by = "lift")
inspect(head(bottom.lift,5))
# Ans: Bottom 5 rules, sorted by lift are: 

#      lhs                   rhs         		 lift 
# [1] {herbs,                           		         
#      rolls/buns}       => {whole milk}		3.130919 
# [2] {butter,                          		         
#      yogurt,                          		         
#      soft cheese}      => {whole milk}		3.130919 
# [3] {frankfurter,                     		         
#      other vegetables,                		         
#      frozen meals}     => {whole milk}		3.130919 
# [4] {tropical fruit,                  		         
#      yogurt,                          		         
#      frozen meals}     => {whole milk}		3.130919 
# [5] {hamburger meat,                  		         
#      other vegetables,                		         
#      curd}             => {whole milk}		3.130919 


# 2.3 Problem 3: Clustering



# (a) Data cleanup

# (i) Think of what attributes, if any, you may want to omit from the dataset when you do the clustering. Indicate
# all of the attributes you removed before doing the clustering.
# Ans: I would omit Name attribute of the data set before doing clustering.

# (iii) Does the data need to be standardized?
# Ans: All attributes has mean close to each other. I don't think there is any need for standardization. fviz_nbclust() with WSS methods also gives almost similar graph for scaled and unscaled data. The data range is smaller in original dataset. Hence, the scaled data do not make much difference.


# (iii) You will have to clean the data to remove multiple spaces and make the comma character the delimiter.
# Please make sure you include your cleaned dataset in the archive file you upload.
# Ans: I have modified the file manually and provided in zip file.

Kmeansfile19 <- read.csv("File19Cleaned.csv",header=T, comment.char = '#') 
summary(Kmeansfile19)
rownames(Kmeansfile19) <- Kmeansfile19[,1]
Kmeansfile19 <- Kmeansfile19 [ , -1]

View(Kmeansfile19)

Kmeansfile19.scale <- scale(Kmeansfile19, scale=T)
View(Kmeansfile19.scale)


# (b) Clustering
# (i) Determine how many clusters are needed by running the WSS or Silhouette graph. Plot the graph using
# fviz_nbclust().


# nb <- NbClust(Kmeansfile19, distance = "euclidean", method = "complete", index ="all")
# nb <- NbClust(Kmeansfile19, method = "single", index ="all")
# nb$Best.nc
# nb
# fviz_nbclust(nb, kmeans, method="wss")

fviz_nbclust(Kmeansfile19, kmeans, method="wss")
fviz_nbclust(Kmeansfile19.scale, kmeans, method="wss")

# Ans: Total number of cluster will be 8 as per the elbow method on WSS.



# (ii) Once you have determined the number of clusters, run k-means clustering on the dataset to create that many
# clusters. Plot the clusters using fviz_cluster().

kmeans <- kmeans(Kmeansfile19, centers=8,nstart=25)
fviz_cluster(kmeans, data=Kmeansfile19,centers=8, nstart=25,main="K Means cluster with 4 clusters")


# (iii) How many observations are in each cluster?
nrow(Kmeansfile19[which(kmeans$cluster == 1),])
nrow(Kmeansfile19[which(kmeans$cluster == 2),])
nrow(Kmeansfile19[which(kmeans$cluster == 3),])
nrow(Kmeansfile19[which(kmeans$cluster == 4),])
nrow(Kmeansfile19[which(kmeans$cluster == 5),])
nrow(Kmeansfile19[which(kmeans$cluster == 6),])
nrow(Kmeansfile19[which(kmeans$cluster == 7),])
nrow(Kmeansfile19[which(kmeans$cluster == 8),])

# Ans: Number of observations in each cluster are:  
# Cluster =1: 9
# Cluster =2: 2
# Cluster =3: 10  
# Cluster =4: 8
# Cluster =5: 17
# Cluster =6: 1
# Cluster =7: 11  
# Cluster =8: 8    


kmeans$tot.withinss
# (iv) What is the total SSE of the clusters?
# Ans: 62.06598

kmeans$withinss
# (v) What is the SSE of each cluster?
# Ans: The SSE of each cluster is given below: 
# Cluster =1: 1.555556
# Cluster =2: 3.000000
# Cluster =3: 21.100000
# Cluster =4: 3.875000
# Cluster =5: 20.705882
# Cluster =6: 0.000000
# Cluster =7: 5.454545
# Cluster =8: 6.375000


# (vi) Perform an analysis of each cluster to determine how the mammals are grouped in each cluster, and whether
# that makes sense? 

C1 <- (Kmeansfile19[which(kmeans$cluster == 1),])
C1
C2 <- (Kmeansfile19[which(kmeans$cluster == 2),])
C2
C3 <- (Kmeansfile19[which(kmeans$cluster == 3),])
C3
C4 <- (Kmeansfile19[which(kmeans$cluster == 4),])
C4
C5 <- (Kmeansfile19[which(kmeans$cluster == 5),])
C5
C6 <- (Kmeansfile19[which(kmeans$cluster == 6),])
C6
C7 <- (Kmeansfile19[which(kmeans$cluster == 7),])
C7
C8 <- (Kmeansfile19[which(kmeans$cluster == 8),])
C8
