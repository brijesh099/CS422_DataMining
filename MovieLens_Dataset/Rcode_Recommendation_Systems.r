# Name: Brijesh Mavani
# CWID: A20406960
# University: Illinois Institute of Technology
# Course: Data Mining
# Assignment: 4
# Practicum problems

# set output length on R console to print more information.
options(max.print=999999) 

# Importing all library which will be used in this code

library(factoextra)
library(cluster)

#Problem 2.1

# set working directory 
setwd("C:/STUDY/MS/Data Mining/Assignment/Assignment4") 

# read CSV file and load it into a variable name Languages
Languages <- read.csv("File46Cleaned.csv",header=T, comment.char = '#',row.names = "Country")

summary(Languages)

# (a) Run hierarchical clustering on the dataset using factoextra::eclust() method. Run the clustering algorithm for three linkages: single, complete, and average. Plot the dendogram associated with each linkage using fviz_dend(). Make sure that the labels (country names) are visible at the leafs of the dendogram.

hc.single <- eclust(Languages, "hclust", hc_method="single")
fviz_dend(hc.single, show_labels=TRUE, palette="jco", as.ggplot=T)


hc.complete <- eclust(Languages, "hclust", hc_method="complete")
fviz_dend(hc.complete, show_labels=TRUE, palette="jco", as.ggplot=T)


hc.average <- eclust(Languages, "hclust", hc_method="average")
fviz_dend(hc.average, show_labels=TRUE, palette="jco", as.ggplot=T)

# (b) Examine each graph produced in (a) and understand the dendrogram. Notice which countries are clustered together as two-singleton clusters (i.e., two countries clustered together because they are very close to each other in the shared languages/s). For each linkage method, list all the two singleton clusters. For instance, {Great Britain, Ireland} form a two-singleton cluster since they share English as a common language. ''
# Ans: For Single menthods following are the two-singleton clusters:
#  
#   {Great Britain,Ireland} 
#   {West Germany,Austria}
#   {Luxemburg,Switzerland}
#   {France, Belgium}
#   {Denmark,Norway}
# 
# For complete menthods following are the two-singleton clusters:
#   {West Germany,Austria} 
#   {Luxemburg,Switzerland}
#   {Denmark,Norway}
#   {Great Britain,Ireland}
#   {France, Belgium}
# 
# 
# For average menthods following are the two-singleton clusters:
# 
#   {Portugal,Spain} 
#   {West Germany,Austria}
#   {Luxemburg,Switzerland}
#   {France, Belgium}
#   {Denmark,Norway}
#   {Great Britain,Ireland} 

# (c) Italy is clustered with a larger cluster in the single and average linkage, whereas in complete linkage it is clustered with a smaller cluster. Which linkage strategy do you think accurately reflects how Italy should be clustered? (Hint: Look at the raw data.) Justify your answer in 1-2 sentences.
# Ans: In Italy majority of populations speaks Italian followed by French. In complete linkage, it is clustered with France and Belgium where most of the people speaks French, FL and German. Whereas in Italy majority of people speaks Italian (100%) and only 11% of populations speaks French. There are other countries in dataset which has higher population of French speaking people. So, clustering Italy with France and Belgium in complete linkage doesn't make sense. In my observation they single and average linkage accurately reflects how Italy should be clustered.

# (d) Let’s pick a hierarchical cluster that we will call pure, and let’s define purity as the linkage strategy that produces the most two-singleton clusters. Of the linkage methods you examined in (b), which linkage method would be considered pure by our definition? 
# Ans: Based on our definition of the purity, cluster created with Average linkage is pure. 

# (e) Using the graph corresponding to the linkage method you chose in (d), at at a height of about 125, how many clusters would you have?
# Ans: If we cut a tree at height of 125, we get 7 clusters. 

clusters.125 <- cutree(hc.average, h= 125)
table(clusters.125)
s <- cluster::silhouette(clusters.125, dist(Languages))
summary(s)


# (f) Now, using the number of clusters you picked in (e), re-run the hierarchical clustering using the three linkage modes again, except this time through, specify the number of clusters using the k parameter to factoextra::eclust(). Plot the dendogram associated with each linkage using fviz_dend(). Make sure that the labels (country names) are visible at the leafs of the dendogram.'

hc.single7 <- eclust(Languages, "hclust", k = 7, hc_method="single")
fviz_dend(hc.single7, show_labels=TRUE, palette="jco", as.ggplot=T)


hc.complete7 <- eclust(Languages, "hclust", k = 7, hc_method="complete")
fviz_dend(hc.complete7, show_labels=TRUE, palette="jco", as.ggplot=T)


hc.average7 <- eclust(Languages, "hclust", k = 7, hc_method="average")
fviz_dend(hc.average7, show_labels=TRUE, palette="jco", as.ggplot=T)



statssingle <- fpc::cluster.stats(dist(Languages), hc.single7$cluster)
statssingle$dunn
statssingle$avg.silwidth

statscomplete <- fpc::cluster.stats(dist(Languages), hc.complete7$cluster)
statscomplete$dunn
statscomplete$avg.silwidth

statsaverage <- fpc::cluster.stats(dist(Languages), hc.average7$cluster)
statsaverage$dunn
statsaverage$avg.silwidth


# (g) For each cluster obtained by the value of k used in (f), print the Dunn and Silhouette width using the fpc::cluster.stats() method. Take a look at the help (or manual) page for fpc::cluster.stats() and see what is the name of the return list component that contains the Dunn index and the average Silhouette width. 
# Ans: For cluster with single linkage method:
#   Dunn = 0.7813006
#   Average Silhouette width = 0.1215148
#   
#   For cluster with complete linkage method:
#   Dunn = 0.6768822
#   Average Silhouette width = 0.1922308 
#   
#   For cluster with average linkage method:
#   Dunn = 0.807345
#   Average Silhouette width = 0.1698248 


# (h) From the three clusters in (g), which is the best cluster obtained if you consider the Dunn index only?
# Ans: If we consider only Dunn index then cluster obtained with average linkage methods is the best cluster.


# (i) From the three clusters in (g), which is the best cluster obtained if you consider the Silhouette width only?
# Ans: If we consider only average Silhouette width then cluster obtained with complete linkage methods is the best cluster.



# Problem 2.2 Topic: Locality sensitive hashing (2.4 point evenly divided by number of questions) 
library(textreuse)
library(magrittr)
library(dplyr)

corpusfiles <- list.files("C:/STUDY/MS/Data Mining/Assignment/Assignment4/corpus", full.names=T)

minhash <- minhash_generator(n=160, seed=100)
corpus <- TextReuseCorpus(corpusfiles, tokenizer = tokenize_ngrams, n = 4,
                          minhash_func = minhash, keep_tokens = TRUE)


length(unlist(tokens(corpus)))

#(a) How many shingles (or tokens) are there in all of the 100 documents? (Hint: look at package TextReuse::tokens()).
#Ans: There are total 22175 shingles with n = 4.


corpusFileOnlyFileName <- list.files("C:/STUDY/MS/Data Mining/Assignment/Assignment4/corpus", full.names=F)
doc_dict <- unlist(tokens(corpus)) %>% unique()
M <- lapply(tokens(corpus), function(set, dict) {   as.integer(dict %in% set)}, dict = doc_dict) %>% data.frame() 
tempSetName <-setNames( M, paste( corpusFileOnlyFileName, 1:length(corpusFileOnlyFileName)) )
rownames(M) <- doc_dict
dim(M)

#(b) What are the dimensions of the characteristic matrix?.
#Ans: The dimension of the charancteristic matrix is : 16784*100


origtaske <- corpus[["orig_taske"]]
tokens(origtaske)[1:5]
#(c) Print the first 5 shingles (or tokens) of the file orig_taske.txt.
# Ans: The first 5 shigles of file orig_taske.txt are as below: 
# [1] "in mathematics and computer"         
# [2] "mathematics and computer science"    
# [3] "and computer science dynamic"        
# [4] "computer science dynamic programming"
# [5] "science dynamic programming is"

minhash240 <- minhash_generator(n=240, seed=100)
corpus <- TextReuseCorpus(corpusfiles, tokenizer = tokenize_ngrams,
                          minhash_func = minhash240, keep_tokens = TRUE)

#tokens(corpus)
length(unlist(tokens(corpus)))
length(tokens(corpus))

#(d) We will fix our signatures (or hashes, or the rows in the signature matrix) at 240. This represents what percentage reduction in the size of the problem?
#Ans: The reduction in the size of the problem will be 98.57%


# Run LSH and find candidate pairs
buckets <- lsh(corpus, bands = 40)
candidates <- lsh_candidates(buckets)


lsh_probability(h = 240, b =  80, s = 0.3)
# (e) At 240 signatures (or hashes) we want a probability of 0.888 of getting a candidate pair in at least one band at a Jaccard similarity of 0.3 and above. How many bands will you need to get such a probability?
# Ans: We will need 80 bands to get a probability of 0.888 of getting a candidate pair in at least one band at a Jaccard similarity of 0.3 and above. 


# Run LSH and find candidate pairs
buckets <- lsh(corpus, bands = 80)
candidates <- lsh_candidates(buckets)
candidates
length(unlist(candidates))
noofcandidates <- length(unlist(candidates))/length(candidates)
noofcandidates
# (f) Using the number of bands you determined in (e), run LSH and find candidate pairs. How many candidate pairs do you get?
# Ans: We will get total 69 candidate pairs with band size as 80.

#candidates %>% arrange(desc(score))
corpus_matches <- buckets %>% 
lsh_candidates() %>% 
lsh_compare(corpus, jaccard_similarity) 
c_m <- corpus_matches %>% arrange(desc(score))
c_m[1:5,] 

#(g) Sort the candidate pairs according to their score field, in descending order (i.e., from highest score to lowest score). List the top 5 candidates that are similar.
#Ans: The top 5 candidates pairs are listed below based on the score:
#        a          b       score
#      <chr>      <chr>     <dbl>
# 1 g4pC_taska orig_taska 0.8431953
# 2 g3pA_taskd orig_taskd 0.7987805
# 3 g3pA_taskd g4pC_taskd 0.5461957
# 4 g4pC_taskd orig_taskd 0.5280000
# 5 g4pB_taske orig_taske 0.5008606


# (h) If you were not to use LSH and instead, examined every pair of documents for similarity, 
# (i) how many pairs of documents would you examine? 
# Ans: If we are not using LSH then we need to find similarity using Jaccard similarity. 
# In Jaccard similarity, we need to compare each file with other files to find similarity. We have total 100 files which will be compared with each other. Hence, total pair need to compare will be 4950. 
# This is huge number of pairs to be processed manually. This can be implemented by programming. 

#(ii) What is the ratio of this number to the to the number of candidate pairs you found in (f)?
# Ans: If implemented programmatically, all 4950 pairs can be compared. Hence, 4950/69 = 71.73. That means number of comparisons we have to do is 71.73 times then number of comparsion if used LSH. 


# 2.3 Topic: Recommender systems (2.3 point evenly divided by number of questions)

#(a) Content-based filtering):

setwd("C:/STUDY/MS/Data Mining/Assignment/Assignment4/MovieLens") 

udata<-read.table("u.data", sep = "\t")
colnames(udata)<-c("user id","Movie ID","rating","timestamp")
#View(udata)

movies<-read.table("u.item", sep = "|",comment.char = '#',quote="")
#View(movies)
colnames(movies)<-c("SrNo","Movie Name","Date","V4","IMDB Link","unknown","Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir","Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western")
colnames(movies)

userdata<-read.table("u.user", sep = "|",comment.char = '#',quote="")
colnames(userdata)<-c("user id","age","gender","occupation","zip code")
#View(userdata)

user200<- subset(udata, udata[,1] == 200)
k<-1
movies200<-subset(movies[1,])
movies200<- movies200[-1,]

for(i in 1:length(unlist(movies[,1])))
{
  for (j in 1:length(unlist(user200[,1])))
  {
    if(movies[i,1] == user200[j,2])
    {
      movies200[k,]<-subset(movies[i,])
      k<-k+1
    }
  }
}
movie.matrix<-subset(movies200[,6:24])
#View(movie.matrix)
genre200 <- apply(movie.matrix, 2, mean)
#View(genre200)

user50<- subset(udata, udata[,1] == 50)
k<-1;
movies50<-subset(movies[1,])
movies50<- movies50[-1,]

for(i in 1:length(unlist(movies[,1])))
{
  for (j in 1:length(unlist(user50[,1])))
  {
    if(movies[i,1] == user50[j,2])
    {
      movies50[k,]<-subset(movies[i,])
      k<-k+1
    }
  }
}
movie.matrix50<-subset(movies50[,6:24])
genre50 <- apply(movie.matrix50, 2, mean)

cosine <- function(x, y) 
  {
    sum(x*y)/(norm(x, type="2") * norm(y, type="2"))
  }

cosine(genre200, genre50)
#(i) Compute the user-user similarity of users with ID 200 and user with ID 50.
#Ans: 0.54825 using Cosine similarity.

movie127<-subset(movies[,6:24],movies[,1]==127)
#genre127 <- apply(movie127, 2, mean)
cosine(movie127, genre200)
#(ii)Compute the user-item similarity of movie with ID 127 to user 200.
#Ans: 0.5533398

cosine(movie127, genre50)

#(iii) Compute the user-item similarity of movie with ID 127 to user 50.
#Ans: 0.6235022

#(iv) Based on the above two computations, the movie 127 will be recommended to which user?
#Ans: Based on cosine similarity from (ii) and (iii) movie with ID 127 will be recommended to user 50 as it has higher cosine similarity.



# (b) Collaborative Filtering:

utilitymatrix<-matrix(0,6,11)

for(i in 1:length(unlist(udata[,1])))
{
  if(udata[i,1]==1 && udata[i,2]<7)
  {
      utilitymatrix[udata[i,2],1]<-udata[i,3]
  }
  
  if(udata[i,1]==21 && udata[i,2]<7)
  {
      utilitymatrix[udata[i,2],2]<-udata[i,3]
  }
  if(udata[i,1]==44 && udata[i,2]<7)
  {
     utilitymatrix[udata[i,2],3]<-udata[i,3]
  }
  if(udata[i,1]==59 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],4]<-udata[i,3]
  }
  if(udata[i,1]==72 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],5]<-udata[i,3]
  }
  if(udata[i,1]==82 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],6]<-udata[i,3]
  }
  if(udata[i,1]==102 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],7]<-udata[i,3]
  }
  if(udata[i,1]==234 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],8]<-udata[i,3]
  }
  if(udata[i,1]==268 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],9]<-udata[i,3]
  }
  if(udata[i,1]==409 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],10]<-udata[i,3]
  }
  if(udata[i,1]==486 && udata[i,2]<7)
  {
    utilitymatrix[udata[i,2],11]<-udata[i,3]
  }
}
colnames(utilitymatrix)<-c("user1","user21","user44","user59","user72","user82","user102","user234","user268","user409","user486")

#View(utilitymatrix)


means <- apply(utilitymatrix, 1, function(x) mean(x, na.rm=T))

means

for (i in 1:dim(utilitymatrix)[1]) {
  for (j in 1:dim(utilitymatrix)[2])
  {
    if(utilitymatrix[i,j]>0)
    {
      utilitymatrix[i,j] <- utilitymatrix[i,j] - means[i]
    }
  }
}
similarmovie<-matrix(0,6,1)

for (i in 1:dim(utilitymatrix)[1])
  {
    similarmovie[i,1]<-round(cosine(utilitymatrix[5,], utilitymatrix[i, ]), digits=2)
}

similarmovie

rownames(similarmovie)<-c("1","2","3","4","5","6")
a<-as.numeric(rownames(similarmovie)[order(similarmovie, decreasing=TRUE)][1:6])

r2685<-((similarmovie[a[2],1]*utilitymatrix[a[2],9])+(similarmovie[a[3],1]*utilitymatrix[a[3],9])+(similarmovie[a[4],1]*utilitymatrix[a[4],9]))/(similarmovie[a[2],1]+similarmovie[a[3],1]+similarmovie[a[4],1])
r2685

# Using this utility matrix, find out the expected rating that user 268 will give to movie 5 using |N| = 3.
#Ans: With |N|=3 user 268 will rate movie 5 with rating of 0.9059123.