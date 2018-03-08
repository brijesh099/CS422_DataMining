# Name: Brijesh Mavani
# CWID: A20406960
# University: Illinois Institute of Technology
# Course: Data Mining
# Assignment: 1
# Practicum problems

# set output length on R console to print more information.
options(max.print=999999) 


#Problem 1.a
# set working directory 
setwd("C:/STUDY/MS/Data Mining/Assignment") 

# read CSV file and load it into a variable name collegeData
collegeData <- read.csv("College.csv") 

# display data from variable collegeData
  View(collegeData)  

#Problem 1.b
# consider values in first column as a rownames for all rows
rownames(collegeData) <- collegeData[,1]

# call data editor for collegeData
fix(collegeData) 

# Reload collegeData variable after removing values from first column
collegeData <- collegeData [ , -1]

# call data editor for collegeData
fix(collegeData) 

#Problem 1.c.i
# produce a numerical summaries of collegeData
summary(collegeData) 

#Problem 1.c.ii
# produce a matrix of scatterplot
pairs(collegeData[,1:10], main = "Simple Scatterplot Matrix of College Data", col=c("red","blue","darkgreen","orange"))

#Problem 1.c.iii
# plot side by side boxplots of Outstate versus Private
plot (collegeData$Outstate~collegeData$Private,type="b",main = "Outstate versus Private",xlab = "Private",ylab ="Outstate" ,col=rainbow(2)) 


#Problem 1.c.iv
# create new Elite variable by binning the Top10perc variable 
Elite <- rep("No", nrow(collegeData))
Elite[collegeData$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
collegeData <- data.frame(collegeData, Elite)

# produce a numerical summary of no. of elite universities
summary(collegeData$Elite)

# plot side by side boxplots of Outstate versus Elite
plot (collegeData$Outstate~collegeData$Elite,type="b",main = "Outstate versus Elite",xlab = "Elite",ylab ="Outstate" ,col=c("red","green"))

#Problem 1.c.v
# histograms for with differing numbers of bins(5 and 10) for a few of the quantitative variables 
par(mfcol=c(2,3),font.main= 7,font.sub=7)

hist(collegeData$Top10perc, breaks = 6,freq=TRUE, col = "red", main = "Histogram of Top10perc with 5 Bin", xlab = "Top10perc", ylab = "Value")
hist(collegeData$Top10perc, breaks = 11, freq = TRUE, col = "orange", main = "Histogram of Top10perc with 10 Bin", xlab = "Top10perc", ylab = "Value")
hist(collegeData$Top25perc, breaks = 6, freq = TRUE, col = "red", main = "Histogram of Top25perc with 5 Bin", xlab = "Top25perc", ylab = "Value")
hist(collegeData$Top25perc, breaks = 11, freq = TRUE, col = "orange", main = "Histogram of Top25perc with 10 Bin", xlab = "Top25perc", ylab = "Value")
hist(collegeData$Enroll, breaks = 6, freq = TRUE, col = "red", main = "Histogram of Enroll with 5 Bin", xlab = "Enroll", ylab = "Value")
hist(collegeData$Enroll, breaks = 11, freq = TRUE, col = "orange", main = "Histogram of Enroll with 10 Bin", xlab = "Enroll", ylab = "Value")

#Problem 1.c.vi

# Observations:
#Find college with maximum applications
MaxApplications <- max(collegeData$Apps)
MaxApplications
MaxApplicationsCollege <- collegeData[collegeData$Apps==MaxApplications, ]
MaxApplicationsCollege
# 1) Rutgers at New Brunswick received maximum applications. 


#Find college with maximum Accepts by numbers
MaxAccepts <- max(collegeData$Accept)
MaxAccepts
MaxAcceptsCollege <- collegeData[collegeData$Accept==MaxAccepts, ]
MaxAcceptsCollege
# 2) Rutgers at New Brunswick accepted maximum applications.


#Find college(s) with maximum Acceptance rate
PercAccept <- rep(0, nrow(collegeData))
collegeData <- data.frame(collegeData, PercAccept)
for (i in 1:nrow(collegeData))
{
  collegeData[i,20] <- round((100*collegeData[i,3])/collegeData[i,2],2)
}
MaxAcceptsPerc <- max(collegeData$PercAccept)
MaxAcceptsPerc
MaxAcceptsPercCollege <- collegeData[collegeData$PercAccept==MaxAcceptsPerc, ]
MaxAcceptsPercCollege
# 3) Emporia State University, Mayville State University, MidAmerica Nazarene College, Southwest Baptist University, University of Wisconsin-Superior and Wayne State College has 100% acceptance rate.


#Find college with maximum enroll by number 
MaxEnroll <- max(collegeData$Enroll)
MaxEnroll
MaxEnrollCollege <- collegeData[collegeData$Enroll==MaxEnroll, ]
MaxEnrollCollege
# 4) Texas A&M Univ. at College Station has maximum student enrollment.


#Find college with maximum enrollment rate(Yield)
PercEnroll <- rep(0, nrow(collegeData))
collegeData <- data.frame(collegeData, PercEnroll)
for (i in 1:nrow(collegeData))
{
  collegeData[i,21] <- round((100*collegeData[i,4])/collegeData[i,3],2)
}
MaxEnrollPerc <- max(collegeData$PercEnroll)
MaxEnrollPerc 
MaxEnrollPercCollege <- collegeData[collegeData$PercEnroll==MaxEnrollPerc, ]
MaxEnrollPercCollege
# 5) California Lutheran University has 100% yield i.e. all accepted students enrolled in college

#Final summary of collegeData data set.
summary(collegeData)

par(mfcol=c(1,1))
library(corrplot)

collegeSet <- subset(collegeData,select=c(Apps,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad,PhD,Grad.Rate))
summary(collegeSet)
corCollegeSet <- cor(collegeSet)
corCollegeSet
corrplot(corCollegeSet,method ="pie")


# By creating a correlation matrix and Corrplot, following observations are made on college data set: 
# 6) Enroll has a better correlation with Top25perc than Top10perc i.e. new students enrolled in college are more from the top 25 % of high school class.
# 7) Enroll has a better correlation with F.Undergrad than P.Undergrad i.e. more students enrolled in a full-time course in college.
# 8) Also, Enroll shows better correlation with Ph.D.  So, enrollment in college also depends on how many Ph.D. faculties are there in a college. Hence, the value of the Elite variable should be derived based on the value of the Ph.D. column. 
# 9) The Graduation rate of students from Top10perc is better than students from Top25perc. 
# 10) Accept has a better correlation with F.Undergrad than P.Undergrad i.e. college accepts more students for full-time than part-time.
