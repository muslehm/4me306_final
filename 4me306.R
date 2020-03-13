#################
##Libraries#####
################
library(plotrix)
library(reshape2)
library(dplyr)
library(ggplot2)
################

###Read CSV Data###
##################
data = read.csv("~/Desktop/cleaned_assessment.csv", stringsAsFactors = FALSE)

######################
#Age Category
myvars <- c('s1q1')
age <- data[myvars]
age_count <- table(age)
par(mar = rep(2, 4))
barplot(age_count,
        main="UEPs Age Categories",
        xlab="Age Range",
        ylab="Count",
        border="red",
        col="blue",
        density=10)

#Read news
myvars <- c('s1q2')
rnews <- data[myvars]
read_news <- table(rnews)
par(mar = rep(2, 4))
barplot(read_news,
        main="Frequency of Reading News",
        xlab="Category",
        ylab="Freuqency",
        border="red",
        col="blue",
        density=10)

#No. of Articles
myvars <- c('s1q5')
rarticles <- data[myvars]
read_articles <- table(rarticles)
par(mar = rep(2, 4))
barplot(read_articles,
        main="No. of articles read daily",
        xlab="Category",
        ylab="UEPs",
        border="red",
        col="blue",
        density=10)
#Usability box plots
boxplot(data[,7:13], main = "Distribution of Usability Responses",
        xlab = "Usability", ylim = c(1, 5),
        col = "orange") 

#UX box plots
boxplot(data[,14:23], main = "Distribution of User Experience Responses",
        xlab = "User Experience", ylim = c(1, 5),
        col = "blue") 

#Averages 
data$Usabiliy <- rowMeans(subset(data, select = c(s2q1, s2q2, s2q3, s2q4, s2q5, s2q6, s2q7)), na.rm = TRUE)
data$User_Experience<- rowMeans(subset(data, select = c(s3q1, s3q2, s3q3, s3q4, s3q5, s3q6, s3q7, s3q8, s3q9, s3q10)), na.rm = TRUE)

boxplot(data[,28:29], main = "Usability vs User Experience",
        xlab = "Evaluation", ylim = c(1, 5),
        col = "Green") 
mean(data$Usabiliy)
mean(data$User_Experience)

###Relate intersting variances to:

##Age group

boxplot(User_Experience~s1q1, data, main = "UX per age group",
        xlab = "Age", ylim = c(1, 5),
        col = "Blue") 

boxplot(s2q2~s1q1, data, main = "s2q2 per age group",
        xlab = "Age", ylim = c(1, 5),
        col = "Orange") 

boxplot(s2q3~s1q1, data, main = "s2q3 per age group",
        xlab = "Age", ylim = c(1, 5),
        col = "Orange") 

boxplot(s3q1~s1q1, data, main = "s3q1 per age group",
        xlab = "Age", ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q3~s1q1, data, main = "s3q3 per age group",
        xlab = "Age", ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q9~s1q1, data, main = "s3q9 per age group",
        xlab = "Age", ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q10~s1q1, data, main = "s3q10 per age group",
        xlab = "Age", ylim = c(1, 5),
        col = "Blue") 


##How often they consume news 

boxplot(User_Experience~s1q2, data, main = "UX by how often users read news",
        xlab = "How often they read news?", ylim = c(1, 5),
        col = "Blue") 

boxplot(s2q2~s1q2, data, main = "s2q2 by how often users read news",
        xlab = "How often they read news?", ylim = c(1, 5),
        col = "Orange") 

boxplot(s2q3~s1q2, data, main = "s2q3 by how often users read news",
        xlab = "How often they read news?", ylim = c(1, 5),
        col = "Orange")  

boxplot(s3q1~s1q2, data, main = "s3q1 by how often users read news",
        xlab = "How often they read news?", ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q3~s1q2, data, main = "s3q3 by how often users read news",
        xlab = "How often they read news?", ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q9~s1q2, data, main = "s3q9 by how often users read news",
        xlab = "How often they read news?",
        ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q10~s1q2, data, main = "s3q10 by how often users read news",
        xlab = "How often they read news?", ylim = c(1, 5),
        col = "Blue") 

##How many articles read daily

boxplot(User_Experience~s1q5, data, main = "UX by articles read daily",
        xlab = "How many articles read/day?", ylim = c(1, 5),
        col = "Blue") 

boxplot(s2q2~s1q5, data, main = "s2q2 by articles read daily",
        xlab = "How many articles read/day?", ylim = c(1, 5),
        col = "Orange") 

boxplot(s2q3~s1q5, data, main = "s2q3 by articles read daily",
        xlab = "How many articles read/day?", ylim = c(1, 5),
        col = "Orange")  

boxplot(s3q1~s1q5, data, main = "s3q1 by articles read daily",
        xlab = "How many articles read/day?", ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q3~s1q5, data, main = "s3q3 by articles read daily",
        xlab = "How many articles read/day?", ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q9~s1q5, data, main = "s3q9 by articles read daily",
        xlab = "How many articles read/day?",
        ylim = c(1, 5),
        col = "Blue") 

boxplot(s3q10~s1q5, data, main = "s3q10 by articles read daily",
        xlab = "How many articles read/day?", ylim = c(1, 5),
        col = "Blue") 