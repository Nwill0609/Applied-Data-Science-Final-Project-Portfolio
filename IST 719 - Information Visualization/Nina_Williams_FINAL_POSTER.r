
# Author: Nina Williams
# Purpose: Final Poster

library(plyr)
library(ggplot2)

setwd("C:/Users/16196/OneDrive/Documents/SYRACUSE/IST 719")
getwd()
vg <- read.csv("VideoGameSales.csv"
               , header = TRUE
               , stringsAsFactors = FALSE)

#Description - what does the data set represent?

#The dataset is a summary of video game sales and ratings over a span of 25 
#years covering several platformss and various developers


dim(vg)
str(vg)
sum(complete.cases(vg))

#Create new dataset with complete cases only
vg1 <- vg[complete.cases(vg),]
vg1[vg1=="N/A"] <- NA
vg1 <- na.omit(vg1)
dim(vg1)

#Provide an output of the str function on your dataset
str(vg1)

#Provide the calculation notes using your data set:
#Formuala for approved dataset size compete cases without N/A's only
(16*4)*(6893/100)
#Total = 4,411 = Approved!

#Exploring the data

colnames(vg1)
#View(vg1)

count(unique(vg1$Platform))
count(unique(vg1$Year_of_Release))
count(unique(vg1$Genre))
count(unique(vg1$Publisher))
count(unique(vg1$Developer))
count(unique(vg1$Rating))

sum(vg1$NA_Sales)
sum(vg1$EU_Sales)
sum(vg1$JP_Sales)
sum(vg1$Global_Sales)
sum(vg1$Other_Sales)

#Single Dimention Plots

par(mfrow = c(3,1))

plot(vg1$User_Score,
     , ylab = "User Ratings"
     , col = "darkgreen"
     , main = "Video Game User Ratings 1-10")

plot(vg1$Critic_Score,
     , ylab = "Critic Ratings"
     , col = "red"
     , main = "Video Game Critic Ratings 1-10")

hist(log10(vg1$Global_Sales)
     , xlab = "Global_Sales"
     , col = "yellow"
     , main = "Video Game Global Sales")

boxplot(log10(vg1$NA_Sales)
        , horizontal = TRUE
        , ylab = "North America Sales"
        , col = "yellow"
        , main = "North America Video Game Sales")

#Multi-Dimension Plots

par(mfrow = c(3,1))

Platform_Sales <- aggregate(vg1$Global_Sales, list(vg1$Platform), sum)
colnames(Platform_Sales) <- c("Platform","Global Sales")
Platform_Sales
class(Platform_Sales)
barplot(Platform_Sales$`Global Sales`, names.arg=Platform_Sales$Platform
        , col= "hotpink"
        , xlab = "Platform"
        , ylab = "Global Sales"
        , main = "Video Game Platform Global Sales")

top10 <- head(arrange(vg1,desc(Global_Sales)), n=50)
top10 <- data.frame(top10)
colnames(top10)
top10

Top10Platform_Sales <- aggregate(top10$Global_Sales, list(top10$Platform), sum)
colnames(Top10Platform_Sales) <- c("Platform","Global Sales")
Top10Platform_Sales
class(Top10Platform_Sales)
barplot(Top10Platform_Sales$`Global Sales`, names.arg=Top10Platform_Sales$Platform
        , col= "hotpink"
        , xlab = "Platform"
        , ylab = "Global Sales"
        , main = "Video Game Platform Global Sales")


vg1$User_Score <- as.numeric(vg1$User_Score)
vg1

UserScore <- aggregate(vg1$User_Score, list(vg1$Platform), mean)
colnames(UserScore) <- c("Platform","User Score")
UserScore
class(UserScore)
barplot(UserScore$`User Score`, names.arg=Platform_Sales$Platform
        , col= "purple"
        , xlab = "Platform"
        , ylab = "Average User Score"
        , main = "Video Game Platform Average User Score")

Genre_Sales <- aggregate(vg1$Global_Sales, list(vg1$Genre), sum)
colnames(Genre_Sales) <- c("Genre","Global Sales")
Genre_Sales
class(Genre_Sales)
barplot(Genre_Sales$`Global Sales`, names.arg=Genre_Sales$Genre
        , col= "Gold"
        , xlab = "Genre"
        , ylab = "Global Sales"
        , main = "Video Game Genre Global Sales")



###############################################################
top20 <- head(arrange(vg1,desc(Global_Sales)), n=20)
top20 <- data.frame(top20)
colnames(top20)
top20
head(vg1)

library(treemap)
treemap(top20, index = c("Name")
        ,vSize = "Global_Sales"
        ,vColor = "Platform"
        ,type = "index"
        ,fontsize.labels = 12
        ,palette = "Blues")

top10 <- head(arrange(vg1,desc(Global_Sales)), n=10)
top10 <- data.frame(top10)
colnames(top10)
top10
head(vg1)

library(treemap)
treemap(top10, index = c("Name")
        ,vSize = "Global_Sales"
        ,vColor = "Platform"
        ,type = "index"
        ,fontsize.labels = 24
        ,palette = "Blues")

###############################################################
vg1[c(2,4,5,15,16)] <- lapply(VGdata[c(2,4,5,15,16)], factor)
vg1[3] <- lapply(VGdata[3], as.numeric)

YearPlat = vg1 %>% group_by(Year_of_Release, Platform) %>% summarise(Total.Platform.Sales = sum(Global_Sales))
MaxPlat = YearPlat %>% group_by(Year_of_Release) %>%slice(which.max(Total.Platform.Sales))
TopOne = YearPlat %>% group_by(Year_of_Release) %>%top_n(1)
ggplot(TopOne, aes(x=Year_of_Release, y=Total.Platform.Sales)) + geom_bar(stat='Identity', position = 'dodge', aes(fill=Platform))


###############################################################


library(reshape2)
top10 <- data.frame(top10)
str(top10)
dim(top10)

vg1
vg2 <- top10[,-2:-10]
vg3 <- vg2[,-3]
str(vg3)
vg4 <- vg3[,-4:-7]
vg4

vg4$NewUserScore <- vg4$User_Score*10
vg4

vg4 <- vg4[,-3]
vg4

vg5 <- melt(vg4, id.vars="Name")

str(vg5)

ggplot(vg5, aes(x= vg5$Name, y = value, colour = variable)) +
        geom_bar(stat='identity', position = 'dodge')

###############################################################

tot_region_sales = vg1 %>% group_by(Year_of_Release) %>% summarise(tot_NA_sales = sum(
        NA_Sales), tot_EU_sales = sum(EU_Sales), tot_JP_sales = sum(JP_Sales))
ggplot(data = tot_region_sales, aes(x = Year_of_Release)) + geom_line(aes(y=tot_NA_sales, colour = 'NA Sales'))+ geom_line(
        aes(y=tot_EU_sales, colour = 'EU Sales')) + geom_line(aes(y=tot_JP_sales, colour = 'JP Sales'))




