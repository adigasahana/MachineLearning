# Analysing and Visualizing Boston MArathon results for years 2015, 2016 and 2017
setwd("C:/Sahana/R/BostonMarathonAnalysis")
#including all the required libraries
library(tidyverse)
library(dplyr)
library(readr) #to read csv files
library(plotly)
library(countrycode)
library(hms)
library(tm)
library(wordcloud)
library(RColorBrewer)
#read csv file from the working directory
#asigning each csv file into a dataframe 
df1_2015 <- read.csv("BM_results_2015.csv", stringsAsFactors = FALSE)
df2_2016 <- read.csv("BM_results_2016.csv", stringsAsFactors = FALSE)
df3_2017 <- read.csv("BM_results_2017.csv", stringsAsFactors = FALSE)
# view the dataframe structure
str(df1_2015)
str(df2_2016)
str(df3_2017)
#X and X.1 columns in 2015 and 2017 df contains no information and X in 2016.Selecting all other columns except X and X.1 from 3 dfs
df1_2015 <- df1_2015 %>% select(-one_of(c("X","X.1")))
head(df1_2015)
df2_2016 <- df2_2016 %>% select(-one_of("X"))
head(df2_2016)
names(df2_2016)
names(df1_2015)
df3_2017 <- df3_2017 %>% select(-one_of(c("X", "X.1")))
head(df3_2017)
names(df3_2017)
# since we have same names for all 2 dfs, we can combine 3 into 1 single dataframe using rbind
df <- rbind(df1_2015, df2_2016, df3_2017)
#get the dimension of the new dataframe using dim() and validate the total no of rows 
dim(df)
# understand the structure of the dataframe
str(df)
#visualize the total number of participants for each year 
#selecting only the gender column for visualization, renames column M.F to Gender
gender_dist <- df %>% select(M.F)
g <- ggplot(gender_dist, aes(x = M.F, fill = M.F)) + geom_bar()
g <- g + labs(x = "Gender", y = "Particpants Count", title = "Gender Distribution", subtitle = "Distribution of Male and Female particpants from 2015-2017") + guides(fill = FALSE)
g
#split male and female participants into age groups
min(df$Age)
max(df$Age)
# since min and max age participants are between 18 and 84, we can split the seq of data from 10 to 90 in 10 intervals
df$Aged_between <- cut(df$Age, seq(10,90,10))
count(df, Aged_between)
#visualize particants by age categories
df_Age_dist <- ggplot(df, aes(x = Aged_between, fill = "red")) + geom_bar() + guides(fill = FALSE)
df_Age_dist <- df_Age_dist + ggtitle("Distribution of Participants by Age") + labs(x = "Age Category", y = "Participants Count") 
df_Age_dist
#visualize participants by age and gender
df_Age_Gender_dist <- df %>% group_by(Aged_between, M.F)
count(df_Age_Gender_dist)
#using summarise 
df_Age_Gender_dist <- df %>% group_by(Aged_between, M.F) %>% summarise(Count = n())
df_Age_Gender_dist
str(df_Age_Gender_dist)
#find out the percentage of MAle and Female compared to the overall participants in each Age category
df_Age_Gender_dist$Pct <- sapply(df_Age_Gender_dist$Count, function(x)  100 * x/sum(df_Age_Gender_dist$Count))
df_Age_Gender_dist$Sex <- as.factor(df_Age_Gender_dist$M.F)
head(df_Age_Gender_dist)
#Plot the distribution of % participants base don age & gender
Age_Gender_dist_plot <- ggplot(data = df_Age_Gender_dist, aes(x = Aged_between, y = Pct, fill = Sex)) + geom_bar(stat = "identity", position="dodge") + ggtitle("Percentage of participants per Age Group & Gender") + xlab("Age Category and Gender") + ylab("Percentage") + guides(fill = FALSE)
Age_Gender_dist_plot
#ANalyse count of participants over different age group
Age_Gender_plot <- ggplot(df, aes(df$M.F, y = df$Age)) + geom_boxplot(fill = "pink", colour = "blue", outlier.color = "red")
Age_Gender_plot + ggtitle("Distribution of Participants Age over Gender") + xlab("Gender") + ylab("Age")
ggplot(df) + geom_bar(aes(df$Age, fill = "red")) + ggtitle("Distribution of Participants per Age group") + xlab("Age") + ylab("Count of Participants") + guides(fill = FALSE)
#countries wordcloud - load df$Country to CountryNames 
#ANalysed with all countries, allcountries except USA, USA&CAN and USA,CAN & GBR
#CountryNames <- df$Country
#CounryNames <- df$Country %>% filter(df$Country != 'USA) 
CountryNames <- df%>%  filter(df$Country != 'USA' & df$Country != 'CAN' & df$Country != 'GBR') 
CountryNames <- CountryNames$Country
CountryNames
write.table(CountryNames, "CountryExceptUSA-CAN-GBR.txt")
#wordcloud analysis - load data as Corpus
docs <- Corpus(VectorSource(CountryNames))
docs
head(docs)
inspect(docs)
#use transformations to remove puctuations, numbers, space etc
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, stripWhitespace)
#build TDM
tdm <- TermDocumentMatrix(docs)
Country_count <- as.matrix(tdm)
count <- sort(rowSums(Country_count), decreasing = TRUE)
Countrylist <- data.frame(Country = names(count), count = count)
head(Countrylist, 10)
wordcloud(Countrylist$Country, Countrylist$count,max.words = 500, random.order = FALSE,colors = brewer.pal(8, "Dark2"))
