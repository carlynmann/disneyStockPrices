## https://www.codingfinance.com/post/2018-03-27-download-price/ website I found how to get the ## stock data on 
# final project 
# group 2 

rm(list=ls()) 
#install.packages('usethis')
library(usethis)
use_git_config(user.name = "Carlyn Mann", user.email = "carlyn-mann@uiowa.edu")
git_vaccinate()
create_github_token()
gitcreds::gitcreds_set()
usethis::git_sitrep()


library(tidyquant)  
library(dplyr) 
library(tidyr) 
library(stringr) 
library(ggplot2) 

## Reading Disney stock prices in and converting to dataframe  
options("getSymbols.warning4.0"=FALSE) 
options("getSymbols.yahoo.warning"=FALSE)  
getSymbols("DIS", from = '2019-01-01',  
           to = "2022-03-31",warnings = FALSE,  
           auto.assign = TRUE)  

DIS <- data.frame(date=index(DIS), coredata(DIS))  
names(DIS)[names(DIS) == 'DIS.Open'] <- "Open" 
names(DIS)[names(DIS) == 'DIS.Close'] <- "Close" 
names(DIS)[names(DIS) == 'DIS.High'] <- "High" 
names(DIS)[names(DIS) == 'DIS.Low'] <- "Low" 
names(DIS)[names(DIS) == 'DIS.Volume'] <- "Volume" 
names(DIS)[names(DIS) == 'DIS.Adjusted'] <- "Adjusted" 

## Reading Disney+ File in and Cleaning it  
titles <- read.csv(file = 'disney_plus_titles.csv', sep= ',', na.strings=c(''))  
titles$date_added <- as.Date(titles$date_added, format = '%m/%d/%y')  
titles$type <- as.factor(titles$type) 
titles$rating <- as.factor(titles$rating) 
titles$listed_in <- gsub(' ','', titles$listed_in) 
titles[c('Genre1', 'Genre2', 'Genre3')] <- str_split_fixed(titles$listed_in, ',', 15) 
titles[c('Country1', 'Country2', 'Country3')] <- str_split_fixed(titles$country, ',', 18) 

titles[titles == '']<- NA 
titles$listed_in <- NULL 
titles$director <- NULL 
titles$cast <- NULL  
titles$country <- NULL 

## Merging both dataframes  
merged<-merge(titles, DIS, by.x="date_added", by.y="date") 


## Analyzing Disney Stock Price Before and after Disney+ 
DISbefore <- subset(DIS, date<'2020-11-19' & date> '2020-10-07') 
summary(DISbefore) 

DISafter <- subset(DIS, date>'2020-11-20' & date < '2021-01-07') 
summary(DISafter) 


DISbefore_maxvol <- max(DISbefore$Volume) 
DISafter_maxvol <- max(DISafter$Volume) 

new <- data.frame(time= c('before', 'after'), max = c(DISbefore_maxvol,DISafter_maxvol)) 

ggplot(data= new, aes(x=max, y=time))+ geom_bar(stat='identity', fill='steelblue') + ggtitle('Max Volume Before and After Disney+ Launch') 

DISbefore_maxad <- max(DISbefore$Adjusted) 
DISafter_maxad <- max(DISafter$Adjusted) 

new2 <- data.frame(time= c('before', 'after'), max = c(DISbefore_maxad,DISafter_maxad)) 

ggplot(data= new2, aes(x=max, y=time))+ geom_bar(stat='identity', fill='steelblue') + ggtitle('Max Adjusted Before and After Disney+ Launch') 

ggplot(data= merged, aes(x = date_added, y = Adjusted)) + 
  geom_line() + 
  theme_classic() + 
  labs(x = 'Date', 
       y = "Adjusted Price", 
       title = "Disney price chart") 

## Analyzing stock price relationship 
# Making sure the relationship is linear 
merged_regression <- subset(merged, date_added>'2020-03-15') 
plot(Adjusted ~ type, data = merged_regression) 

#Linear Regression 
fit1<-lm(Adjusted~type+release_year+rating+Genre1+Genre2+Genre3, data=merged_regression) 
summary(fit1) 

#Narrow Down to Features that matter 
fit2<-lm(Adjusted~type+Genre3, data=merged_regression) 
summary(fit2) 
ggplot(data=merged_regression, aes(x=type, y=Adjusted)) + geom_boxplot() 

#Interactions 
interaction.plot(merged_regression$Genre3,  
                 merged_regression$type,  
                 merged_regression$Adjusted) 

#lines are not parallel so there is possible interaction 
fit2_int<-lm(Adjusted~Genre3*type,  
             data=merged_regression) 

summary(fit2_int) 

#What movie or show was released when stock price was highest?   
highest_stocks <- (merged[order(merged$Close, decreasing = TRUE), ])  

#Stock price was highest on March 12th, 2021, which was a big release day for Disney+  
closing_price <- data.frame(Date = merged$date_added, Stock_Price = merged$Close)  
stocks <- closing_price[!duplicated(closing_price),]  

ggplot(stocks, aes(x=Date, y=Stock_Price)) +  
  geom_line()+ ggtitle('Disney Stock Prices Since the Release of Disney Plus')  


#Do ratings affect Disney revenues differently?   
groupby1 <- group_by(merged, rating)  
summary1 <- summarise(groupby1)  
ratings <- na.omit(table(merged$rating))  
barplot(ratings, col = 'blue', 
        xlab = 'Rating', 
        ylab = 'Count', 
        main = 'Number of Movies/Shows On Disney+ By Rating') 

