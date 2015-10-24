#setwd("...")
if(!require(plyr)) install.packages("plyr")
library(plyr)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(stats)) install.packages("stats")
library(stats)
if(!require(fpc)) install.packages("fpc")
library(fpc)
if(!require(cluster)) install.packages("cluster")
library(cluster)

#Read consumer survey data, if not already read
if(!(exists("CSdata_temp"))){
  CSdata_temp<-read.csv("Data_consumer_expenditure_survey.csv")
}
#Read price index data, if not already read
if(!(exists("PIdata_temp"))){
  PIdata_temp<-read.csv("Data_Supplementary_price.csv")
}
#Remove features I don't need from consumer survey and price index data
CSdata<-CSdata_temp[, c(166,167,2,170,13,14,51,180,203,189,206,207,208,209,210,211)]
PIdata<-PIdata_temp[, c(1,2,4,5,10,11,12,14,17,20,21,22,23,25,26,27,28,29)]
#Consolidate split categories (by median since it's skewed), remove unwanted features and adjust to 2000 dollar
for (i in 1:nrow(PIdata)){
PIdata$utilities[i]=median(c(PIdata$homefuel_price[i],PIdata$elect_price[i],PIdata$gas_price[i],PIdata$water_price[i]))
PIdata$food[i]=median(c(PIdata$foodhome_price[i],PIdata$foodout_price[i],PIdata$foodwork_price[i]))
PIdata$alctob[i]=median(c(PIdata$alcohol_price[i],PIdata$tobacco_price[i]))
PIdata$transport[i]=median(c(PIdata$gasoline_price[i],PIdata$carservs_price[i],PIdata$airfare_price[i],PIdata$othtrans_price[i],PIdata$masstran_price[i]))
}
PIdata<-PIdata[,-c(4:8,10:18)]
PIdata[3:8]=100.0/PIdata[3:8]
#Adjust expenditures in consumer survey data to 2000 $
for (i in 1:nrow(CSdata)){
  CSdata$food[i]=CSdata$food[i]*PIdata$food[PIdata$year==CSdata$year[i] & PIdata$quarter==CSdata$quarter[i]]
  CSdata$rent[i]=CSdata$rent[i]*PIdata$renthome_price[PIdata$year==CSdata$year[i] & PIdata$quarter==CSdata$quarter[i]]
  CSdata$clothes_pcare[i]=CSdata$clothes_pcare[i]*PIdata$clothes_price[PIdata$year==CSdata$year[i] & PIdata$quarter==CSdata$quarter[i]]
  CSdata$utility[i]=CSdata$utility[i]*PIdata$utilities[PIdata$year==CSdata$year[i] & PIdata$quarter==CSdata$quarter[i]]
  CSdata$transport[i]=CSdata$transport[i]*PIdata$transport[PIdata$year==CSdata$year[i] & PIdata$quarter==CSdata$quarter[i]]
  CSdata$alc_tob[i]=CSdata$alc_tob[i]*PIdata$alctob[PIdata$year==CSdata$year[i] & PIdata$quarter==CSdata$quarter[i]]
}
CSdata$income[CSdata$income<0]<-0
CSdata$wage_spouse[CSdata$wage_spouse=="\\N"]<-0
CSdata$wage_spouse<-as.numeric(as.character(CSdata$wage_spouse))
CSdata$income<-CSdata$income+CSdata$wage_spouse
CSdata$hrswkd<-CSdata$hrswkd*CSdata$wkswkd
CSdata<-CSdata[,-c(1,2,6,10)]

########################TRENDS overall
#Scale data
df<-scale(CSdata[,c(1:2,4:12)])
reldata<-CSdata[,c(1:2,4:12)]

#Define function to create WSS plot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i,iter.max=30)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(df)

#Cluster
fit<- kmeans(df, 3)
fit$size
aggregate(reldata, by=list(cluster=fit$cluster), mean)
plotcluster(reldata, fit$cluster, xlab="Discriminant 1", ylab="Discrominant 2")
with(reldata, pairs(reldata, col=c(1:ncol(reldata))[fit$cluster])) 

########################Trends in Income
#Scale data
df<-scale(CSdata[,c(1:2,4:6)])
reldata<-CSdata[,c(1:2,4:6)]
fit.inc <- kmeans(df, 3)
fit.inc$size
aggregate(reldata, by=list(cluster=fit.inc$cluster), mean)

########################TRENDS in food expenditure
df<-scale(CSdata[,c(1:2,4:7)])
reldata<-CSdata[,c(1:2,4:7)]
fit.food <- kmeans(df, 2)
fit.food$size
aggregate(reldata, by=list(cluster=fit.food$cluster), mean)

########################TRENDS in rent and utilities

df<-scale(CSdata[,c(1:2,4:6,8,10)])
reldata<-CSdata[,c(1:2,4:6,8,10)]
fit.rentut <- kmeans(df, 3)
fit.rentut$size
aggregate(reldata, by=list(cluster=fit.rentut$cluster), mean)

########################TRENDS in clothing expenditure

df<-scale(CSdata[,c(1:2,4:6,9)])
reldata<-CSdata[,c(1:2,4:6,9)]
fit.cloth <- kmeans(df, 3)
fit.cloth$size
aggregate(reldata, by=list(cluster=fit.cloth$cluster), mean)

########################TRENDS in transport expenditure

df<-scale(CSdata[,c(1:2,4:6,11)])
reldata<-CSdata[,c(1:2,4:6,11)]
fit.trans <- kmeans(df, 3)
fit.trans$size
aggregate(reldata, by=list(cluster=fit.trans$cluster), mean)

########################TRENDS in alctob expenditure

df<-scale(CSdata[,c(1:2,4:6,12)])
reldata<-CSdata[,c(1:2,4:6,12)]
fit.alctob <- kmeans(df, 3)
fit.alctob$size
aggregate(reldata, by=list(cluster=fit.alctob$cluster), mean)


