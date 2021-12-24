library("tidyr")
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(data.table)



#Data loading
s <- read.csv("https://raw.githubusercontent.com/viveksavita/R/main/data/2021-08-west-yorkshire-street.csv")
o <- read.csv("https://raw.githubusercontent.com/viveksavita/R/main/data/2021-08-west-yorkshire-outcomes.csv")


head(s)

#data cleaning
s$clocation <- substring( s$Location,12)



#Data aggregation
s1 <-  s%>%
  group_by(LSOA.name)%>%
  summarise(locationCount=n_distinct(clocation))

s2 <- s%>%
  group_by(LSOA.name)%>%
  summarise(CrimeCount=n_distinct(Crime.ID))

s1$rank <- rank(-s1$locationCount)

#coverting data in dataframe
s1 <- as.data.frame(s1)
s2 <- as.data.frame(s2)

#Joining two dataframes
s3 <- s1%>%
  left_join(s2, by = "LSOA.name")


#Selecting top 20 
s3 <- s3[s3$rank <=20,]



rownames(s3) <- s3[,1]
s3 <- s3[order(-s3$CrimeCount),]

s3 <- s3[,c("locationCount","CrimeCount")]

s4 <- transpose(s3)

colnames(s4) <- rownames(s3)
rownames(s4) <- colnames(s3)

s4 <- as.matrix(s4)

barplot(s4 , 
        beside=T , 
        legend.text=T,
        col=c("blue" , "skyblue" ) 
         , ylab="Count",
        cex.axis = .6,
        cex.lab = 1,
        font.axis=1,
        cex.names=.7,
        ,las=2
        ,args.legend = list(bty = "n", x = "top", ncol = 2),
        ylim = c(0,600),
        main = "Top 20 Crime Active Area's")




#######Analysis of leeds 111b

u <- s[s$LSOA.name =="Leeds 111B",]

u1 <-  u%>%
  group_by( Crime.type)%>%
  summarise(CrimeCount=n_distinct(Crime.ID))

#Ordering the data
u1 <- u1[order(u1$CrimeCount),]


par(mar=c( 4,10,5,10))
barplot(height=u1$CrimeCount, 
        names=u1$Crime.type, 
        col="#69b3a2", horiz=T , 
        las=1,
        cex.names=.7,xlim = c(0,200),
        main= "Crime Analysis of: Leeds 111B")

