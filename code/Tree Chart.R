library("tidyr")
library(ggplot2)
library(hrbrthemes)
library(data.table)
library(data.tree)


y <- read.csv("https://raw.githubusercontent.com/viveksavita/R/main/data/2021-08-west-yorkshire-stop-and-search.csv")

#Data aggregation
y1 <- 
  y%>%
  group_by(Age.range , Type)%>%
  summarise(Offense=n())%>%
  ungroup()

#New percentage field
y1 <- y1%>%
  mutate(perc = Offense/sum(Offense))

y1$perc = percent(y1$perc)

#Taking care of missing values

y1[y1$Age.range =="",c("Age.range")] <- "NotAvailable"
y1[y1$Gender =="",c("Gender")] <- "NotAvailable"

y1 <- as.data.frame(y1[, c("Age.range","Type","perc")])



#Creating path strings for nodes
y1$pathString <- paste("All",y1$Type,y1$Gender, y1$Age.range, y1$perc, sep="/")

#Using package data.tree to create nodes 
y2 <- as.Node(y1)


SetGraphStyle(y2, rankdir = "TB")
SetEdgeStyle(y2, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(y2, style = "filled,rounded", shape = "box", 
             fillcolor = "LightBlue", 
             fontname = "helvetica", 
             fontcolor = "black")
plot(y2)

y2






