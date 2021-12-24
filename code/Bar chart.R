library("tidyr")
library(ggplot2)
library(hrbrthemes)
library(data.table)


z <- read.csv("https://raw.githubusercontent.com/viveksavita/R/main/data/2021-08-west-yorkshire-stop-and-search.csv")


z1 <- 
  z%>%
  group_by( Age.range)%>%
  summarise(Offense=n())%>%
  ungroup()

z1 <- z1%>%
  mutate(perc = Offense/sum(Offense))

z1$perc = percent(z1$perc)
z1[z1$Self.defined.ethnicity =="",c("Age.range")] <- "NotAvailable"

par(mar=c( 4,10,5,6))

barplot(height=z1$Offense, 
        names=z1$Age.range, 
        col="#69b3c2" , 
        las=1,
        cex.names=1,
        horiz=T,
        main= "Age Group",
        cex.axis = 1)



