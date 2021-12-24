
path = "https://raw.githubusercontent.com/viveksavita/R/main/data/2021-08-west-yorkshire-street.csv"
y <- read.csv(path, header=TRUE , stringsAsFactors=FALSE)

df1 <- data.frame(y)

head(df1)

df_cleaned <- df1[ c("Crime.ID","Crime.type","Last.outcome.category")]

#df_cleaned

library(dplyr)
pie_1 <- df_cleaned %>%
  group_by(Crime.type)%>%
  summarise( count = n())%>%
  mutate(total = sum(count)) %>%
  mutate(perc = round( (count/total)*100))



pie_1$perc_1 = paste( pie_1$perc ,"%")

par(mar=c(3,2,2,2))


#Color palette
library(RColorBrewer)
myPalette <- colorRampPalette(brewer.pal(8, "Set2"))(length(pie_1$perc))


#Create pie chart
pie( pie_1$perc , 
     labels = pie_1$perc_1 , 
     main = "Crime Type",
     #col = rainbow(length(pie_1$perc)),
     border="black", 
     col=myPalette,
     radius = .6,
     cex = 0.6)

#Legend location
legend("topright",pie_1$Crime.type , cex = 0.5,
       fill = myPalette)


