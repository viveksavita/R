library(heatmaply)
library("tidyr")

d2 <- df1[df1$Last.outcome.category != "",]
d2[d2==""]<-NA
d2 <- d2%>%
  group_by( Crime.type,Last.outcome.category)%>%
  summarise(count = n())

df1[df1$Last.outcome.category == "",]



d3 <-d2 %>%
  pivot_wider(names_from = Last.outcome.category, 
              values_from = count , 
              values_fill = 0)

d4 <- d2
d4 <- as.data.frame(d3)
rownames(d4) <- d4[,1]
data.matrix(d5)


d5 <- d4[,-1]

p <- heatmaply(d5, 
               cellnote= d5,
               dendrogram = "none",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               branches_lwd = 0.1,
               fontsize_row = 8, 
               fontsize_col = 8,
                                      )


p
