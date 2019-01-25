ufc <- read.csv("ufc.csv")
ufc

diameter.m<-ufc$dbh.cm/100
height.m<-ufc$height.m
surface<- pi*diameter.m*height.m

ufc.plots<-data.frame(diameter.m, height.m, surface)
ufc.plots

library(plotly)
p <- plot_ly(ufc, 
             x = diameter.m, 
             y = height.m, 
             z = surface,
             color = surface, 
             type = "scatter3d")

layout(p, title = "The surface area of trees")