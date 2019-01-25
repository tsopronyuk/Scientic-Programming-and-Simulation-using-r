"
Suppose that (x(t), y(t)) has polar coordinates 
(a) (r, 2 sin (2t)), 
(b) (r, 1+1.5*cos(t)), 
respectively, for t in 0 to 10, 
Plot the two graphs side-by-side. 
Your plot should look like the following. 
You need to draw the vertical/horizontal lines and the equations on top of the graphics.
"
library("pracma", lib.loc="~/Library/R/3.2/library")
t <- seq(0,10, len=100)  # the parametric index
par(mfrow=c(1,2))
polar(t, 2*sin(2*t), grcol = "red", bxcol = "black",col = "green", main = "(a) (r, 2 sin (2t))")
polar(t, 1+1.5*cos(t),  grcol = "red", bxcol = "black",col = "green",  main = "(b) (r, 1+1.5*cos(t))")
x=2*sin(2*t)*cos(t)
y=2*sin(2*t)*sin(t)
plot(x,y,
     type='l',
     main='(a) (r, 2 sin (2t))',
     col="green")
abline(h = -2:2, v = -2:2, col = "lightgray", lty = 3)
abline(h = 0, v = 0, col = "red")

x=(1+1.5*cos(t))*cos(t)
y=(1+1.5*cos(t))*sin(t)
plot(x,y,
     type='l',
     main='(b) (r, 1+1.5*cos(t))',
     col="green")
abline(h = -2:2, v = -2:2, col = "lightgray", lty = 3)
abline(h = 0, v = 0, col = "red")

#initiate a 100 x 3 matrix filled with zeros
m <- matrix(numeric(300), ncol = 3)
# simulate a 3D random-walk
for (i in 2:100) m[i, ] <- m[i-1, ] + rnorm(3)
# collect everything in a data-frame
df <- setNames(
    data.frame(m, seq(1, 100)),
    c("x", "y", "z", "time")
)


ufc <- read.csv("ufc.csv")
ufc

diameter.m<-ufc$dbh.cm/100
height.m<-ufc$height.m
surface<- pi*diameter.m*height.m

library(plotly)
p <- plot_ly(ufc, x = diameter.m, y = height.m, z = surface, 
             color = surface, type = "scatter3d")
layout(p, title = "The surface area of trees")

ufc.plots<-data.frame(diameter.m, height.m, surface)
ufc.plots
library(lattice)
contourplot(surface ~ diameter.m * height.m,
            main = "The surface area of trees", 
            xlab = "Diameter (m)", 
            ylab = "Height (m)",
            region = TRUE,
            aspect = "iso",
            col.regions = gray((11:1)/11),
            data = ufc.plots)
wireframe(surface ~ diameter.m * height.m,
          main = "The surface area of trees", 
          xlab = "Diameter (m)", 
          ylab = "Height (m)",
          data = ufc.plots)
