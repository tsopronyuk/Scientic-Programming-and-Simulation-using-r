#PROBLEM 1
"
In this problem, you will create two data frames and merge them. 
The two data frames are defined as follows. 
The first one contains two columns: d1.Kids and d1.States. 
The second data frame contains also two columns: d2.Ages and d2.Kids. 
The d1.Kids may not appearin d2.Kids, and vice versa. 
You need to write a function to merge them without using merge().
"

my.merge <- function(d1,d2) {
    kids<-union(d1$kids,d2$kids)
    names<-union(names(d1), names(d2))
    
    df <- data.frame(matrix(NA, length(kids), length(names),
                           dimnames=list(c(),names)),
                           stringsAsFactors=F)
    df$kids<-kids
    
    df[match(d1$kids,df$kids),2]<-d1$states
    df[match(d2$kids,df$kids),3]<-d2$ages
    df
}

# construct d1 as a data frame using d1.Kids and d1.States
d1.Kids <-c("Jack", "Jill", "Jillian", "John", "James") 
d1.States <-c("CA", "MA", "DE", "Hi", "PA")
d1<-data.frame(kids=d1.Kids,states=d1.States,stringsAsFactors=F)
d1

# construct d2 as a data frame using d2.Ages and d2.Kids
d2.Ages<-c(10, 7,12, 30) 
d2.Kids<-c("Jill", "Jillian", "Jack", "Mary")
d2<-data.frame(ages=d2.Ages, kids=d2.Kids,stringsAsFactors=F)
d2

my.merge(d1,d2)

#PROBLEM 2
"
Suppose that (x(t), y(t)) has polar coordinates 
(a) (r, 2 sin (2t)), 
(b) (r, 1+1.5*cos(t)), 
respectively, for t in 0 to 10, 
Plot the two graphs side-by-side. 
Your plot should look like the following. 
You need to draw the vertical/horizontal lines and the equations on top of the graphics.
"

library(ggplot2)

dat <- data.frame(x=seq(0, 4*pi, length.out=100))
fun_a <- function(x){
    2*sin(2*x)   
}
p1<-ggplot(dat, aes(x=x)) + 
    ggtitle("(a) (r, 2 sin (2t))")+
    stat_function(fun=fun_a, colour = 'green') +
    coord_flip()+
    geom_vline(xintercept = 0, colour = 'red')+
    geom_vline(xintercept = pi, colour = 'red')+
    geom_vline(xintercept  = 2*pi, colour = 'red')+
    geom_vline(xintercept = 3*pi, colour = 'red')+
    coord_polar(theta = "y")

dat <- data.frame(x=seq(0, 2*pi, length.out=100))
fun_b <- function(x){
    1+1.5*cos(x)  
}
p2<-ggplot(dat, aes(x=x)) + 
    ggtitle("(b) (r, 1+1.5*cos(t))")+
    stat_function(fun=fun_b, colour = 'green') + 
    geom_vline(xintercept = 0, colour = 'red')+
    geom_vline(xintercept = pi/2, colour = 'red')+
    geom_vline(xintercept  = pi, colour = 'red')+
    geom_vline(xintercept  = 3*pi/2, colour = 'red')+
    coord_flip()+
    coord_polar(theta = "y")

library(grid)
pushViewport(viewport(layout = grid.layout(1, 2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


#PROBLEM 3

ufc <- read.csv("ufc.csv")
head(ufc)

diameter.m<-2*ufc$dbh.cm/100
height.m<-ufc$height.m
surface<- pi*diameter.m*height.m

ufc.plots<-data.frame(diameter.m, height.m, surface)
head(ufc.plots)

library(lattice)

# 3d scatterplot 
cloud(surface~diameter.m*height.m, 
      main= expression(paste("Upper Flat Greed Tree Surface Area (", m^3, ")", sep = "")), 
      aspect = c(2/3, 0.8),
      xlab = "diameter(m)", 
      ylab = "height(m)",
      zlab =  expression(paste("surface (", m^2, ")", sep = "")),
      scales = list(col = "transparent"),
      drape = TRUE,
      colorkey = TRUE,
      data = ufc.plots)

          
          
       