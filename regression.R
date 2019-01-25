"
Problem2. The linear regression can be used to find the estimated height of a set of diameters 
70, 80, 90, 100 meters. Please draw a diagram using the diameter (cm) as the x-axis, 
the height (m) as the y-axis. You need to add the regression line 
and the estimated values as black dots. You graph should look like the following. 
Please use the right units and annotations.
"

ufc <- read.csv("ufc.csv")
#ufc
plot(ufc$dbh.cm, ufc$height.m,
     main=  "Estimate height", 
     xlab = "dbh.cm", 
     ylab = "height.m"
     )
model = lm(ufc$height.m ~ ufc$dbh.cm)
#model
abline(model)
