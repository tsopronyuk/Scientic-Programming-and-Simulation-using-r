#Problem 3c.
#Draw a 3D graph using the service rates (x axis) from 0.5 to 1.5 with the increment 
#of 0.1; and the arrival rate (y axis) from 0.3 to 0.8 with the increment of 0.1 unit; 
#and the mean waiting time as the z axis. 
#You may use the function wireframe() or cloud() in the library of lattice 
#to draw the graph. You have to develop a driver function to implement the simulation. 

source('DES.R')
source('mm1.R')

serviceRate <- seq(0.5,1.5, by = 0.1)
arrivalRate <- seq(0.3,0.8, by = 0.1)


myGrid <- data.frame(expand.grid(arrivalRate,serviceRate))


for (i in 1:nrow(myGrid)){
    myGrid$meanWaitTime[i] <-dosim(mm1initglbls,
                                   mm1reactevnt,
                                   mm1prntrslts,
                                   10000.0,
                                   list(arrvrate=myGrid[i,1],
                                        srvrate=myGrid[i,2])
    ) 
}

colnames(myGrid) <- c("arrivalRate","serviceRate","meanWaitTime")

library(lattice)
cloud(myGrid$meanWaitTime ~ myGrid$arrivalRate * myGrid$serviceRate,
      main = "The relation of mean waiting times", 
      xlab = "serviceRate", 
      ylab = "arrivalRate",
      zlab =  "meanWaitTime",
      scales = list(col = "transparent"),
      pch = 1)
