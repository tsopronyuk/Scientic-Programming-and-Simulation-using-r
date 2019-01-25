"
Problem 3a.
If the arrival rate is always 0.5, draw a graph using the 
plot(serviceRate, meanWaitTime) function to illustrate 
the relation of mean waiting times with a given range of service rate 
from 0.5 to 1.5 with the increment of 0.1.
"
source('DES.R')
source('mm1.R')
serviceRate <- seq(0.5,1.5, by = 0.1)
meanWaitTime<-vector()


f <- function(s) dosim(mm1initglbls,
                       mm1reactevnt,
                       mm1prntrslts,
                       10000.0,
                       list(arrvrate=0.5,srvrate=s)
)
meanWaitTime<-sapply(serviceRate, f)

plot(serviceRate, meanWaitTime,
     main = "the relation of mean waiting times with a given range of service rate")
