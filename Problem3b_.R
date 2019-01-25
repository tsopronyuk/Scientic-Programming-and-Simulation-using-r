#Problem 3b.
#If the service rate is always 1.0, draw a graph using the
#plot(arriveRate, meanWaitTime) to illustrate the relationship of mean waiting times
#with a given range of arrival rate from 0.3 to 0.8 with the increment of 0.1.

source('DES.R')
source('mm1.R')
arrivalRate <- seq(0.3,0.8, by = 0.1)
meanWaitTime<-vector()

f <- function(s) dosim(mm1initglbls,
                       mm1reactevnt,
                       mm1prntrslts,
                       10000.0,
                       list(arrvrate=s,srvrate=1)
)
meanWaitTime<-sapply(arrivalRate, f)


plot(arrivalRate, meanWaitTime,
     main = "The relation of mean waiting times with a given range of arrival rate")
