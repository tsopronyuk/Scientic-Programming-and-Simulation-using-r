# DES.R:  R routines for discrete-event simulation (DES)

# each event will be represented by a data frame row consisting of the
# following components:  evnttime, the time the event is to occur;
# evnttype, a character string for the programmer-defined event type;
# optional application-specific components, e.g.
# the job's arrival time in a queuing app

# a global list named "sim" holds the events data frame, evnts, and
# current simulated time, currtime; there is also a component dbg, which
# indicates debugging mode

# forms a row for an event of type evntty that will occur at time
# evnttm; see comments in schedevnt() regarding appin
evntrow <- function(evnttm,evntty,appin=NULL) {
    rw <- c(list(evnttime=evnttm,evnttype=evntty),appin)
    return(as.data.frame(rw))
}

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm,evntty,appin=NULL) {
    newevnt <- evntrow(evnttm,evntty,appin)
    # if the event list is empty, set it to consist of evnt and return
    if (is.null(sim$evnts)) {
        sim$evnts <<- newevnt
        return()
    }
    # otherwise, find insertion point
    inspt <- binsearch((sim$evnts)$evnttime,evnttm) 
    # now "insert," by reconstructing the data frame; we find what
    # portion of the current matrix should come before the new event and
    # what portion should come after it, then string everything together
    before <- 
        if (inspt == 1) NULL else sim$evnts[1:(inspt-1),]
    nr <- nrow(sim$evnts)
    after <- if (inspt <= nr) sim$evnts[inspt:nr,] else NULL
    sim$evnts <<- rbind(before,newevnt,after)
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; could be changed to C
# code for efficiency
binsearch <- function(x,y) {
    n <- length(x)
    lo <- 1
    hi <- n
    while(lo+1 < hi) {
        mid <- floor((lo+hi)/2)
        if (y == x[mid]) return(mid)
        if (y < x[mid]) hi <- mid else lo <- mid
    }
    if (y <= x[lo]) return(lo)
    if (y < x[hi]) return(hi)
    return(hi+1)
}

# start to process next event (second half done by application
# programmer via call to reactevnt()) 
getnextevnt <- function() {
    head <- sim$evnts[1,]
    # delete head
    if (nrow(sim$evnts) == 1) {
        sim$evnts <<- NULL
    } else sim$evnts <<- sim$evnts[-1,]
    return(head)
}

# simulation body
# arguments:
#    initglbls:  application-specific initialization function; inits
#      globals to statistical totals for the app, etc.; records apppars
#      in globals; schedules the first event
#    reactevnt: application-specific event handling function, coding the
#       proper action for each type of event
#    prntrslts:  prints application-specific results, e.g. mean queue
#       wait
#    apppars:  list of application-specific parameters, e.g.
#      number of servers in a queuing app
#    maxsimtime:  simulation will be run until this simulated time 
#    dbg:  debug flag; if TRUE, sim will be printed after each event
dosim <- function(initglbls,reactevnt,prntrslts,maxsimtime,apppars=NULL,
                  dbg=FALSE) {
    sim <<- list()
    sim$currtime <<- 0.0  # current simulated time
    sim$evnts <<- NULL  # events data frame
    sim$dbg <<- dbg
    initglbls(apppars)
    while(sim$currtime < maxsimtime) {  
        head <- getnextevnt()
        sim$currtime <<- head$evnttime  # update current simulated time
        reactevnt(head)  # process this event 
        if (dbg) print(sim)
    }
    prntrslts()
}



# DES application:  M/M/1 queue, arrival rate 0.5, service rate 1.0

# the call 
# dosim(mm1initglbls,mm1reactevnt,mm1prntrslts,10000.0,
#    list(arrvrate=0.5,srvrate=1.0))
# should return a value of about 2 (may take a while)

# initializes global variables specific to this app
mm1initglbls <- function(apppars) {
    mm1glbls <<- list()
    # simulation parameters
    mm1glbls$arrvrate <<- apppars$arrvrate
    mm1glbls$srvrate <<- apppars$srvrate
    # server queue, consisting of arrival times of queued jobs
    mm1glbls$srvq <<- vector(length=0) 
    # statistics
    mm1glbls$njobsdone <<- 0  # jobs done so far
    mm1glbls$totwait <<- 0.0  # total wait time so far
    # set up first event, an arrival; the application-specific data for
    # each event will consist of its arrival time, which we need to
    # record in order to later calculate the job's residence time in the
    # system
    arrvtime <- rexp(1,mm1glbls$arrvrate)
    schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
    #cat("ARRIVAL:", arrvtime,"\n");
}

# application-specific event processing function called by dosim()
# in the general DES library 
mm1reactevnt <- function(head) {
    if (head$evnttype == "arrv") {  # arrival
        # if server free, start service, else add to queue (added to queue
        # even if empty, for convenience)
        if (length(mm1glbls$srvq) == 0) {
            mm1glbls$srvq <<- head$arrvtime
            srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
            schedevnt(srvdonetime,"srvdone",list(arrvtime=head$arrvtime))
        } else mm1glbls$srvq <<- c(mm1glbls$srvq,head$arrvtime)
        # generate next arrival
        arrvtime <- sim$currtime + rexp(1,mm1glbls$arrvrate)
        schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
        #cat ("ARRIVED", arrvtime, "\n")
    } else {  # service done
        # process job that just finished
        # do accounting
        mm1glbls$njobsdone <<- mm1glbls$njobsdone + 1
        mm1glbls$totwait <<- 
            mm1glbls$totwait + sim$currtime - head$arrvtime
        # remove from queue
        mm1glbls$srvq <<- mm1glbls$srvq[-1]
        # more still in the queue?
        if (length(mm1glbls$srvq) > 0) {
            # schedule new service
            srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
            schedevnt(srvdonetime,"srvdone",list(arrvtime=mm1glbls$srvq[1]))
        }
    }
}

mm1prntrslts <- function() {
    #print("mean wait:")
    #print(mm1glbls$totwait/mm1glbls$njobsdone)
    mm1glbls$totwait/mm1glbls$njobsdone
}




"
Problem1.Exercise 11.4.3 is a problem for finding the ???standard normal percentage point??? F(Zp) using (Zp) = p 
for p=0.5, 0.95, 0.975, and 0.99, respectively. 
Hint: You can change this problem into a root-finding problem as ???F(Zp) - p = 0???. 
Also, the program Phi.r finds the integration of the normal distribution, i.e., F(Z); 
and the program phi.r find the value of the function F???(Z). For each value of p, 
your solution contains only one line of code. 
You need to use Newton-Raphson Method to find the solution.
"

#install.packages("spuRs")
library("spuRs")

phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi)) 

#program spuRs/resources/scripts/simpson_n.r
simpson_n <- function(ftn, a, b, n = 100) {
    # numerical integral of ftn from a to b
    # using Simpson's rule with n subdivisions #
    # ftn is a function of a single variable
    # we assume a < b and n is a positive even integer
    n <- max(c(2*(n %/% 2), 4))
    h <- (b-a)/n
    x.vec1 <- seq(a+h, b-h, by = 2*h) 
    x.vec2 <- seq(a+2*h, b-2*h, by = 2*h) 
    f.vec1 <- sapply(x.vec1, ftn)
    f.vec2 <- sapply(x.vec2, ftn)
    S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2)) 
    return(S)
}   

root<-function(p){
    
    ftn <- function(x) {
        # returns function value and its derivative at x
        dfx <- phi(x) #exp(-x^2/2)/sqrt(2*pi)
        fx <- if (x < 0) 
            (0.5 - simpson_n(phi, x, 0))
        else 
            (0.5 + simpson_n(phi, 0, x))
        
        fx <-fx-p
    
        return(c(fx, dfx))
    }
    
    newtonraphson(ftn, 0, 1e-6) # from library("spuRs")
}

vec.p<-c(0.5, 0.95, 0.975,  0.99)
vec.roots<-vector()

for (p in vec.p) 
    vec.roots<-c(vec.roots, root(p))

print (vec.roots)

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


"
Problem 3a.
If the arrival rate is always 0.5, draw a graph using the 
plot(serviceRate, meanWaitTime) function to illustrate 
the relation of mean waiting times with a given range of service rate 
from 0.5 to 1.5 with the increment of 0.1.
"
#source('DES.R')
#source('mm1.R')
serviceRate <- seq(0.5,1.5, by = 0.1)
meanWaitTime<-vector()
for (r in serviceRate){
    meanWaitTime<-c(meanWaitTime, 
                    dosim(mm1initglbls,
                          mm1reactevnt,
                          mm1prntrslts,
                          10000.0,
                          list(arrvrate=0.5,srvrate=r)
                    )
    )  
}

#Problem 3b.
#If the service rate is always 1.0, draw a graph using the
#plot(arriveRate, meanWaitTime) to illustrate the relationship of mean waiting times
#with a given range of arrival rate from 0.3 to 0.8 with the increment of 0.1.

#source('DES.R')
#source('mm1.R')
arrivalRate <- seq(0.3,0.8, by = 0.1)
meanWaitTime<-vector()
for (a in arrivalRate){
    meanWaitTime<-c(meanWaitTime,
                    dosim(mm1initglbls,
                          mm1reactevnt,
                          mm1prntrslts,
                          10000.0,
                          list(arrvrate=a,srvrate=1)
                    )
    )
}

plot(arrivalRate, meanWaitTime,
     main = "The relation of mean waiting times with a given range of arrival rate")


#Problem 3c.
#Draw a 3D graph using the service rates (x axis) from 0.5 to 1.5 with the increment 
#of 0.1; and the arrival rate (y axis) from 0.3 to 0.8 with the increment of 0.1 unit; 
#and the mean waiting time as the z axis. 
#You may use the function wireframe() or cloud() in the library of lattice 
#to draw the graph. You have to develop a driver function to implement the simulation. 

#source('DES.R')
#source('mm1.R')

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

plot(serviceRate, meanWaitTime,
     main = "the relation of mean waiting times with a given range of service rate")



