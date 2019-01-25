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
