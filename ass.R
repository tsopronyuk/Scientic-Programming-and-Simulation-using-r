#1.(a) Write a function to count the number of even numbers in a vector of integers without 
#using the modulus operator, i.e., %%. (Hint: You cannot use any functions such as floor(). 
#You may use any number of operations including addition, subtraction, division of any kind, 
#and multiplication. (b) Use your function to count the number of even numbers in the 
#Nile dataset that came with RStudio.

countEven <- function(vec) {
    count<-0
    n<-length(vec)
    for (k in 1:n){
        
       
    if(vec[k] %%2 ==0)
        count<-count+1
    }
    count
}
#v<-ifelse(vec[length((as.character(vec)))] =='2'  , 1, 0) 

ifEven <- function(num) {    
   if(num == 1)
       return (0)
    else if (num==0)
        return (1)
        else 
            return (ifEven(num-2))
} 


v<-c(1,4,6,3,99,6,5,0,5,44,2,42,0)
apply(v(num),function(num)    
    if(num == 1)
        return (0)
    else if (num==0)
        return (1)
    else 
        return (ifEven(num-2))
)

countEven_(Nile)
countEven(v)

countEven(Nile)


#2.(Write an R program which assigns an odd number, say 9,
#   (you can use an assignment to assign a valid odd number to a variable, e.g., n, 
#and your function returns the following vectors: 
#(a) (1, 3, 5, 7, 9, 7, 5, 3, 1),

fun2a <- function(n) {
    v<-c(n)
    n<-n-2
    while(n>0){
            v<-c(n,v,n)
            n<-n-2
    }
    v  
}

fun2a(9)

#and (b) 
#Using n = 5, your function returns a vector containing three l's, followed by three 2's, 
#and then three 3's, etc., until three 5's.

fun2b <- function(n) {
    v<-c()
    
    for (k in 1:n)
            v<-c(v,k,k,k)       
    v  
}

fun2b(5)


#3.Write an assignment statement to complete each of the following without using any loop, 
#diff() and signO: (a) generate a vector of the difference between the ith element and the 
#(i+l)th element in a vector v of integers, where i = 1: (length(v)-1), 

fun3a <- function(vec) {
    
    v1<-c(0,vec)
    v2<-c(vec,0)
    v<-v1-v2
    
    v<-v[-1]
    v <- v[-length(v)]         
    v  
}

v <- c(1,4,7,2,1)
fun3a(v)

#and (b) generate 
#a vector of the sign of each elements in the vector generated in (a) as follows: 
#'p' for positive, 'n' for negative. Again, you cannot use any loops. (Use v <- c(1,4,7,2,1))
 
fun3b <- function(vec) {
    
    v1<-c(0,vec)
    v2<-c(vec,0)
    v<-v1-v2
    
    v<-v[-1]
    v <- v[-length(v)]         
    v[v<0]<-'n'
    v[v!='n']<-'p'
    v
}

v <- c(1,4,7,2,1)
fun3b(v) 

#4.Write a function that assigns an integer value, say 7, to an integer n and generate a 
#symmetric metrics where the diagonal elements are all zeros, and the elements following the 0 value are 
#consecutive odd numbers. For example, the first row of a 5X5 matrix contains "0, 1, 3, 5, 7"; 
#the second row contains "1, 0, 3, 5, 7". 
#You may use any function that we have used in our lectures to write your function. 
#(You cannot hard-code the matrix; it needs to be generated.)

fun4 <- function(n) {
    n<-n / 2 + 2
    (1-diag(n))*(cumsum(diag(n)*2)-1);
}

fun4(7) 
fun4(9)  

fun4_ <- function(n) {
    v <- seq(from=1, to=n, by=2)
    size<-length(v)+1
    matr <- matrix(0,  size,  size)
    for (i in 1:size) 
        matr[i, -i] <- v 
    matr
}

fun4_(7) 
fun4_(9) 
