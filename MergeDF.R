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
    
    df[match(d1$kids,df$kids),2]<-unlist(d1$states)
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
