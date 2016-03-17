rm(list=ls())

## consider the following utility matrix
## question 2
## part a
## 

library(xtable)
xtab <- data.frame(rbind(c(4, 5, 0, 5, 1, 0, 3, 2),
                 c(0, 3, 4, 3, 1, 2, 1, 0),
                 c(2, 0, 1, 3, 0, 4, 5, 3)))

colnames(xtab) <- c(letters[1:8])
rownames(xtab) <- c("A", "B", "C")
xtable(xtab)
print.xtable

require(arules)



jaccard <- function(a, b){ 
        int <- sum(a[a==b])
        union <- sum(a+b)-int 
        round(1-(int/union), 4)
}

cosine <- function(a, b){
        AB <- a*b
        AB <- sum(AB)
        AA <- a^2
        AA <- sum(AA)
        BB <- b^2
        BB <- sum(BB)
        round(AB/(sqrt(AA)*sqrt(BB)), 4)
}
jaccard(boolean.mat[2,], boolean.mat[3,])




## treat the utility matrix as boolean
boolean.mat <- rbind(c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
                  c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
                  c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))

## compute Jaccard distance between users
User1vUser2.bool <- jaccard(boolean.mat[1,], boolean.mat[2,])
User1vUser3.bool <- jaccard(boolean.mat[1,], boolean.mat[3,])
User2vUser3.bool <- jaccard(boolean.mat[2,], boolean.mat[3,])
message(paste0("Jaccard Distance between User 1 and 2 is: ", User1vUser2.bool, "\n",
               "Jaccard Distance between User 1 and 3 is: ", User1vUser3.bool, "\n",
               "Jaccard Distance between User 2 and 3 is: ", User2vUser3.bool))

## compute cosine distance between users
User1vUser2.bool.cos <- cosine(boolean.mat[1,], boolean.mat[2,])
User1vUser3.bool.cos <- cosine(boolean.mat[1,], boolean.mat[3,])
User2vUser3.bool.cos <- cosine(boolean.mat[2,], boolean.mat[3,])
message(paste0("Cosine Distance between User 1 and 2 is: ", User1vUser2.bool.cos, "\n",
               "Cosine Distance between User 1 and 3 is: ", User1vUser3.bool.cos, "\n",
               "Cosine Distance between User 2 and 3 is: ", User2vUser3.bool.cos)) 




## part b -- treat ratings 3,4,5 as 1, and ratings 1,2, and blank as 0

util.mat.b <- rbind(c(1, 1, 0, 1, 0, 0, 1, 0),
                    c(0, 1, 1, 1, 0, 0, 0, 0),
                    c(0, 0, 0, 0, 1, 0, 1, 1))

## compute jaccard 
## compute jaccard 
User1vUser2utilb <- jaccard(util.mat.b[1,], util.mat.b[2,])
User1vUser3utilb <- jaccard(util.mat.b[1,], util.mat.b[3,])
User2vUser3utilb <- jaccard(util.mat.b[2,], util.mat.b[3,])
message(paste0("Jaccard Distance between User 1 and 2 is: ", User1vUser2utilb, "\n",
               "Jaccard Distance between User 1 and 3 is: ", User1vUser3utilb, "\n",
               "Jaccard Distance between User 2 and 3 is: ", User2vUser3utilb))


## compute cosine
User1vUser2utilb.cos <- cosine(util.mat.b[1,], util.mat.b[2,])
User1vUser3utilb.cos <- cosine(util.mat.b[1,], util.mat.b[3,])
User2vUser3utilb.cos <- cosine(util.mat.b[2,], util.mat.b[3,])
message(paste0("Cosine Distance between User 1 and 2 is: ", User1vUser2utilb.cos, "\n",
               "Cosine Distance between User 1 and 3 is: ", User1vUser3utilb.cos, "\n",
               "Cosine Distance between User 2 and 3 is: ", User2vUser3utilb.cos))

## part c

org.mat <- rbind(c(4, 5, 0, 5, 1, 0, 3, 2),
                 c(0, 3, 4, 3, 1, 2, 1, 0),
                 c(2, 0, 1, 3, 0, 4, 5, 3)) 
org.mat[org.mat==0] <- NA #replaces zeroes as NAs
#norm function subtracts non-NA values in row by mean of that row
norm <- function(x) {sweep(x, 1, rowSums(x,na.rm=T)/ncol(x), "-")}
norm.org <- norm(org.mat) #apply normalization
norm.org[is.na(norm.org)] <- 0 #replace NAs back to 0s
norm.org

User1vUser2utilb.norm <- cosine(norm.org[1,], norm.org[2,])
User1vUser3utilb.norm <- cosine(norm.org[1,], norm.org[3,])
User2vUser3utilb.norm <- cosine(norm.org[2,], norm.org[3,])
message(paste0("Cosine Distance between User 1 and 2 is: ", User1vUser2utilb.norm, "\n",
               "Cosine Distance between User 1 and 3 is: ", User1vUser3utilb.norm, "\n",
               "Cosine Distance between User 2 and 3 is: ", User2vUser3utilb.norm))

