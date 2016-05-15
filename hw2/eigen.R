x <- SwissBankNotes[,-7]
center <- colMeans(x, na.rm = TRUE)
x <- sweep(x, 2L, center, check.margin = FALSE) #default is subtract

f <- function(v) {
        v <- v[!is.na(v)]
        sqrt(sum(v^2)/max(1, length(v) - 1L))
}


scale <- apply(x, 2L, f)
x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
head(x)

sd(eigen(cov(x))$vectors[[1]])

cor(x)
cov(x)

s <- svd(cov(x))

eigen(cov(x))
eigen(cor(x))
svd(cov(x))$u * svd(cov(x))$d  * t(svd(cov(x))$v)
rank <- sum(svd(cov(x))$d > (svd(cov(x))$d[1L]))

s
D <- diag(s$d)
s$u %*% D %*% t(s$v) #  X = U D V'
t(s$u) %*% cov(x) %*% s$v #  D = U' X V

#proportion of variance
eigen(cov(x))$values / sum(eigen(cov(x))$values)


