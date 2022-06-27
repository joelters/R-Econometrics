rm(list = ls())
library(ggplot2)
library(EnvStats)
library(DescTools)
set.seed(123)


n = 1000
xm = 1
a = 1.5
tr <- 0.5
X <- rpareto(n,xm,a)
X
sort(X/sum(X))
max(X)/sum(X)

mu <- a/(a-1)
var <- ((xm^2)*a)/(((a-1)^2)*(a-2))*(a>2)
MAD <- (2*a)/((a-1)*(2*a-1))
G <- MAD/(2*mu)


e <- 1
an <- n^(-1/e)
Jt <- an + G*(1-an)
Ht <- (1/n) + (1/n)*(var/mu)^2

J <- an + Gini(X)*(1-an)
H <- sum((X/sum(X))^2)
G
Gini(X)

Jt
H

c("Gt" = G, "G" = Gini(X), "Jt" = Jt, "Ht" = Ht, "J" = J, "H" = H)
sum(sort(X/sum(X), TRUE)[1:5])
  
  Hind <- function(n,lambda){
    # X <- rexp(n,lambda)
    X <- rpareto(n,1,2.5)
    s <- X/sum(X)
    H <- sum(s^2)
  }



# n <- seq(100,10000,100)
# H <- vapply(n, function(x) Hind(x,lambda),1)
# plot(n,H, title(main = "Herfindahl index"))
