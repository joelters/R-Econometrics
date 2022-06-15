#Functions

s_ecdf <- function(Y){
  n <- length(Y)
  Ys <- sort(Y)
  ecdf <- (1:n)/n
}


s_dgp <- function(n,mu = 0,s = 1){
  X <- rnorm(n,mu,s)
  Y <- exp(X)
}

s_reg <- function(X,Y, se = NULL){
  n <- length(Y)
  Y <- as.matrix(Y)
  bhat <- solve(t(X)%*%X)%*%(t(X)%*%Y)
  u <- Y - X%*%bhat
  if (is.null(se) == 1){
    Shat <- solve((1/n)*(t(X) %*% X))*as.numeric((1/(n-2))*(t(u)%*%(u)))
  }
  else{
    B <- 0
    for (i in 1:n){
      B <- B + u[i]^2*(X[i,]%*%t(X[i,]))
    }
    Shat <- (solve((1/n)*(t(X) %*% X)) %*% ((1/(n-2)*B)) %*% solve((1/n)*(t(X) %*% X)))
  }
  sehat <- sqrt(Shat[row(Shat) == col(Shat)]/n)
  res <- list("b" = bhat, "se" = sehat)
}