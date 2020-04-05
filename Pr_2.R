a <- -150000
Pr <- 0.015
N <- 1000
z <- qnorm(0.05)

Es <- ((S*(1-Pr)) + (a*(Pr)))*N
Es
Sd <-  abs(S-a)*sqrt((Pr*(1-Pr)))*sqrt(N)
Sd

S <- abs(a)*((N*Pr-z*sqrt(N*Pr*(1-Pr)))/(N*(1-Pr)+z*sqrt(N*Pr*(1-Pr))))
S

B <- 10000
set.seed(28)
Queen <- replicate(B, {
  X <- sample(c(S, a), replace = TRUE, N, prob = c((1-Pr), Pr))
  sum(X)
})
mean(Queen <=0)



set.seed(29)
Queen <- replicate(B, {
  Pr <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  X <- sample(c(S, a), replace = TRUE, N, prob = c((1-Pr), Pr))
  sum(X)
})
mean(Queen <= -1000000)






