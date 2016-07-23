A <- rnorm(100000, 10, 3)

mean(A)
var(A)

B <- rbinom(100000, 1, .3)

mean(B)
var(B)

C <- A * B

B2 <- rbeta(100000, sum(B == 1) + 1, length(B) - sum(B == 1) + 1)

mean(C) - mean(A) * mean(B)
var(C)

C2 <- A * B2

mean(C2)
var(C2)

var(A)
