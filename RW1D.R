require("graphics")
n_steps = 200; n_exp = 2000
walks = c()
plot(rep(0,n_steps+1), type="l", ylim = c(-n_steps/2,n_steps/2), xlab="time",ylab="X_N",main="Random Walk")
# παράγουμε n_exp τυχαίους περίπατους
for(i in 1:n_exp) {
  deigma = sample(c(-1,1), n_steps, replace = TRUE)
  x = cumsum(c(0,deigma))
  walks=c(walks,x)
lines(x, col=rgb(runif(255),runif(255),runif(255)))
}
# επεξεργαζόμαστε τους περιπάτους
walks_mat = matrix(walks, n_steps + 1,n_exp)
  plot(apply(walks_mat, 1, mean),ylim=c(-1,1),xlab="time",ylab="mean",main="mean value")
  plot(apply(walks_mat, 1, sd),ylim=c(0,10),xlab="time",ylab="standard deviation",main="standard deviation")
  plot(apply(walks_mat, 1, sd)^2,ylim=c(0,n_steps),xlab="time",ylab="standard deviation^2",main="standard deviation^2")