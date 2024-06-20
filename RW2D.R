require("graphics")
n_steps = 5; n_exp = 100
disp = matrix(c(1,0,-1,0,0,1,0,-1),2,4)
print(disp)
total_sum = c(0); total_sd2 = c(0)
# παράγουμε n_exp τυχαίους περίπατους
for(i in 1:n_exp) {
  m = disp[,sample(4,n_steps,replace=TRUE)]
  m = cbind(c(0,0),m)
  xy = apply(m,1,cumsum)
  if(i==1)
    plot(xy, type='l',main="Random Walk 2D", xlab="x-axis",ylab="y-axis", xlim=c(-5,5),ylim=c(-5,5))
  else
    points(xy,type="l",col=rgb(runif(255),runif(255),runif(255)))
  total_sum = total_sum + apply(xy, 2 , sum)
  total_sd2 = total_sd2 + apply(xy^2, 1 , sum)
} # end of for(i)

# επεξεργαζόμαστε τους περιπάτους
plot(matrix(total_sum/n_steps/n_exp,1,2), xlim=c(-2,2), ylim=c(-2,2),xlab="mean x",ylab="mean y",main="mean value")
plot(total_sd2/n_exp,xlab="time",ylab="r^2",main="mean r^2 vs time")
