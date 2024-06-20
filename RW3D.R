library(plot3D)
n_steps = 20; n_exp = 500
disp = matrix(c(1,0,0,-1,0,0,0,1,0,0,-1,0,0,0,1,0,0,-1),3,6)
print(disp)
total_sum = c(0); total_sd2 = c(0)
# παράγουμε n_exp τυχαίους περίπατους
for(i in 1:n_exp) {
  m = disp[,sample(6,n_steps,replace=TRUE)]
  m = cbind(c(0,0,0),m)
  xyz = apply(m,1,cumsum)
  if(i==1)
    lines3D(xyz[,1],xyz[,2],xyz[,3],box=TRUE,pch=16,main="Random Walk 3D", xlab="x-position",bty="b2",axes=TRUE,label=TRUE, colkey=FALSE,ylab="y-position", zlab="z-positon",theta=40,phi=40,xlim=c(-7,7),ylim=c(-7,7),zlim=c(-7,7))
  else
    lines3D(xyz[,1],xyz[,2],xyz[,3],type="l",box=TRUE,pch=16,add=TRUE,main="Random Walk 3D", xlab="x-position",bty="b2",axes=TRUE,label=TRUE, colkey=FALSE,ticktypes="detailed",ylab="y-position", zlab="z-positon",theta=40,phi=40
  ,col=rgb(runif(255),runif(255),runif(255)))
  total_sum = total_sum + apply(xyz, 2 , sum)
  total_sd2 = total_sd2 + apply(xyz^2, 1 , sum)
} # end of for(i)
# επεξεργαζόμαστε τους περιπάτους
#plot3D(matrix(total_sum/n_steps/n_exp,1,3), xlim=c(-2,2), ylim=c(-2,2),xlab="mean x",ylab="mean y",main="mean value")
plot(total_sd2/n_exp,xlab="time",ylab="r^2",main="mean r^2 vs time")
