mc_int=function(n){
               u=runif(n)
               g=4*sqrt(1-u^2)
               mc_int=mean(g)
               mc_int
               }
F= rep(0,1000)
for (i in 1:1000){
F[i] = mc_int(i)}
plot(F,type='l', xlab="N",ylab="F",main="Monte Carlo Integration",ylim=c(2.5,3.8))
abline(h=3.14, col="red",lwd=2)
legend('topright', c("MC Approximation","True Value"), fill =c("black", "red"),cex=1,bty='n')
dev.print(device=postscript,"mcint.eps",width=7,height=7, horizontal=FALSE)
dev.off()