#Visualize pdf of binomial RV
x=0:10
plot(x,dbinom(x,size=10,prob=0.2),type="S",
     ylim=c(0,0.3))
lines(x,dbinom(x,size=10,prob=0.5),type="S",
      col="green",lwd=3)
lines(x,dbinom(x,size=10,prob=0.8),type="S",
      col="red",lwd=3)

#Visualize cdf of binomial RV
x=0:10
plot(x,pbinom(x,size=10,prob=0.2),type="S",
     ylim=c(0,1))
lines(x,pbinom(x,size=10,prob=0.5),type="S",
      col="green",lwd=3)
lines(x,pbinom(x,size=10,prob=0.8),type="S",
      col="red",lwd=3)

##Poisson
S=5000
lambda1=2
lambda2=3

pdata1=rpois(S,lambda1)
pdata2=rpois(S,lambda2)

mean(pdata1)
var(pdata1)
plot(table(pdata1))
plot(table(pdata2))

pdatasum=pdata1+pdata2
mean(pdatasum)
var(pdatasum)
plot(table(pdatasum))
plot(table(pdatasum)/length(pdatasum))
lines(0:15,dpois(0:15,mean(pdatasum)),
      col='green')



##Negative Binomial
S=5000
NBdata=rnbinom(S,size=3,prob=0.2)
plot(table(NBdata))
table(NBdata)

mean(NBdata)
var(NBdata)

plot(table(NBdata[NBdata<=30]))
table(NBdata[NBdata<=30])

prob.est=mean(NBdata)/var(NBdata)
size.est=mean(NBdata)*prob.est/(1-prob.est)


plot(table(NBdata[NBdata<=30])/
        length(NBdata),ylim=c(0,0.12))
lines(0:30,dnbinom(0:30,size=size.est,
           prob=prob.est),col='green',lwd=2)
lines(0:30,dpois(0:30,mean(NBdata)),
      col='red')


