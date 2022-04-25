#Below is a function that simulates random samples from 
#the Myerson distribution in section 4.4 of Roger Myerson's 2005 book
#This is a generalized version of normal & lognormal distribution
rMyerson <- function(n,q1,q2,q3,lower=-Inf, upper=Inf,tl=0.5){
  #n: the number of random samples
  #q1: xx percentile, xx<50 & usually xx=25
  #q2: 50 percentile
  #q3: xx percentile, xx>50 & usually xx=75
  #lower: minimum possible value
  #upper: maximum possible value
  #tl: tail probability = P(X<q1)+P(X>q3)
  ######################################
  x=runif(n)
  x[which(x>0.999999)]=0.999999
  x[which(x<0.000001)]= 0.000001
  #Above avoids sampling values that are too extreme
  norml=qnorm(x)/qnorm(1-tl/2)
  br=(q3-q2)/(q2-q1)
  if(br==1){
    res=q2+(q3-q2)*norml
  } else {
    res=q2+(q3-q2)*(br^norml-1)/(br-1)
  }
  pmin(pmax(res, lower), upper)
}


##Case: Scotia Snowboards
#P(weather is cold)
p.cold=1/3
#Demand parameters
q1.normal=60000
q2.normal=75000
q3.normal=90000

q1.cold=80000
q2.cold=100000
q3.cold=125000

#Cost parameters
unitcost=20
unitprice=48
salvage=8

#profit function given x as order quantity
profit=function(x=80000){
  #x:production quantity
  weather=sample(c(1,0),1,replace=TRUE,
                 prob=c(p.cold,1-p.cold))
  #1: cold; 0:weather
  if(weather==1){
    d=rMyerson(1,q1.cold,q2.cold,q3.cold,
               lower=0)
  }else{
    d=rMyerson(1,q1.normal,q2.normal,q3.normal,
               lower=0)
  }
  demand=round(d,0)
  profit=(unitprice-unitcost)*min(x, demand)+
    (salvage-unitcost)*max(x-demand, 0)
  profit
}

#x as the order quantity. Try different ordering decisions
x.val=seq(50000,150000,1000)

#Number of simulation runs
S=15000

#Simulate random demand
#1: cold; 0:weather
sim.weather=sample(c(1,0),S,replace=TRUE,
                   prob=c(p.cold,1-p.cold))

sim.demand=rep(0,S)
for(s in 1:S){
  #1: cold; 0:weather
  if(sim.weather[s]==1){
    sim.demand[s]=rMyerson(1,q1.cold,q2.cold,q3.cold,
                           lower=0)
  }else{
    sim.demand[s]=rMyerson(1,q1.normal,q2.normal,q3.normal,
                           lower=0)
  }
  sim.demand[s]=round(sim.demand[s],0)
}

profit=function(x=80000,d){
  #d: demand realizations
  profit.val=c()
  for(i in 1:length(d)){
    profit.val[i]=(unitprice-unitcost)*min(x, d[i])+
      (salvage-unitcost)*max(x-d[i], 0)
  }
  profit.val
}


sim.profit=matrix(0,nrow=S,ncol=length(x.val))
avg.profit=c()
sd.profit=c()

start.time <- Sys.time()

for(i in 1:length(x.val)){
  sim.profit[,i]=profit(x.val[i],d=sim.demand)
  avg.profit[i]=mean(sim.profit[,i])
  sd.profit[i]=sd(sim.profit[,i])
  cat("production quantity:", x.val[i], "\n")
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

x11(width=12,height=5)
par(mfrow=c(1,2))
plot(x.val,avg.profit,type='l',xlab="production quantity",lwd=3)
plot(x.val[which(avg.profit>1800000)],
     avg.profit[which(avg.profit>1800000)],
     type='l',xlab="production quantity",lwd=3)


x.val[which.max(avg.profit)]
max(avg.profit)



##Assess the value of perfect information
cu=unitprice-unitcost
co=unitcost-salvage
cu
co
#Calculate the critical fractile
frac=cu/(cu+co)
frac

dcold=rMyerson(S,q1.cold,q2.cold,q3.cold,lower=0)
x.cold=quantile(dcold,frac,names=FALSE)
x.cold=round(x.cold,0)
x.cold

dnormal=rMyerson(S,q1.normal,q2.normal,q3.normal,lower=0)
x.normal=quantile(dnormal,frac,names=FALSE)
x.normal=round(x.normal,0)
x.normal

profit.x.cold=profit(x.cold,dcold)
profit.x.normal=profit(x.normal,dnormal)
mean(profit.x.cold)
mean(profit.x.normal)

frac

profit.perfectinfo=c()

for(s in 1:S){
  #1: cold; 0:weather
  if(sim.weather[s]==1){
    x=x.cold
  }else{
    x=x.normal
  }
  demand=sim.demand[s]
  profit.perfectinfo[s]=
    (unitprice-unitcost)*min(x, demand)+
    (salvage-unitcost)*max(x-demand, 0)
}

mean(profit.perfectinfo)

max(avg.profit)

mean(profit.perfectinfo)-max(avg.profit)






##Starlux Airlines
D=function(Price,b0=600,b1=-5){
  D.val=b0+b1*Price+rnorm(1,0,10)
  return(max(D.val,0))
}

D(100)

capacity=150
refund=0.5
penalty=2.5
#
mu.noshow=0.1
sd.noshow=0.025

beta.params=function(x){
  c(f1=x[1]/(x[1]+x[2])-mu.noshow,
    f2=(x[1]*x[2])/((x[1]+x[2])^2*(x[1]+x[2]+1))-sd.noshow^2)
}

library(rootSolve)
multiroot(beta.params,c(5,5),positive=TRUE)

shape1.est=14.29985
shape2.est=128.69853

fixed.C=12000

sim.revenue=function(Price,Quantity,S=1000){
  sold=show=board=bumped=empty=rep(0,S)
  profit=rep(0,S)
  for(s in 1:S){
    demand.s=D(Price)
    sold[s]=min(demand.s,Quantity)
    noshowfrac.s=rbeta(1,shape1.est,shape2.est)
    show[s]=round(sold[s]*(1-noshowfrac.s))
    board[s]=min(show[s],capacity)
    bumped[s]=show[s]-board[s]
    empty[s]=max(0,capacity-board[s])
    profit[s]=Price*sold[s]-(sold[s]-show[s])*Price*refund-
      bumped[s]*Price*penalty-fixed.C
  }
  return(c(mean(profit),mean(board),mean(show),mean(empty),
           sum(bumped>0)/S,mean(profit/board)))
  #return(c(mean(profit)))
}



sim.revenue(90,150)


sim.revenue.x=function(x){
  S=1000
  sold=show=board=bumped=empty=rep(0,S)
  profit=rep(0,S)
  Price=floor(x[1])
  Quantity=floor(x[2])
  for(s in 1:S){
    demand.s=D(Price)
    sold[s]=min(demand.s,Quantity)
    noshowfrac.s=rbeta(1,shape1.est,shape2.est)
    show[s]=round(sold[s]*(1-noshowfrac.s))
    board[s]=min(show[s],capacity)
    bumped[s]=show[s]-board[s]
    empty[s]=max(0,capacity-board[s])
    profit[s]=Price*sold[s]-(sold[s]-show[s])*Price*refund-
      bumped[s]*Price*penalty-fixed.C
  }
  #return(c(mean(profit),mean(board),mean(show),mean(empty),
  #         sum(bumped>0)/S,mean(profit/board)))
  return(c(mean(profit)))
}




##Hooke-Jeeves Algorithm
library(pracma)
res=fminsearch(sim.revenue.x,
               c(70,150),
               lower=c(30,150), 
               upper=c(120,200),
               method=c("Hooke-Jeeves"),
               minimize=FALSE)




library(dfoptim)
res.b=hjkb(par=c(70,150),fn=sim.revenue.x,
           lower=c(30,150), upper=c(120,200),
           control=list(maxfeval=2000000, 
                        maximize=TRUE, 
                        info=TRUE))
res.b

