##Case: Operations at Conley Fisheries
#The quantity of fish caught each day
fish.Q = 3500

##S: The number of simulation runs
S=10000

#expected price at Rock port
mu.PR = 3.65
#the standard devaition of the price at Rock port
sigma.PR = 0.2

#Simulated price at Rock port
sim.PR=rnorm(S,mu.PR,sigma.PR)

#Simulated demand at Rock port 
sim.D = sample(c(0,1000,2000,3000,4000,5000,6000),S,
               prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
               replace = TRUE)

#The operation cost of the boat
oper.cost = 10000


F=c()
#F is the stochastic revenue when selling all the fish at Rockport
for(s in 1:S){
  F[s] = sim.PR[s]*min(fish.Q, sim.D[s]) - oper.cost
}


summary(F)
hist(F, breaks=200)

sum(F>1375)/S
sum(F<0)/S

quantile(F,0.975)
quantile(F,0.025)

lowestq5=F[which(F<=quantile(F,0.05))]
CVaRq5=mean(lowestq5)
CVaRq5



##Complications of Operations at Conley Fisheries
S=1000
PR.Rock=rnorm(S,3.65,0.25)
PR.Glou=rnorm(S,3.5,0.5)
summary(PR.Rock)
summary(PR.Glou)

x11(width=12,height=5)
par(mfrow=c(1,2))
hist(PR.Rock,breaks=50)
hist(PR.Glou,breaks=50)


install.packages("EnvStats")
#Install the package above if needed
library(EnvStats)
D.Glou=round(rtri(S,2000,6000,5000),0)
D.Rock=sample(c(0,1000,2000,3000,4000,5000,6000),S,
              prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
              replace = TRUE)

x11(width=12,height=5)
par(mfrow=c(1,2))
hist(D.Rock)
hist(D.Glou)


##S: The number of simulation runs
S=10000

#The quantity of fish caught each day
fullload = 3500
frac=runif(S,0.7,1)

fish.Q=round(fullload*frac,0)


#expected price at Rock port
mu.PRR = 3.65
#the standard devaition of the price at Rock port
sigma.PRR = 0.2
#expected price at Glou
mu.PRG = 3.5
#the standard devaition of the price at Glou
sigma.PRG = 0.5

#Simulated prices
sim.PRR=rnorm(S,mu.PRR,sigma.PRR)
sim.PRG=rnorm(S,mu.PRG,sigma.PRG)

#Simulated demand 
sim.DR = sample(c(0,1000,2000,3000,4000,5000,6000),S,
               prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
               replace = TRUE)

sim.DG = round(rtri(S,2000,6000,5000),0)

#The operation cost of the boat
oper.cost = 10000


F=c()
G=c()
#F is the stochastic revenue when selling all the fish at Rockport
for(s in 1:S){
  F[s] = sim.PRR[s]*min(fish.Q[s], sim.DR[s]) - oper.cost
  G[s] = sim.PRG[s]*min(fish.Q[s], sim.DG[s]) - oper.cost
}


summary(G)
summary(F)
x11(width=12,height=5)
par(mfrow=c(1,2))
hist(G, breaks=200)
hist(F, breaks=200)

sum(G>1375)/S
sum(G<0)/S

sum(F>1375)/S
sum(F<0)/S

quantile(G,0.975)
quantile(G,0.025)

quantile(F,0.975)
quantile(F,0.025)

lowestq5G=G[which(G<=quantile(G,0.05))]
CVaRq5G=mean(lowestq5G)
CVaRq5G

lowestq5F=F[which(F<=quantile(F,0.05))]
CVaRq5F=mean(lowestq5F)
CVaRq5F



###Modeling exponentially distributed time
S=10000
calls=c()
for(s in 1:S){
    k=0
    totaltime=0
    while(totaltime<=60){
      totaltime=totaltime+rexp(1,1/10)
      k=k+1
      #cat("totaltime=",totaltime,"; k=",k,"\n")
    }
    calls[s]=k-1
    if(s%%1000==0)print(s)
}

plot(table(calls)/S)
lines(min(calls):max(calls),
      dpois(min(calls):max(calls),6),col='red',lty=2,lwd=3)





###Case: Simulation of Queues
S = 200

simulation = function(server, S){
  results = matrix(NA, nrow = S, ncol = 6, dimnames = list(NULL, c("total_cost", "idle_cost", "loss_cost", "wait_cost", "utilization", "interval")))
  for(s in 1:S){
    num_server = server
    num_idle_server = num_server #idle server的數量
    num_serving = 0 #正在serve的數量
    open_hour = 8
    open_minute = open_hour*60
    
    record_end_serve_time = double() # 紀錄server結束服務客人的時間
    
    num_loss_cust = 0 #Loss 客人的數量
    total_time_server_idle = 0 #min 總共所有server idle的時間
    total_wait_time = 0 #min 總共所有客人等待的時間
    
    #排隊隊伍 cus_ID:客人ID, arr_time:客人抵達時間, quit_time:客人抵達時間+客人願意等待時間
    queue = matrix(nrow = 0, ncol = 3,dimnames = list(NULL, c("cus_ID", "arr_time", "quit_time"))) 
    cust_list = matrix(nrow = 0, ncol = 4, dimnames = list(NULL, c("cus_ID", "arr_time", "quit_time", "description"))) #紀錄當天所有客人
    ID_count = 1 #起始客人ID編號
    ###simulate
    for (t in 1:open_minute) { # convert to minute
      #end serve, release server
      if(length(which(record_end_serve_time == t)) != 0){ #check 這個時間點t有沒有結束服務(end serve time = t)的server
        num_idle_server = num_idle_server + length(which(record_end_serve_time == t)) #idle server數量+結束服務的server數量
        num_serving = num_serving - length(which(record_end_serve_time == t))#正在服務的客人數量-結束服務的客人數
        record_end_serve_time[which(record_end_serve_time == t)] = 0 #重置server結束時間
      }
      
      #check loss customer 
      num_leave = length(which(queue[, "quit_time"] == t)) #多少個客人在這個時間quit
      if(num_leave != 0){
        num_loss_cust = num_loss_cust + num_leave #紀錄Loss客人數量
        queue = queue[-which(queue[, "quit_time"] == t), , drop = FALSE]#從隊伍移除quit客人
        cust_list[which(cust_list[, "quit_time"] == t), "description"] = paste("Loss at time:", t)#紀錄客人狀態
      }
      
      #new customer coming 
      if(t <= (open_minute - 30)){ ###關店前30分鐘不收客人
        arrived_customer = rpois(1, 0.1) #6 customer per hour => 0.1 customer per minute
        if(arrived_customer != 0){ #如果有客人抵達
          for(n in 1:arrived_customer){ #record each customer
            willing_wait_time = ceiling(rexp(1, 4) * 60) #0.25 per hour
            queue = rbind(queue, c(ID_count, t, t+willing_wait_time)) #把每個客人放進排隊隊伍
            cust_list = rbind(cust_list, c(ID_count, t, t+willing_wait_time, ""))
            ID_count = ID_count + 1
          }
        }
      }
      
      arrived_customer = 0 #reset arrived customer
      
      if(num_idle_server != 0){ # check 有沒有idle server
        for(n in 1:num_idle_server){ #對每一個idle server
          if(dim(queue)[1] == 0){ #if 沒人在排隊
            total_time_server_idle = total_time_server_idle + 1 #總idle時間加1
          }else{
            #get customer 
            id_ = queue[[1,"cus_ID"]]
            cust_wait_time = t - queue[[1, "arr_time"]] #calculate the wait time of the customer 
            total_wait_time = total_wait_time + cust_wait_time 
            queue = queue[-1,,drop = FALSE] #remove the customer from queue
            serve_time = round(rexp(1, 3) * 60) #simulate serve time and convert to minute 
            end_serve_time = t + serve_time #calculate the end time
            record_end_serve_time = c(record_end_serve_time, end_serve_time) #紀錄serve結束時間
            cust_list[which(cust_list[, "cus_ID"] == id_), "description"] = paste("Finish serve at:", end_serve_time)
            num_serving = num_serving + 1 #加上正在Serve的數量
          }
        }
      }
      num_idle_server = num_server - num_serving #計算目前idle server數量
    }#end t
    
    interval = mean(diff(as.numeric(cust_list[, "arr_time"]), lag = 1, differences = 1))
    #loss cost:30, idle cost(per hour):10 (per minute):10/60, wait cost(per hour):5 (per minute):5/60
    total_cost = num_loss_cust * 30 + total_time_server_idle * (1/6) + total_wait_time * (1/12)
    utilization = (num_server * 480 - total_time_server_idle) / (num_server * 480)
    results[s, ] = c(total_cost, total_time_server_idle * (1/6), num_loss_cust * 30, total_wait_time * (1/12), utilization,interval)
  }
  return(results)
}

##put the mean cost of different number of server
results = matrix(NA, nrow = 10, ncol = 6,
                 dimnames = list(NULL, c("avg_total_cost", "avg_idle_cost", "avg_loss_cost", "avg_wait_cost", "avg_utilization", "avg_interval")))
for(n in 1:10){ # 1到10台server
  sim_results = simulation(n, 200)
  results[n, ] = apply(sim_results, 2, mean) #calcalate mean cost of 200 simulation
}
results


plot(results[, "avg_total_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg total cost",
     xlab = "number of server", ylab = "avg total cost")

plot(results[, "avg_idle_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg idle cost",
     xlab = "number of server", ylab = "avg idle cost")

plot(results[, "avg_loss_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg loss customer cost",
     xlab = "number of server", ylab = "avg loss customer cost")

plot(results[, "avg_wait_cost"], type = "b", col = "blue",lwd=2, pch=15, main = "avg wait cost",
     xlab = "number of server", ylab = "avg wait cost")

plot(results[, "avg_utilization"], type = "b", col = "blue",lwd=2, pch=15, main = "avg utilization",
     xlab = "number of server", ylab = "avg utilization")



###Verify Memoryless
Tsamples=rexp(S,1/10)
sum(Tsamples>5)/S
sum(Tsamples>15)/sum(Tsamples>10)


###Verify gamma distribution
S=10000
time.five=c()
for(s in 1:S){
  time.five[s]=sum(rexp(5,1/10))
}

min.x=round(min(time.five),2)
max.x=round(max(time.five),2)
x=seq(min.x, max.x, 0.01)
shape.est=5
scale.est=10
hist(time.five,breaks=50,freq=FALSE)
lines(x,dgamma(x,shape=shape.est,scale=scale.est),col='red',lwd=3)


x=seq(1,30,0.1)
plot(x,dexp(x,0.2),type='l',lwd=2)
lines(x,dgamma(x,shape=1,scale=1/0.2),col='red',lty=2)
lines(x,dgamma(x,shape=2,scale=1/0.2),col='blue')
lines(x,dgamma(x,shape=5,scale=1/0.2),col='green')



##Project duration Simulation
taskt.mean=c(20,50,60,15,65,35,30,10)
taskt.stdev=c(7,10,12,3,30,15,5,3)
#Assuming task time as a RV X ~ gamma(shape, scale)
#E[X]=shape*scale & Var[X]=shape*scale^2
shape.est=c()
scale.est=c()
scale.est=(taskt.stdev)^2/taskt.mean
scale.est

shape.est=taskt.mean/scale.est
shape.est
#Need to check both parameters>0 or not

#Define BT:Begin Time & FT: Finish Time

S=10000
penaltyperday=100000
B.reduced=0
#B.reduced=1 if crashing activity B
simDays.temp=c()
simPenalty.temp=c()
for(i in 1:S){
#i:index for the ith simulation
    taskt.i=c()
    for(j in 1:length(taskt.mean)){
    #j:activity index
        shape.j=shape.est[j]
        scale.j=scale.est[j]
        taskt.i.j=rgamma(1,shape=shape.j,
                         scale=scale.j)
        taskt.i[j]=round(taskt.i.j,0)
    }
    ##Assuming NO reduction in task B time
    BT.A=BT.C=BT.E=0
    ET.A=BT.A+taskt.i[1]
    ET.C=BT.C+taskt.i[3]
    ET.E=BT.E+taskt.i[5]
    #
    BT.B=ET.A
    if(B.reduced==0){
       ET.B=BT.B+taskt.i[2]
    }
    ##
    if(B.reduced==1){
      ET.B=BT.B+round(taskt.i[2]*0.8,0)
    }
    #
    BT.D=max(ET.B,ET.C)
    ET.D=BT.D+taskt.i[4]
    #
    BT.F=ET.E
    ET.F=BT.F+taskt.i[6]
    #
    BT.G=ET.D
    ET.G=ET.D+taskt.i[7]
    #
    BT.H=ET.G
    ET.H=BT.H+taskt.i[8]
    #
    simDays.temp[i]=max(ET.H, ET.F)
    delay.i=max(simDays.temp[i]-130,0)
    simPenalty.temp[i]=delay.i*penaltyperday
}

#No crashing activitiy B
simDays.base=simDays.temp
simPenalty.base=simPenalty.temp
#
summary(simDays.base)
sd(simDays.base)
sum(simDays.base<=130)/S
#
summary(simPenalty.base)

#Do crash activitity B
simDays.reduced=simDays.temp
simPenalty.reduced=simPenalty.temp
summary(simDays.reduced)
sd(simDays.reduced)
sum(simDays.reduced<=130)/S
#
summary(simPenalty.reduced)







###Howard's retirement fund
savings=1000000
inflation=0.03
withdraw=seq(50000,80000,500)
life.mu=20
life.sd=10
scale.est=
shape.est=

growth.mu=0.08
growth.sd=0.02

S=5000
prob.broke=c()

life=rgamma(S,shape=shape.est,scale=scale.est)
sim.growth=list()
for(s in 1:S){
  sim.growth[[ ]]=rnorm(         , growth.mu, growth.sd)
}


for(i in 1:length(withdraw)){
  broke.i=0
  for(s in 1:S){
    years.togo=round(life[s],0)
    j=1
    spending=withdraw[i]
    savings.left=savings
    while(j<=years.togo & savings.left>0){
      #cat("life=",j,"$left=",savings.left,"\n")
      savings.left=
        savings.left=
        spending=
        
    }
    if(savings.left<=0){broke.i=broke.i+1}
    if(s%%1000==0){cat("Withdraw=",withdraw[i],
                       "S=",s,"\n")}
  }
  prob.broke[i]=broke.i/S
}

prob.broke
which(prob.broke<0.05)
withdraw[which(prob.broke<0.05)]

plot(withdraw,prob.broke,type='l',lwd=3)
points(withdraw,prob.broke,pch=1)
abline(h=0.05,col='green',lty=2,lwd=2)

