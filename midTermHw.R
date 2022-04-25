#########################Global funcs and param#####################################3

history.demand = c(105,95,75,70,70,110,105,90,65,80,90,120)
#april, may, june, july, ..........., march

calEarning = function(Pi,Di) {
  if(Pi>Di) {
    return(10000*Di-6000*Pi)
  }
  else {
    return(4000*Pi + 400*(Di-Pi))
  }
}

calDemand = function(Hi) {
  return(round(Hi*(1+rnorm(1,0,5)/100)*(1+rnorm(1,0,10)/100)))
}

calSupply = function(Qi,Pi,monthIdx) { #Qi: 甄試的人, Pi: 上個月工作得人,
  Ai = 0
  if(monthIdx==6 | monthIdx==10) {
    buff = runif(1,80,100)}
  else if (monthIdx==2 | monthIdx==3 | monthIdx==4 | monthIdx==5){
    buff = runif(1,95,100)}
  else{
    buff = runif(1,90,100)}
  if (Qi!=0){
    Ai = rbinom(1,Qi,0.7)
  }
  return(round(Pi*(buff/100)+Ai))
  
}

######################################## Question I ###########################################

earningFixed = function (bufferApplyP){  #will range from 10~110
  totalEarnList = rep(0,12)
  anaylists = 63
  bufferApplyP = round(bufferApplyP)

  demand = calDemand(history.demand[1])
  totalEarnList[1] = calEarning(anaylists,demand)

  for (month in 2:12) {

    if (month==4) {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(bufferApplyP,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand)
    } else {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(0, anaylists, month)
      totalEarnList[month] = calEarning(anaylists, demand)
    }

  }
  return(sum(totalEarnList))
}

sim.earningFixed = function(x){
  S = 5000
  sims = replicate(S, earningFixed(x))
  return(mean(sims))
}

library(pracma)
res=fminbnd(sim.earningFixed,11,110,maximum = TRUE)

######################################## Question II ###########################################

earningFlexible = function (bufferApplyP){  #will range from 10~110
  totalEarnList = rep(0,12)
  anaylists = 63
  bufferApplyP = round(bufferApplyP)
  firstHalf = round(bufferApplyP/2)
  secondHalf = bufferApplyP - firstHalf
  
  demand = calDemand(history.demand[1])
  totalEarnList[1] = calEarning(anaylists,demand)
  
  for (month in 2:12) {
    
    if (month==4) 
      {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(firstHalf,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand)
      }
    
    else if (month==6) 
      {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(secondHalf,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand) 
      }
    
     else 
      {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(0, anaylists, month)
      totalEarnList[month] = calEarning(anaylists, demand)
      }
    
  }
  return(sum(totalEarnList))
}

sim.earningFlexible = function(x){
  S = 5000
  sims = replicate(S, earningFlexible(x))
  return(mean(sims))
}

library(pracma)
res=fminbnd(sim.earningFlexible,11,110,maximum = TRUE)

######################################## Question III-1 ###########################################

earningTwoRecruitsFixed = function (recruits){  #will range from 10~110
  totalEarnList = rep(0,12)
  anaylists = 63
  recruitOne = round(recruits[1])
  recruitTwo = round(recruits[2])

  demand = calDemand(history.demand[1])
  totalEarnList[1] = calEarning(anaylists,demand)
  
  for (month in 2:12) {
    
    if (month==4) 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(recruitOne,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand)
    }
    
    else if (month==10) 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(recruitTwo,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand) 
    }
    
    else 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(0, anaylists, month)
      totalEarnList[month] = calEarning(anaylists, demand)
    }
    
  }
  return(sum(totalEarnList))
}

sim.earningTwoRecruitsFixed= function(x){
  S = 1000
  sims = replicate(S, earningTwoRecruitsFixed(x))
  return(mean(sims))
}

library(pracma)
res=fminsearch(sim.earningTwoRecruitsFixed,
               c(20,20),
               lower=c(11,11), 
               upper=c(110,110),
               method=c("Hooke-Jeeves"),
               minimize=FALSE)


######################################## Question III-2 ###########################################

earningTwoRecruitsFlexible = function (recruits){  #will range from 10~110
  
  totalEarnList = rep(0,12)
  anaylists = 63
  
  recruitOne = round(recruits[1])
  recruitOneHalf1 = round(recruitOne/2)
  recruitOneHalf2 = recruitOne - recruitOneHalf1
  
  recruitTwo = round(recruits[2])
  recruitTwoHalf1 = round(recruitTwo/2)
  recruitTwoHalf2 = recruitTwo - recruitTwoHalf1
  
  demand = calDemand(history.demand[1])
  totalEarnList[1] = calEarning(anaylists,demand)
  
  for (month in 2:12) {
    
    if (month==4) 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(recruitOneHalf1,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand)
    }

    else if (month==6) 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(recruitOneHalf2,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand) 
    }
    
    else if (month==9) 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(recruitTwoHalf1,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand) 
    }
    
    else if (month==11) 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(recruitTwoHalf2,anaylists,month)
      totalEarnList[month] = calEarning(anaylists, demand) 
    }
    
    else 
    {
      demand = calDemand(history.demand[month])
      anaylists = calSupply(0, anaylists, month)
      totalEarnList[month] = calEarning(anaylists, demand)
    }
    
  }
  return(sum(totalEarnList))
}

sim.earningTwoRecruitsFlexible = function(x){
  S = 5000
  sims = replicate(S, earningTwoRecruitsFlexible(x))
  return(mean(sims))
}

library(pracma)
res=fminsearch(sim.earningTwoRecruitsFlexible,
               c(20,20),
               lower=c(11,11), 
               upper=c(110,110),
               method=c("Hooke-Jeeves"),
               minimize=FALSE)



