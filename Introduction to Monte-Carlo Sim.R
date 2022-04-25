##Simulating coin-tossing experiments
x=sample(c("H","T"),10,replace=TRUE)
x
table(x)
table(x)/10
#
x=sample(c("H","T"),100,replace=TRUE)
table(x)/100
#
x=sample(c("H","T"),10000,replace=TRUE)
table(x)/10000


##Simulating a game of chance
win=sample(c(-1,1),size=50,replace=T)
cum.win=cumsum(win)
cum.win
sum(win)
#
win=sample(c(-1,1),
           size=50,replace=T,
           prob=c(0.6,0.4))


KoP=function(n=50){
    win=sample(c(-1,1),size=n,replace=T)
    sum(win)
}
F=replicate(1000,KoP())
table(F)
plot(table(F))
sum(F==0)/1000

#
KoP=function(n=50){
  win=sample(c(-1,1),size=n,replace=T)
  cum.win=cumsum(win)
  c(F=sum(win),
    L=sum(cum.win>0),
    M=max(cum.win))
}
S=replicate(1000,KoP())
#
S[,1:10]
mean(S["L",])
mean(S["M",])
sum(S["M",]>10)/1000



##IC Module Analysis
preg=0.7
pbad.reg=0.1
pgood.reg=1-pbad.reg
#
pirreg=1-preg
pbad.irreg=0.4
pgood.irreg=1-pbad.irreg

ICmodule.sim=function(n=10){
   simulated.modules=rep(NA,n)
   #1 denotes regular
   #-1 denotes irregular
   labels=sample(c(1,-1),n,
                 prob=c(preg,pirreg),
                 replace=TRUE)
   if(any(labels==1)){
      simulated.modules[which(labels==1)]=
        sample(c("goodreg","badreg"),
               sum(labels==1),
               prob=c(pgood.reg,pbad.reg),
               replace=TRUE) 
   }
   if(any(labels==-1)){
      simulated.modules[which(labels==-1)]=
       sample(c("goodirreg","badirreg"),
              sum(labels==-1),
              prob=c(pgood.irreg,pbad.irreg),
              replace=TRUE) 
   }
   simulated.modules
}

ICmodule.sim()
S=10000
sim.table=replicate(S,ICmodule.sim())
dim(sim.table)

badnum=c()
badnumreg=c()
badnumirreg=c()
numreg=c()
numirreg=c()
for(i in 1:ncol(sim.table)){
    badnumreg[i]=sum(sim.table[,i]=="badreg")
    badnumirreg[i]=sum(sim.table[,i]=="badirreg")
    badnum[i]=sum(sim.table[,i]=="badreg")+
              sum(sim.table[,i]=="badirreg")
    numreg[i]=sum(sim.table[,i]=="badreg")+
              sum(sim.table[,i]=="goodreg")
    numirreg[i]=sum(sim.table[,i]=="badirreg")+
                sum(sim.table[,i]=="goodirreg")
}

sum(badnum==2)/S

sum(numreg==10 & badnum==2)
sum(numreg==10 & badnum==2)/sum(badnum==2)

k=1
sum(badnum==k)
sum(numirreg>=1 & badnum==k)
sum(numirreg>=1 & badnum==k)/sum(badnum==k)


