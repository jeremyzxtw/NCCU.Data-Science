##A sourcing problem with two suppliers S & P
#Market price
price=150

#Q:Capacity; c:Unit cost
#Supplier S
QS=5000
cS=120

#Supplier P
QP=10000
cP=100
#Fixed cost
fixcP=50000

#Simulate demand
set.seed(9527)
S=10000
D.weak=sample(seq(2000,8000,by=1),S,
              replace=TRUE)
D.strong=sample(seq(6000,14000,by=1),S,
                replace=TRUE)

#Vectors for profits
S.profit.weak=S.profit.strong=c()
P.profit.weak=P.profit.strong=c()


for(s in 1:S){
   #Supplier S & weak market
   S.profit.weak[s]=-cS*QS+price*min(QS,D.weak[s])
   #Supplier S & strong market
   S.profit.strong[s]=-cS*QS+price*min(QS,D.strong[s])
   ##
   #Supllier P & weak market
   P.profit.weak[s]=-cP*QP-fixcP+price*min(QP,D.weak[s])
   #Supplier P & strong market
   P.profit.strong[s]=-cP*QP-fixcP+price*min(QP,D.strong[s])
}

summary(S.profit.weak)
summary(S.profit.strong)
summary(P.profit.weak)
summary(P.profit.strong)


##Using E[Demand] for decision-making
-cS*QS+price*min(QS,5000)
-cS*QS+price*min(QS,10000)

-cP*QP-fixcP+price*min(QP,5000)
-cP*QP-fixcP+price*min(QP,10000)



