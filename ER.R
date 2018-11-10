install.packages('igraph')
library('igraph')

erdos.renyi.game()


n=20
p=0.2
m=(n*(n-1)/2)*p
er11 <- erdos.renyi.game(n, p, type="gnp")
er12 <- erdos.renyi.game(n, m, type="gnm")
# We will produce two plots and put them in 1 row and 2 columns:
op <- par(mfrow = c(1, 2))
# Produce plots
plot(er11, layout=layout.circle(er11), main="gnp")
plot(er12, layout=layout.circle(er12), main="gnm")

par(op)
print(length(E((er11))))

par(op)
print(length(E((er12))))



n=100
p1=0.01
p2=0.04
p3=0.1
er21 <- erdos.renyi.game(n, p1, type="gnp")
er22 <- erdos.renyi.game(n, p2, type="gnp")
er23 <- erdos.renyi.game(n, p3, type="gnp")
op <- par(mfrow = c(1, 3))
plot(er21, layout=layout.circle(er21), main="p=0.01")
plot(er22, layout=layout.circle(er22), main="p=0.04")
plot(er23, layout=layout.circle(er23), main="p=0.1")

par(op)

der21 <- degree(er21)
der22 <- degree(er22)
der23 <- degree(er23)
op <- par(mfrow = c(2, 3))
plot(er21, layout=layout.circle(er21), main="p=0.01")
plot(er22, layout=layout.circle(er22), main="p=0.04")
plot(er23, layout=layout.circle(er23), main="p=0.1")
hist(der21, col=rgb(0,0,1,.4), xlim=c(0,20), xlab="degree",ylab="freq", main="p=0.01")
hist(der22, col=rgb(1,0,0,.4), xlim=c(0,20), xlab="degree",ylab="freq", main="p=0.04")
hist(der23, col=rgb(0,1,0,.4), xlim=c(0,20), xlab="degree",ylab="freq", main="p=0.1")



n1=10
n2=30
#n3=200
er311 <- erdos.renyi.game(n1, 0.5/n1, type="gnp")
er312 <- erdos.renyi.game(n1, 1.1/n1, type="gnp")
er313 <- erdos.renyi.game(n1, 4/n1, type="gnp")
er321 <- erdos.renyi.game(n2, 0.5/n2, type="gnp")
er322 <- erdos.renyi.game(n2, 1.1/n2, type="gnp")
er323 <- erdos.renyi.game(n2, 4/n2, type="gnp")
op <- par(mfrow = c(2, 3))
plot(er311, layout=layout.fruchterman.reingold(er311), main="n=10, np=0.5")
plot(er312, layout=layout.fruchterman.reingold(er312), main="n=10, np=1")
plot(er313, layout=layout.fruchterman.reingold(er313), main="n=10, np=4")
plot(er321, layout=layout.fruchterman.reingold(er321), main="n=30, np=0.5")
plot(er322, layout=layout.fruchterman.reingold(er322), main="n=30, np=1")
plot(er323, layout=layout.fruchterman.reingold(er323), main="n=30, np=4")


gcc=c()
cc=c()
probability=c()
for (i in 1:70){
  prob=0.0002*i
  probability=c(probability, prob)
  er <- erdos.renyi.game(500, prob, type="gnp")
  comp <- clusters(er)
  largest <- max(comp$csize)
  gcc=c(gcc, largest)
  clust <- transitivity(er, type="global")
  cc=c(cc, clust)
}
par(mfrow = c(1,1))
plot(probability*500, gcc, pch=19, col=26, xlab="np", main="Size of the gigantic connected component")
