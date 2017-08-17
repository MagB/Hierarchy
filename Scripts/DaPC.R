#Another approach is to use discriminant function analysis to try to see if individuals can be reassigned to their original groups.
#I think this analysis shows that there is some differentiation between groups. Some individuals can be reassigned to thier group with high probability, but most cannot
install.packages("adegenet", dep=TRUE)
library("adegenet")
update.packages("adegenet")
require(pegas)
require(PopGenReport)


#read.genetable is from popgenreport
platy <- read.genetable("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat_by_plot.csv", ind=1, pop=c(2,3), other.min=4, other.max=6, oneColPerAll = FALSE, sep=":", ploidy=2)
platy <- read.genetable("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat.csv", ind=1, pop=c(2,3), other.min=4, other.max=6, oneColPerAll = FALSE, sep=":", ploidy=2)

str(platy)



barplot(table(pop(platy)), col=funky(17), las=3,
        xlab="Population", ylab="Sample size")

temp=summary(platy)
plot(temp$Hexp, temp$Hobs, pch=20, cex=3, xlim=c(.4,1), ylim=c(.4,1))

#Kmeans
grp=find.clusters( platy, max.n.clust = 40)

grp=find.clusters( platy, max.n.clust = 40,stat=c("BIC"), choose.n.clust=T)
grp=find.clusters(platy, clust=NULL, n.pca=NULL,n.clust=NULL, stat=c("BIC"),choose.n.clust=TRUE, criterion=c("diffNgroup"),max.n.clust = 40, n.iter=1e5,n.start=10,  scale=FALSE, pca.select=c("percVar"), perc.pca=NULL,glPca=NULL)

plot(grp$Kstat, type="b", col="blue")

install.packages("HSAUR")
library(HSAUR)
install.packages("cluster")
library("cluster")
install.packages("fpc")
library("fpc")
dat=platy@tab[,1:18]
plotcluster(platy@tab[,1:18], grp$grp)
clusplot(dat, grp$grp, color=TRUE, shade=TRUE, labels=2, lines=1)

km=kmeans(dat, center=6)
km$cluster
plot(dat[,2:18],col=km$cluster)
points(km$center,col=1:2,pch=8,cex=1)
#DPCA but with 7 clusters and ask can individuals be reassigned to orginal population?
dapc.7=dapc(platy)
summary(dapc.7)
#the sot "assign.per.pop indicates the proportions of successful reassignment based on the discriminant functions of individuals to thie original clusters.
#low values suggest highly admixed groups. 
assignplot(dapc.7)
compoplot(dapc.7, posi="bottomright", txt.leg=paste("Cluster", 1:7), lab="", ncol=1, xlab="individuals")

scatter(dapc.7)
#DPCA
dapc1=dapc(platy, grp$grp)
scatter(dapc1)
round((dapc1$posterior),3)
#just plain PCA
library(hierfstat)
nancycats <- genind2hierfstat(platy)
basicstat <- basic.stats(nancycats, diploid = TRUE, digits = 2) 
x <- indpca(nancycats) 
plot(x, cex = 0.7)


head(platy@tab)
#next option
#14 pops
pop(platy)
#here a PCA is fit to the data. We keep 200 PC's ; here we just keep all the PC components
grp=find.clusters( platy, max.n.clust = 40)
#retain 30 (retaining 40 gives 6 as lowest BIC, but 35 gives 6 or 7, 30 gives 8 as lowest) 
#retain 200 PC and 6 clusters
#choose 5 clusters based on lowest BIC
grp$size
grp$grp
#output of find.clusters
#individuals are assigned to groups.
names(grp)
dapc1=dapc(platy, grp$grp)
table(pop(platy), grp$grp)



dapc1
scatter(dapc1)
rainbow(11)

myCol=c("darkblue", "purple", "green", "orange", "red", "blue", "black", "grey", "purple", "pink", "yellow")
scatter(dapc1, scree.da = F, bg="white", pch=20, cell=0, cstar=0, col=myCol, solid=0.4, cex=2, clab=0, leg=T, txt.leg=paste("Cluster", 1:11))
contrib=loadingplot(dapc1$var.contr, axis=2, thres=0.07, lab.jitter = 1)

freq9=tab(genind2genpop(platy[loc=c("loc9")]), freq=T)
freq7=tab(genind2genpop(platy[loc=c("loc7")]), freq=T)
par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.1, .1), las=3)
matplot(freq9, type="l")
str(freq9)
#next
dapc2=dapc(platy, n.da=100, n.pca=50)
temp=optim.a.score(dapc2)

#
set.seed(999)
pramx <- xvalDapc(tab(platy, NA.method = "mean"), pop(platy))
set.seed(999)
system.time(pramx <- xvalDapc(tab(platy, NA.method = "mean"), pop(platy),
        n.pca = 60:80, n.rep = 10,
        parallel = "multicore", ncpus = 4L))
set.seed(999)
system.time(pramx <- xvalDapc(tab(platy, NA.method = "mean"), pop(platy),
        n.pca = 70:75, n.rep = 10,
        parallel = "multicore", ncpus = 4L))
pramx[-1]
scatter(pramx$DAPC, col = other(platy)$comparePal, cex = 2, legend = TRUE,
        clabel = FALSE, posi.leg = "bottomleft", scree.pca = TRUE,
        posi.pca = "topleft", cleg = 0.75, xax = 1, yax = 2, inset.solid = 1)
