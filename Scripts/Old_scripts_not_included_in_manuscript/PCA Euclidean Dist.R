library(dplyr)
install.packages('ggfortify')
library(ggfortify)


rm(list=ls())

floral08=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/flower data/Nested ANOVA/floral08.csv",header=T)
head(floral08)
dim(floral08)




#AW find the average of the two measurements taken for each plant, note however that there are 4 plants with only 1 measurement.
aggregate(floral08[,c(12:16)],by=list(Outcrop=floral08$OutcropSystem, SampCircle=floral08$SampCircle, Plant=floral08$Plant, FlwPos=floral08$FlwPos),mean,rm.na=TRUE)->avefloral

dim(avefloral)[1]*2

head(avefloral)
tail(avefloral)

#Here AW removes observation 228 for QRL1 sampCircle 1 plant 1T because there is no measurement for SpurLength for this plant
floral<-na.omit(avefloral)
avefloral[!complete.cases(avefloral),]

dim(floral)
dim(avefloral)
head(floral)
tail(floral)
#fix(avefloral)
#fix(floral)


#Here AW pulls out the main columns of interest
floral.sc<-data.frame(floral[,c(1:5)],scale(floral[,c(6:9)]))
floral.sc.slim<-scale(floral[,c(6:9)])
head(floral.sc)
head(floral.sc.slim)

# ok so the easy way to do this is to do a PCA. 
#From Rbloggers:Since skewness and the magnitude of the variables influence the resulting PCs, it is good practice to apply skewness transformation, center and scale the variables prior to the application of PCA.
floral.pca<-prcomp(na.omit(floral[,c(6:9)]), scale=TRUE, center=TRUE)
head(floral.pca) # so this produces PC1, PC2, PC3 and PC4 for all four traits (loading shown)
summary(floral.pca, loadings=TRUE) # So first 2 accounts for 77.9% of variation. Good enough. 
plot(floral.pca) # This shows the scree plot, of how much variance is explained by the 4 PCs. First 2 should be sufficient. (Need to put a number to this)

biplot(floral.pca) # shows how PC 1 and PC2 are loaded relative to the traits. (Red arrows and labels)


#Here MPB checks how the PCA looks with populations coloured.
#http://rpubs.com/sinhrks/plot_pca
autoplot(prcomp(floral.sc.slim))
autoplot(prcomp(floral.sc.slim), data = floral, colour = 'Outcrop',loadings = TRUE,loadings.length=26, loadings.label = TRUE, loadings.label.size = 3,frame = TRUE, frame.type = 'norm')
#the centre of the elipse is the mean(X), mean(Y)



predict(floral.pca)[,1]->floral$pc1 # ok so this is the first PC extracted. 

predict(floral.pca)[,2]->floral$pc2 # ok so this is the second PC extracted. 
levels(floral$Outcrop)
here<-complete.cases(floral[,5:8])
#colr<-c("red","blue") 
pchr<-c(3,4,0,8,2,10,1) 
ss<-levels(floral$Outcrop)
ss
floral.pca$scores[,1]

floral[order(floral$Outcrop),]->floral
floral$SampCircleID<-paste(floral$Outcrop, floral$SampCircle, sep='_')
head(floral)

head(floral)
floral$SampCircleID<-NULL
floral$SampCircleID<-paste(floral$Outcrop,floral$SampCircle,sep="-")
floral$PlantID<-paste(floral$Outcrop,floral$SampCircle,floral$Plant,sep="-")

floral[,c(1:3,10:11)]->floral.pc
head(floral.pc)

# ok so now we need to calculate mean PC1 and PC2 for each sampling circle outcrop combinations. 
aggregate(floral.pc[,c(4,5)],by=list(Outcrop=floral.pc$Outcrop, SampCircle=floral.pc$SampCircle),mean,rm.na=TRUE)->ave.pc
ave.pc
ave.pc$Pop<-paste(ave.pc$Outcrop,ave.pc$SampCircle,sep="_")
ave.pc[,c(5,3,4)]->ave.pc



# ok so now we want to construct euclidean distances among Geographic Distances, Genetic Distances (pairwise Fst) and Euclidean morphological distances. Fortunately, Geographic distances and genetic distances data is readily available. 

read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/Arlequin/2008 IBD fst/Sheet 4-Table 1.csv", header=TRUE)->Dist

head(Dist)
Dist$PopA->Dist$Pop
Dist$PopA<-NULL
merge(Dist,ave.pc)->Dist1
head(Dist1)
Dist1$Pop->Dist1$PopA
Dist1$Pop<-NULL
Dist1$pc1->Dist1$pc1A
Dist1$pc2->Dist1$pc2A
Dist1$pc1<-NULL
Dist1$pc2<-NULL
Dist1$PopB->Dist1$Pop
Dist1$PopB<-NULL

merge(Dist1,ave.pc)->Dist2
head(Dist2)
Dist2$pc1->Dist2$pc1B
Dist2$pc2->Dist2$pc2B
Dist2$pc1<-NULL
Dist2$pc2<-NULL
Dist2$Pop->Dist2$PopB
Dist2$Pop<-NULL
head(Dist2)
Dist2[,c(3,8,1:2,4:7)]->IBD
head(IBD)
dim(IBD)
ave.pc # handcheck passed. 

# now we have all possible combination of PopA and PopB, we need to calculate the euclidean distance. 

IBD$Morph.ED<-sqrt((IBD$pc1A-IBD$pc1B)^2+(IBD$pc2A-IBD$pc2B)^2)
head(IBD)
IBD[,c(1:4,9)]->IBD.final
head(IBD.final)
tail(IBD.final)
IBD[,c(1:2,9)]->Morph.final
head(Morph.final)
#write.csv(Morph.final, file="~/Dropbox/AndyWong/Hierarchical Structure/Arlequin/2008 IBD fst/Morph matrix.csv")

quartz(width=5,height=7,family="Helvetica")
quartz(width=5,height=7,family="Helvetica",type="pdf", file='/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure1_IBD.pdf')
par(mfrow=c(2,1))
par(oma=c(6,8,2.5,1.5))
par(mar=c(0,0,1.7,0)) 
plot(Morph.ED~Distance.m,data=IBD, pch=1, xaxt='n', ylim=c(0,2), xlim=c(0,4000))
plot(Fst~Distance.m,data=IBD, pch=1, ylim=c(0,0.08), xlim=c(0,4000))
mtext("Geographic distance (meters)", side=3, line=-15.3, cex=1.1, adj=0.5)
mtext('Floral PC Euclidean distance', side=4, line=-20, cex=1.1, adj=-60)
mtext('Slatkins linearized Fst', side=4, line=-20, cex=1.1, adj=0.5)
mtext('a',side=3, line=11.5, cex=1.5, adj=0.95)
mtext('b',side=3, line=-2, cex=1.5, adj=0.95)
dev.off()

# ok now the plots basically shows that neither genetic nor morphological distance is correlated with distance, we will do a Mantel test to see if either are significantly correlated. 

# Mantel test - it seems like we can use mantel.test or mantel.rtest
read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/Arlequin/2008 IBD fst/Dist matrix.csv", header=TRUE)->Dist
#dist(Dist)->Dist
Dist
read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/Arlequin/2008 IBD fst/Fst matrix.csv", header=TRUE)->Fst
read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/Arlequin/2008 IBD fst/Morph matrix.csv", header=TRUE)->Morph
Morph
as.matrix(Dist)[1:14,1:14]->Dist.matrix
Dist.matrix
Dist
as.matrix(Fst)[1:14,1:14]->Fst.matrix
Fst.matrix
as.matrix(Morph)[1:14,1:14]->Morph.matrix
Morph.matrix

# ok let's give the Mantel test a go. 
#library(ade4)
#library(ape)
library(vegan)
#vignette("decision-vegan",package='vegan')
#mantel.test(Dist.matrix,Fst.matrix, nperm=10000)
mantel(Dist.matrix,Fst.matrix,method="spear",permutations=10000)->mantel1
str(mantel1)

mantel(Dist.matrix,Morph.matrix,method="spear",permutations=10000)->mantel2

mantel3=mantel(Fst.matrix,Morph.matrix,method="spear",permutations=10000)

mantel1
mantel2
mantel3
names(mantel2)
mantel2[3]

quartz(width=5,height=7,family="Helvetica")
par(mfrow=c(3,1))
par(oma=c(6,8,2.5,1.5))
par(mar=c(0,0,0,0)) 
hist(mantel1$perm, main='', xaxt='n') # for genetic distance
abline(v=mantel1$statistic, lwd=3)
hist(mantel2$perm, main='')
abline(v=mantel2$statistic, lwd=3) # for morphology
hist(mantel3$perm, main='')
abline(v=mantel3$statistic, lwd=3) # for morphology
mtext("Correlation coefficient estimates", side=3, line=-18.8, cex=1.1, adj=0.51)
mtext('Frequency', side=1, line=20, cex=1.5, adj=1.95)
mtext('Frequency', side=4, line=-20, cex=1.5, adj=0.45)
mtext('a',side=9, line=11.5, cex=1.1, adj=0.8)
mtext('b',side=6, line=-2, cex=1.1, adj=0.8)
mtext('c',side=3, line=-2, cex=1.1, adj=0.8)

#MPB plots
library(ggplot2)
require(gridExtra)

ggplot()

dat=as.data.frame(mantel1$perm)
mantel_results1=ggplot(dat, aes(mantel1$perm)) +geom_histogram( binwidth = 0.05,fill="white", colour="black")+
        ggtitle("a")+
        theme(plot.margin=unit(c(0.5,4,0,4), "cm"), 
                plot.title = element_text(hjust=-0.25, vjust=2.12,size=20),
                axis.title.y=element_text(margin=margin(0,20,0,0)),
                text=element_text(size=20),axis.text=element_text(size=20), 
                panel.background=element_blank(),  axis.line = element_line(colour = "black"))+
        geom_vline(aes(xintercept=mantel1$statistic), colour="black", size=1.2)+
        xlim(c(-0.4,0.4))+
        xlab("") + ylab("Frequency")+scale_y_continuous(expand = c(0,0), limits=(c(0,3000)))

dat2=as.data.frame(mantel2$perm)
mantel_results2=ggplot(dat2, aes(mantel2$perm)) +geom_histogram( binwidth = 0.05,fill="white", colour="black")+
        ggtitle("b")+
        theme(plot.margin=unit(c(0.5,4,0,4), "cm"), 
                plot.title = element_text(hjust=-0.25, vjust=2.12,size=20),
                axis.title.y=element_text(margin=margin(0,20,0,0)),
                text=element_text(size=20),axis.text=element_text(size=20), 
                panel.background=element_blank(),  axis.line = element_line(colour = "black"))+
        geom_vline(aes(xintercept=mantel2$statistic), colour="black", size=1.2)+
        xlim(c(-0.4,0.4))+
        xlab("") + ylab("Frequency")+scale_y_continuous(expand = c(0,0), limits=(c(0,3000)))

dat3=as.data.frame(mantel3$perm)
mantel_results3=ggplot(dat3, aes(mantel3$perm)) +geom_histogram( binwidth = 0.05,fill="white", colour="black")+
        ggtitle("c")+
        theme(plot.margin=unit(c(0.5,4,0,4), "cm"), 
                plot.title = element_text(hjust=-0.25, vjust=2.12,size=20),
                axis.title.y=element_text(margin=margin(0,20,0,0)),
                text=element_text(size=20),axis.text=element_text(size=20), 
                panel.background=element_blank(),  axis.line = element_line(colour = "black"))+
        geom_vline(aes(xintercept=mantel3$statistic), colour="black", size=1.2)+
        xlim(c(-0.4,0.4))+
        ylim(c(0,3000))+
        xlab("") + ylab("Frequency") +scale_y_continuous(expand = c(0,0), limits=(c(0,3000)))
        
library(ggplot2)


mantel_test_MPB_jul2016=grid.arrange(mantel_results1,mantel_results2,mantel_results3, bottom=textGrob("Correlation coefficient estimates", gp=gpar(fontsize=22)), nrow=3) 


ggsave("Supp_Figure2_mantel_test_MPB_jul2016.png", plot = mantel_test_MPB_jul2016, scale = 1, width = 10, height = 10, units = c("in"), dpi = 600 )
ggsave("Supp_Figure2_mantel_test_MPB_jul2016.pdf", plot = mantel_test_MPB_jul2016, scale = 1, width = 10, height = 10, units = c("in"), dpi = 600 )

