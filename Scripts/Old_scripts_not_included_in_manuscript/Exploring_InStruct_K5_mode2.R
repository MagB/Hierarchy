############
####### Identifying K from the output of A. canadensis

#Here I look at optimal K using delta K criterion and a barplot of K=5. 
#The output used ere is from an InStruct run using default parameters except for -v which we selected Mode 2. Mode 2 is an 
#admixture model which infers population structure and selfing rates (selfing rates are estimated based on number of generations since outcrossing)
#This script was provided by Adriana S.


#Instruct_output<-read.csv("/Users/Instruct_outputie/Dropbox/AndyWong/1. Hierarchical Structure/Instruct_outputie_Hierarchy/instruct_files/instruct_summary_stats.csv", header = T)
#Instruct_output<-read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Supplemental_Figure_3_Instruct_K/instruct_summary_stats.csv", header = T)

Instruct_output<-read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/instruct_files/instruct_files_june_2016/summary_instruct_mode2.txt", header = T)

dim(Instruct_output)
head(Instruct_output)
summary(Instruct_output)

str(Instruct_output)



#lets create a vector for each K
Ks<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
##This vector won't change across plots. It is always going to be X axis

###Obtain means across chains

###################### mean across chains
DICmeans<-tapply(Instruct_output$DIC, Instruct_output$K, mean)
DICmeans
plot(DICmeans~Ks, data=Instruct_output)

G.Rmeans<-tapply(Instruct_output$Gelman, Instruct_output$K, mean)
G.Rmeans
plot(G.Rmeans~Ks, data=Instruct_output)

postVar<-tapply(Instruct_output$Posterior_var, Instruct_output$K, mean)
plot(postVar~Ks, data=Instruct_output)

PostMean_means<-tapply(Instruct_output$Posterior_mean, Instruct_output$K, mean)
plot(PostMean_means~Ks, data=Instruct_output)

dev.off()
quartz()
par(mfrow=c(2,2))

plot(DICmeans~Ks, data=Instruct_output)
plot(PostMean_means~Ks, data=Instruct_output)
plot(postVar~Ks, data=Instruct_output)
plot(G.Rmeans~Ks, data=Instruct_output)


############################################ Explore the diff. stats across chains and K values
quartz()
par(mfrow=c(2,2))

plot(DIC~K, data=Instruct_output)
plot(Posterior_mean~K, data=Instruct_output)
plot(Posterior_var~K, data=Instruct_output)
plot(Gelman.Rubin~K, data=Instruct_output)

###Obtain Delta K
###
## From Evann et al 2005: ΔK as the mean of the absolute values of L′′(K)
#averaged over 20 runs divided by the standard deviation of L(K),

maxK<-max(Instruct_output$K)
maxK
Kmeans<-tapply(Instruct_output$Posterior_mean, Instruct_output$K, mean)
Kmeans
Kvars<-tapply(Instruct_output$Posterior_mean, Instruct_output$K, var)
Kvars
class(Kmeans)
Ksds<-Kvars^0.5
Ksds
deltaK<-rep(NA,maxK)
for(x in 2:maxK){deltaK[x] = abs(Kmeans[x+1] - 2*Kmeans[x] + Kmeans[x-1])/Ksds[x]}
quartz()
plot(deltaK , type='b', xlab='K', cex.axis=0.75)
dev.off()
#prettier plot
#plot(deltaK, type="n", xaxt="n", yaxt="n", ylab="", xlab="", xlim=c(1,15), ylim=c(0,40), main="A.canadensis")
plot(deltaK, type="n", xaxt="n", yaxt="n", ylab="", xlab="", xlim=c(1,15), ylim=c(0,40))
axis(1,at=seq(1,15,1),labels=seq(1,15,1), cex.axis=1.05, mgp=c(5,0.7,0))
mtext(side=1, cex=1.05, line=3, "K (number of clusters)")
axis(2, at=seq(0,40, 10), labels=seq(0,40, 10), cex.axis=1, mgp=c(5,0.7,0), las=1)
mtext(side=2, cex=1.05, line=2," ΔK")
#points(deltaK, type="b", pch=1, cex=1.25)
lines(deltaK~Ks, type="b", lty=5, pch=c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21), bg=c("red","red","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray"), cex=1)


#PLOT DIC and Delta K together on same plot
plot(DIC~K, data=Instruct_output)
library(ggplot2)
library(gridExtra)
library(grid)
require(cowplot)

DIC_plot=ggplot(Instruct_output, aes(x = K, y = DIC))+
        geom_point(data = Instruct_output, aes(x = K, y = DIC))+
        xlab("")+
        # xlab(expression(italic(K))) + 
        ylab("DIC")+
        theme_bw() +
        scale_y_continuous(breaks = c(seq(14000,16000, by=1000)))+
        scale_x_continuous(limits = c(1,14),breaks = seq(2,14,2))+
        theme(plot.margin=unit(c(0.5,0.05,0,0), "cm"), 
                axis.text = element_text(size=12),
                axis.title=element_text(colour="black", size="12"),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())


DeltaK_dat=as.data.frame(cbind(Ks, deltaK))
require("scales")
ylabel_text=expression("Δ"*italic("K"))
DeltaK_plot=ggplot(DeltaK_dat, aes(x =Ks, y = deltaK))+
        geom_point(data = DeltaK_dat, aes(x = Ks, y = deltaK))+
        xlab(expression(italic(K))) + 
        ylab(ylabel_text)+
        theme_bw() +
        scale_x_continuous(limits = c(1,14),breaks = seq(2,14,2))+
        theme(plot.margin=unit(c(0.5,0.05,0,0.8), "cm"), 
                axis.text = element_text(size=12),
                axis.title=element_text(colour="black", size="12"),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())
plot_grid(DIC_plot, DeltaK_plot, labels=c("A", "B"), ncol = 1,  nrow = 2)

quartz(width=3.38, height=6, family="Helvetica")
postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Supplemental_Figure_3_Instruct_K/Supp_Figure3_Delta_K_DIC.eps",  bg = "transparent", family="Helvetica",width=3.38, height=6)

plot_grid(DIC_plot, DeltaK_plot, labels=c("A", "B"), ncol = 1,  nrow = 2)
grid.draw
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Supplemental_Figure_3_Instruct_K/Supp_Figure3_Delta_K_DIC.tiff", type="tiff", dpi=1000)
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Supplemental_Figure_3_Instruct_K/Supp_Figure3_Delta_K_DIC.jpg", type="jpg", dpi=1000)
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Supplemental_Figure_3_Instruct_K/Supp_Figure3_Delta_K_DIC.png", type="png", dpi=1000)

dev.off()

#
instructdat_k5_mode2=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/clumpp/CLUMPP_MacOSX.1.1.2/acanadensis_K5_mode2/acanadensis_k5.indfile", header=FALSE, stringsAsFactors = FALSE)
head(instructdat_k5_mode2)
my_colors=rainbow(5)
barplot(t(as.matrix(instructdat_k5_mode2[,6:10])), beside=FALSE, border=NA, space=0,col=my_colors,axes=FALSE, xaxs="i",xaxt='n')
for(i in seq(20,260, 20)){
        abline(v=i, lwd=2)
}
