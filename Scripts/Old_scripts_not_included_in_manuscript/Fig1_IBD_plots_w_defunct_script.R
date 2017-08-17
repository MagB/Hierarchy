library(ggplot2)
library(gridExtra)
library(grid)
require(cowplot)
rm(list = ls())

#Note that loading cowplot: The following object is masked from ‘package:ggplot2’:ggsave
#Heredity requirements: http://www.nature.com/aj/artworkguidelines.pdf
#single column 86mm, double column 178mm
#for publicatoin "we prefer to work with Adobe Illustrator Creative Suite 6 " 
#fof line art, use Illustrtor eps files or pdf
#Images should not exceed 640 x 480 pixels (9 x 6.8 inches at 72 pixels per inch)

#This script makes 2 plots (Fst v Geographic distance and FLoral PCA distance vs geographic distance) 

#This figure accompanies 2 analyses:
#1.Mantel's test
#These plots accompany the Mantel test to test for a correlation between the Fst and geographic distance matrix and the PC distance vs geog. distance matrix.
#The script for Mantel's test is called "PCA_Euclidean_Dist_mpb.R" and is housed in the folder "Scripts"
#2. Regression analysis of the model fit on these plots. This analysis invovles: fitting a linear model to the data, 
#generating 95% bootstrapped CI and using permutation tests to evaluate whether the parameters estimates are statistically significantly different from 0. 



#The script to generate plots is partitioned into 2 parts.
#Section 1. is the IBD plot with FloralPCA vs geographic distance as top panel and Fst vs geographic distance on the bottom panel
#In this plot a simple linear relationship is fit to the data
#Section 2. The same IBD relationships are plotted, but a nonlinear term is fit to the data. Although the nonlinear model best fits the data, there is no
#obvious biological mechanism that could account for this curvature. Thus, we do not include the nonlinear model in the manuscript, but present it here for completeness. 

#The data used to generate these plots was created by merging the pairwise Fst estimates (obtained in Arlequin) to the parwise geographic distance and pairwise PCA distances for the floral traits
#the Hierstruct_IBD_final.csv file was generated in the script called " " 

IBD=read.csv("/Users/Maggie/Dropbox/AndyWong/1.\ Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_1_IBD/Hierstruct_IBD_final.csv", header=T)
head(IBD)
data=IBD
subdata={}

#which pairs have highest Fst
temp=IBD %>% arrange(desc(Fst))
temp2= temp[1:28,]
temp3=temp2 %>% group_by(PopA) %>% summarise(n=n())
temp4=temp2 %>% group_by(PopB) %>% summarise(n=n())
temp3=as.data.frame(temp3)
length(grep("QBED", temp3$PopA))+ length(grep("QBED", temp4$PopB))
length(grep("QLL3", temp3$PopA))+ length(grep("QLL3", temp4$PopB))

#The point of this script is to bootstrap the parameter estimates and Fstatistics of a linear model of Genetic/Trait Distance plotted against Geogrpahic distance
#In other words this tests if the relationship between genetic/morphological distance and geographic distance is linear, or non-linear. 

#The data used in this script is the same which is used to make the IBD plot, with Fst pairwise distance or the Morph distance (calculated as the euclidean distance of PCA1 and PCA2 of floral traits)
#Just plot of Fst vs centered Distance

#Section 1. plot Fst vs Geogrpahic distance and Morphological distance vs Geographic distance. 
#Originally  a lm model was fit to each of the IBD plots. this is no longer part of the manuscript so 
#the lines of code fitting the line to the figure have been commented out

#First center the predictor variable, distance.
IBD$Distance_centered=(IBD$Distance.m-mean(IBD$Distance.m, na.rm=TRUE))


Morph_plot=ggplot(IBD, aes(x = IBD$Distance_centered, y = IBD$Morph.ED))+
        geom_point(data = IBD, aes(x = IBD$Distance_centered, y = IBD$Morph.ED))+
       # geom_smooth(method='lm',formula=y~x,se=FALSE)+
        xlab("") + 
        ylab("Floral PC Euclidean distance")+
        theme_bw() + scale_x_continuous(limits = c(-2000,2000))+
        geom_text(data = data.frame(), aes(-1990, 2, label = "A"))+
        #top, right, bottom, and left margins
        theme(  plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
                axis.text = element_text(size=12),
                axis.title=element_text(colour="black", size="12"),
                axis.title.y=element_text(margin=margin(0,18,0,0)),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())



ylable_title=expression(italic('F'[ST])*'/('*'1-'*italic('F'[ST])*")")

Fst_plot=ggplot(IBD, aes(x = IBD$Distance_centered, y = IBD$Fst))+
        #geom_smooth(method='lm',formula=y~x,se=FALSE)+
        geom_point(data = IBD, aes(x = IBD$Distance_centered, y = IBD$Fst))+
        xlab("Geographic distance (m)") + 
        ylab(ylable_title)+
        theme_bw() +
        scale_x_continuous(limits = c(-2000,2000))+
        geom_text(data = data.frame(), aes(-1990, 0.085, label = "B"))+
        theme(plot.margin= unit(c(0.5,0.5,0.5,0.5), "cm"),
                axis.text = element_text(size=12),
                axis.title=element_text(colour="black", size="12"),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())


grid.arrange(Morph_plot,Fst_plot)
#plot_grid(Morph_plot, Fst_plot, labels=c("a", "b"), ncol = 1, nrow = 2)
plot_grid(Morph_plot, Fst_plot,ncol = 1, nrow = 2)

#Save as EPS
setEPS()
postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_1_IBD/Figure1_IBD_plot.eps",  bg = "transparent", family="Helvetica",width=3.38, height=8,pointsize = 14)
par(mar = c(4, 4, 4, 4)) 
plot_grid(Morph_plot, Fst_plot,ncol = 1, nrow = 2)
dev.off()

#Save as png jpeg and tiff
quartz(width=3.38, height=8, family="Helvetica")
par(mar=c(5,6,4,2)+0.1)

#plot_grid(Morph_plot, Fst_plot, labels=c("a", "b"), ncol = 1, nrow = 2 , hjust = -5, vjust=-3, scale = 0.80)
plot_grid(Morph_plot, Fst_plot,ncol = 1, nrow = 2)
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_1_IBD/Figure1_IBD_plot.tiff", type = "tiff", device = dev.cur(),  dpi = 1000,width=3.38, height=8, pointsize = 14, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_1_IBD/Figure1_IBD_plot.jpg", type = "jpg", device = dev.cur(),  dpi = 1000,width=3.38, height=8, pointsize = 14, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_1_IBD/Figure1_IBD_plot.png", type = "png", device = dev.cur(),dpi = 1000,width=3.38, height=8, pointsize = 14, bg="transparent")

dev.off()



