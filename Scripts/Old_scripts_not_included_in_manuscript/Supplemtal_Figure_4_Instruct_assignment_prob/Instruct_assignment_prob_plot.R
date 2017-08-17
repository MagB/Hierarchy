library(dplyr)
library(ggplot2)
library(cowplot)
rm(list=ls())

#The dataset "acanadensis_K11.indq" is the concensus individual assignment matrix assigened by CLUMPP

K11=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/clumpp/CLUMPP_MacOSX.1.1.2/acanadensis_K11/acanadensis_K11.indq", header=F)
colnames(K11)=c("indiv", "indiv_copy", "instruct_num", "donnu", ":", "group1_ass", "group2_ass", "group3_ass", "group4_ass", "group5_ass", "group6_ass", "group7_ass", "group8_ass", "group9_ass", "group10_ass", "group11_ass")
head(K11)

#I'd like to know what the range of assignment probabilities is for each individual.
K11$min=apply(K11[,6:16], 1, min)
K11$max=apply(K11[,6:16], 1, max)
K11$range=K11$max-K11$min

hist(K11$range)

#this counts how many groups are given a 0 assignment probability for each individual
K11$zeros=apply(K11[,6:16],1, function(x) length(x[x==0]))

#
1/11
str(K11)
summary(K11)
head(K11)
hist(as.vector(K11[,6:16]))


join1={}
for(i in (seq(6,16))){
        join1=as.vector(rbind(join1, K11[,i]))
}

hist(join1)
mean(join1)
1/11

summary(join1)
which(join1==280)
length(join1)

#what proportion of individuals have assignment probabilities less than 80%
#Find max assignment for each individual
K11$max_p=apply(K11[,6:16],1, max)
max(K11$max_p)
1/11
hist(K11$max_p)
summary(K11$max_p)
length(K11$max_p[K11$max_p>0.80])
length(K11$max_p[K11$max_p>0.80])/length(K11$max_p)

K11_plot=ggplot(K11, aes(K11$max_p)) + geom_histogram(colour="black",fill='grey')+
        xlab("Maximum assignment probability") + 
        ylab("Frequency")+
        theme_bw() +                
       # geom_text(data = data.frame(), aes(0.35, 35, label = "K=11"))+
        geom_text( aes(0.35, 35, label = "= 11"))+
        geom_text(data = data.frame(), aes(0.1, 47, label = "B"))+
        annotate(geom="text", x=0.32, y=35, label="K", 
                size=4, family="Helvetica", fontface="italic")+
        scale_y_continuous(expand = c(0,0), lim=c(0,50)) +
        scale_x_continuous(lim= c(0.1,0.4),breaks=seq(0,1,0.1)) +
        theme(plot.margin=unit(c(0,0.05,0,0.8), "cm"), 
                axis.text = element_text(size=12),
                axis.title=element_text(colour="black", size="12"),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())



K2=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/clumpp/CLUMPP_MacOSX.1.1.2/acanadensis_K2/acanadensis_K2.indq", header=F)
head(K2)
K2$max_p=apply(K2[,6:7],1, max)

hist(K2$max_p)
summary(K2$max_p)
length(K2$max_p[K2$max_p>0.80])
length(K2$max_p[K2$max_p>0.70])/length(K2$max_p)
K2$plot=0
#average assignment within plot
plots=c("QBED1_1", "QBED1_2", "QBED2_1", "QBED2_2", "QLL1_1", "QLL1_2", "QLL3_1", "QLL3_2", "QRL1_1", "QRL1_2","QRL2_1", "QRL2_2", "QTUR_1", "QTUR_2")
count=1
j=1
for(i in seq(0,length(K2$V1), by=20)[2:15]){
        
        if(i!=j){K2$plot[j:i]=plots[count]}
        count=count+1
        j=i+1
}
table(K2$plot)

for(i in unique(K2$plot)){
        temp=K2[K2$plot==i,]
        print(paste(i, length(temp$max_p[temp$max_p>0.50])/length(temp$max_p)),sep=" ")
}
length(K2$max_p[K2$max_p>0.70])/length(K2$max_p)

K2_plot=ggplot(K2, aes(K2$max_p)) + geom_histogram(colour="black",fill='grey')+
       # xlab("Maximum assignment probability") + 
        xlab("")+
        ylab("Frequency") +
        theme_bw() +
        scale_y_continuous(expand = c(0,0),lim=c(0,20)) +
        scale_x_continuous(breaks=seq(0.5,1,0.1)) +
        geom_text(data = data.frame(), aes(0.5, 19, label = "A"))+
        geom_text( aes(0.9, 15, label = "= 2"))+
        annotate(geom="text", x=0.85, y=15, label="K", 
                size=4, family="Helvetica", fontface="italic")+
        theme(plot.margin=unit(c(0.2,0.05,0,0.8), "cm"), 
                axis.text = element_text(size=12),
                axis.title=element_text(colour="black", size="12"),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())

K2_plot_all_assignments=ggplot(K2, aes(K2$V6)) + geom_histogram(colour="black",fill='grey')+
        # xlab("Maximum assignment probability") + 
        xlab("")+
        ylab("Frequency") +
        theme_bw() +
        scale_y_continuous(expand = c(0,0),lim=c(0,20)) +
        scale_x_continuous(breaks=seq(0.5,1,0.1)) +
        geom_text(data = data.frame(), aes(0.5, 19, label = "A"))+
        geom_text( aes(0.9, 15, label = "= 2"))+
        annotate(geom="text", x=0.85, y=15, label="K", 
                size=4, family="Helvetica", fontface="italic")+
        theme(plot.margin=unit(c(0.2,0.05,0,0.8), "cm"), 
                axis.text = element_text(size=12),
                axis.title=element_text(colour="black", size="12"),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())
plot_grid(K2_plot, K11_plot,  ncol = 1, nrow = 2,scale = 0.9)

#Save plot

quartz(width=3.38, height=5, family="Helvetica")
postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supplemtal_Figure_4_Instruct_assignment_prob/Supp_Figure4_assignment_dist..eps",  horizontal=FALSE, family="Helvetica", pointsize = 12, bg = "transparent")

plot_grid(K2_plot, K11_plot,  ncol = 1, nrow = 2)
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supplemtal_Figure_4_Instruct_assignment_prob/Supp_Figure4_assignment_dist.jpg", type = "jpg", device = dev.cur(),  dpi = 1000,width=3.38, height=5, pointsize = 12, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supplemtal_Figure_4_Instruct_assignment_prob/Supp_Figure4_assignment_dist.png", type = "png", device = dev.cur(),dpi = 1000,width=3.38, height=5, pointsize = 12, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supplemtal_Figure_4_Instruct_assignment_prob/Supp_Figure4_assignment_dist.tiff", type = "tiff", device = dev.cur(),dpi = 1000,width=3.38, height=5, pointsize = 12, bg="transparent")

dev.off()

#option 2 for Supplemental Figure S4 would be 
