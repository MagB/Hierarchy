# install.packages("devtools")
#devtools::install_github("BioShock38/TESS3_encho_sen")
library(tess3r)
library(maps)
library(ggplot2)
library(rworldmap)
library(dplyr)

#https://github.com/bcm-uga/TESS3_encho_sen/blob/master/vignettes/main-vignette.Rmd

#I used tess2.3 to generate spatial coordinates for each individual. 

my_data=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/TESS/Run_Jul7/InStruct2008popquad_w_lon_lat.txt", header=T)
head(my_data)

#Part 1. Prepare data for analysis
#The individual assignment matrix is presented in the order in which outcrops and plots were input into the program Instruct.
pop_order=c("QBED1-1","QBED1-2","QBED2-1","QBED2-2","QLL1-1","QLL1-2","QLL3-1","QLL3-2","QRL1-1","QRL1-2","QRL2-1","QRL2-2","QTUR-1","QTUR-2")

#geographic order
geog_order=c("QBED1-1","QBED1-2","QBED2-1", "QBED2-2", "QTUR-1", "QTUR-2", "QLL3-1", "QLL3-2", "QLL1-1", "QLL1-2", "QRL1-1", "QRL1-2", "QRL2-1", "QRL2-2")

#Add a column of population name labels so I can order the individuals based on geographic order
last_num=0
my_data$plot=NA
i="QRL2-2"
for(i in pop_order){
        # print(i)
        #I pull out which samples correspond to the sample
        count=which(pop_order==i)
        num=seq(40,560, by=40)
        my_data$plot[(last_num+1):num[count]]=i
        last_num=num[count]
}

#arrange my data based on geographic order
my_data=my_data %>% mutate(plot =  factor(plot, levels = geog_order)) %>%
        arrange(plot) 
my_data=my_data[,1:ncol(my_data)-1]
head(my_data)
tail(my_data)

#Part 2. Convert data into tess format and perform TESS
#convert my data into tess format
hier_tess3 <- tess2tess3(my_data, FORMAT = 2, diploid = T, extra.column = 2)
genotype=hier_tess3$X
coordinates=hier_tess3$coord

"From:https://github.com/bcm-uga/TESS3_encho_sen/blob/master/vignettes/main-vignette.Rmd
The best choice for the $K$ value is when the cross-validation curve exhibits a plateau or starts increasing."
struct_hier_tess3 <- tess3(X = hier_tess3$X, coord = hier_tess3$coord,K = 1:20, ploidy = 2, rep=10, verbose=TRUE,method="projected.ls")
str(struct_hier_tess3)

#Here, best K is reached at K=10 where the cross-validation curve hits a plateau.
q.matrix_k10=qmatrix(struct_hier_tess3, K = 10)

#Part 4. Choose best K and make a ggplot of cross-validation score vs K
#cross_val_curve=plot(struct_hier_tess3, pch = 19, col = "blue",xlab = "Number of ancestral populations", ylab = "Cross-validation score", ylim=c(0.19, 0.22))

struct_hier_tess3[[1]]$rmse
new_data={}
for(i in seq(1:20)){
        rmse=struct_hier_tess3[[i]]$rmse
        pop_num=rep(i,20)
        new_rows=cbind( pop_num, rmse)
        new_data=rbind(new_data, new_rows)
}
str(new_data)
head(new_data)
new_data=as.data.frame(new_data)
library(dplyr)

cross_validation_data=new_data %>% group_by(pop_num) %>% summarise(mean=mean(rmse), sd=sd(rmse))
head(cross_validation_data)
dev.off()
Cross_validation_plot=ggplot(cross_validation_data, aes(x =pop_num, y = mean))+ geom_point()+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
        xlab(expression(italic(K))) + 
        ylab("Cross-validation score")+
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


#Part 5. make structure-like plot
qmatrix_for_plot=as.data.frame(q.matrix_k10)
head(qmatrix_for_plot)
qmatrix_for_plot$inds=row.names(qmatrix_for_plot)
qmatrix_for_plot$inds=as.integer(qmatrix_for_plot$inds)
head(qmatrix_for_plot)
library(tidyr)
temp=qmatrix_for_plot %>% gather(cluster,"assign_p" ,1:10) %>% arrange(inds)
head(temp)

cols <- c(cluster1="darkgrey",cluster2="gray98") 
assign_plot=ggplot(temp, aes(x = inds, y = assign_p, fill = cluster)) + geom_bar(stat = "identity") +
        #scale_fill_manual(values = cols)  +
        geom_vline(xintercept = seq(20,260, by=20),lwd=1.2)+
        theme_bw() +
        #scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0),breaks=c(0.0, 0.2, 0.4,0.6, 0.8, 1.0))+
        scale_x_discrete(name="plot", limits=c(10,30,50,70,90,110,130,150,170,190,210,230,250,270),labels=c(geog_order))+
        ylab("Assignment probability")+
        theme(legend.position="none",axis.text = element_text(size=12),
                axis.title.y=element_blank(),
                #axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title=element_text(colour="black", size="12"),
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())+
        #  theme(plot.margin = unit(c(3,3,3,3), "cm"))+
        coord_flip()

#Save as eps
library(ggplot2)
library(gridExtra)
library(grid)
require(cowplot)
setEPS()
postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Figure_1_Instruc_w_map/Figure_1_Instruc_w_map.eps", paper="special", width=7, height=4, family="Helvetica", pointsize = 14)
plot_grid(Cross_validation_plot, assign_plot,  ncol = 2, nrow = 1)
dev.off()


quartz(width=7, height=3.38, family="Helvetica")
plot_grid(Cross_validation_plot, assign_plot,  ncol = 2, nrow = 1)

quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/resubmission_July_2017/Figures/Supplemental_Fig3_TESS3/Supplemental_Fig3.tiff", type = "tiff", device = dev.cur(),  dpi = 1000,width=7, height=4, pointsize = 14, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/resubmission_July_2017/Figures/Supplemental_Fig3_TESS3/Supplemental_Fig3.jpg", type = "jpg", device = dev.cur(),  dpi = 1000,width=7, height=4, pointsize = 14, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/resubmission_July_2017/Figures/Supplemental_Fig3_TESS3/Supplemental_Fig3.png", type = "png", device = dev.cur(),dpi = 1000,width=7, height=4, pointsize = 14, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/resubmission_July_2017/Figures/Supplemental_Fig3_TESS3/Supplemental_Fig3.pdf", type = "pdf", device = dev.cur(),dpi = 1000,width=7, height=4, pointsize = 14, bg="transparent")

dev.off()

#how many individuals ahve a high assignment probability?
qmatrix_for_plot 
qmatrix_for_plot$max=apply(qmatrix_for_plot[,1:10],1,max)
head(qmatrix_for_plot)
hist(qmatrix_for_plot$max)
length(qmatrix_for_plot$max[qmatrix_for_plot$max>0.2 & qmatrix_for_plot$max<0.8])
nrow(qmatrix_for_plot)
275/280

length(qmatrix_for_plot$max[qmatrix_for_plot$max>0.5])
29/280
