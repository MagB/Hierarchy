#
TESS_output=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/TESS/Run_Jul7/summary_table.txt", header=F, stringsAsFactors = FALSE, skip=1)

colnames(TESS_output)=c("Run_id", "Kmax", "DIC", "model", "model_name")

head(TESS_output)

#Get DIC means across the 10 run
DICmeans2<-tapply(TESS_output$DIC, TESS_output$K, mean)
Ks2=c(seq(1:length(DICmeans2)))
plot(DICmeans2~Ks2)



#Make Bar graphs
# Import the single matrix of assignment probabilities. See notes at begin of script for a more detailed description of this file. 
TESS_K5=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/TESS/Run_Jul7/CLUMPP/TESS_K5/K5.outfile", header=FALSE, stringsAsFactors = FALSE)
head(TESS_K5)
str(TESS_K5)

barplot(t(TESS_K5[2:6]), border = NA, space = 0, xlab = "Individuals", ylab = "Admixture coefficients")


TESS_K6=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/TESS/Run_Jul7/CLUMPP/TESS_K6/K6.outfile", header=FALSE, stringsAsFactors = FALSE)
head(TESS_K6)
str(TESS_K6)

barplot(t(TESS_K6[2:7]), border = NA, space = 0, xlab = "Individuals", ylab = "Admixture coefficients")

TESS_K7=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/TESS/Run_Jul7/CLUMPP/TESS_K7/K7.outfile", header=FALSE, stringsAsFactors = FALSE)

barplot(t(TESS_K7[2:8]), border = NA, space = 0, xlab = "Individuals", ylab = "Admixture coefficients")


TESS_K8=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/TESS/Run_Jul7/CLUMPP/TESS_K8/K8.outfile", header=FALSE, stringsAsFactors = FALSE)

barplot(t(TESS_K8[2:9]), border = NA, space = 0, xlab = "Individuals", ylab = "Admixture coefficients")

TESS_K2=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/TESS/Run_Jul7/CLUMPP/TESS_K2/K2.outfile", header=FALSE, stringsAsFactors = FALSE)
barplot(t(TESS_K2[2:3]), border = NA, space = 0, xlab = "Individuals", ylab = "Admixture coefficients")

#The individual assignment matrix is presented in the order in which outcrops and plots were input into the program Instruct.
pop_order=c("QBED1-1","QBED1-2","QBED2-1","QBED2-2","QLL1-1","QLL1-2","QLL3-1","QLL3-2","QRL1-1","QRL1-2","QRL2-1","QRL2-2","QTUR-1","QTUR-2")

#geographic order
geog_order=c("QBED1-1","QBED1-2","QBED2-1", "QBED2-2", "QTUR-1", "QTUR-2", "QLL3-1", "QLL3-2", "QLL1-1", "QLL1-2", "QRL1-1", "QRL1-2", "QRL2-1", "QRL2-2")

#reverse geographic order so that it matches the map
geog_order2=rev(geog_order)

#To make the barplot a rectangular dataframe with a column that defines cluster is required.
#Here a dataframe called "cluster_1_and_2" is created that is rectangular and will be used to make the bar plots.
#4 variables are required in this dataset.
#1. assignment probability to a cluster (defined by a cluster_id column)
#2. cluster_id (will be either cluster1 or cluster2)
#3. sample (this should be the order of samples presented in the plot and not the sample order of the original instruct output file)
#4. plot (this is the plot id for the 14 plots surveyed i.e., "QRL2-2") 

#First, #reduce the instruct data to only the 2 columns of cluster assignment and rename the columns
TESS_K5_assig_prob=TESS_K5[,2:6]
colnames(TESS_K5_assig_prob)=c("cluster1", "cluster2",  "cluster3", "cluster4", "cluster5")
str(TESS_K5_assig_prob)

#Add on plot names to the data. The individuals in the dataset "instruct_k2_assig_prob" are ordered 
#according to the vector "pop_order". This order was provided in the input file used for Instruct. 


last_num=0
TESS_K5_assig_prob$plot=NA
i="QRL2-2"
for(i in pop_order){
        # print(i)
        #I pull out which samples correspond to the sample
        count=which(pop_order==i)
        num=seq(20,280, by=20)
        TESS_K5_assig_prob$plot[(last_num+1):num[count]]=i
        last_num=num[count]
}

#Now, make 2 dataframes: cluster1_rect and cluster2_rect. These will be binded in a later step and named: "cluster_1_and_2".
#the following loop reorders the samples according to the order desired in the barplot.
#note that the sample identifier in this dataframe reflects the desired order of samples presented in the final instruct barplot and
#is not the order of individuals in the original instruct file. Note that the row names correspond to the original sample order of the instruct file. 


#Generate an empty dataframe called "cluster1_rect"
last_num=0
cluster1_rect=data.frame(cluster_assign=numeric(0), plot=character(0), sample_order=numeric(0))

#Loop through each of the plots in the order defined by the vectors "geog_order2"
#then pull out the corresponding samples from the instruct dataframe.
#give each sample a new sample identifier reflecting the order in which that sample will be drawn in the barplot

for(i in geog_order2){
        #pull out which samples correspond to the current plot
        #these will be called new_rows
        count=which(pop_order==i)
        num=seq(20,280, by=20)
        stop_pos=num[count]
        start_pos=num[count]-20+1
        new_rows=TESS_K5_assig_prob[start_pos:stop_pos,c(1,3)]
        
        #make a vector defining the order that these samples will appear in the instruct bar plot
        sample_count=which(geog_order2==i)
        sample_count_start=num[sample_count]-20+1
        sample_count_stop=num[sample_count]
        sample_order=seq(sample_count_start,sample_count_stop)
        
        #column bind the sample assignment probabilities, plot id, to the sample order 
        new_rows=cbind(new_rows, sample_order)
        #add the new row to the dataframe cluster1_rect
        cluster1_rect=rbind(cluster1_rect, new_rows)
        
}

#Repeat the above process for assignment to cluster 2. 
last_num=0
cluster2_rect=data.frame(cluster_assign=numeric(0), plot=character(0), sample_order=numeric(0))

for(i in geog_order2){
        # print(i)
        #I pull out which samples correspond to the sample
        count=which(pop_order==i)
        num=seq(20,280, by=20)
        stop_pos=num[count]
        start_pos=num[count]-20+1
        new_rows=TESS_K5_assig_prob[start_pos:stop_pos,c(2,3)]
        
        sample_count=which(geog_order2==i)
        sample_count_start=num[sample_count]-20+1
        sample_count_stop=num[sample_count]
        sample_order=seq(sample_count_start,sample_count_stop)
        
        new_rows=cbind(new_rows, sample_order)
        cluster2_rect=rbind(cluster2_rect, new_rows)
        
}

#Repeat the above process for assignment to cluster . 

last_num=0
cluster2_rect=data.frame(cluster_assign=numeric(0), plot=character(0), sample_order=numeric(0))

for(i in geog_order2){
        # print(i)
        #I pull out which samples correspond to the sample
        count=which(pop_order==i)
        num=seq(20,280, by=20)
        stop_pos=num[count]
        start_pos=num[count]-20+1
        new_rows=TESS_K5_assig_prob[start_pos:stop_pos,c(2,3)]
        
        sample_count=which(geog_order2==i)
        sample_count_start=num[sample_count]-20+1
        sample_count_stop=num[sample_count]
        sample_order=seq(sample_count_start,sample_count_stop)
        
        new_rows=cbind(new_rows, sample_order)
        cluster2_rect=rbind(cluster2_rect, new_rows)
        
}

#rename the columns 
colnames(cluster1_rect)=c("assign_p", "plot","sample_order")
colnames(cluster2_rect)=c("assign_p", "plot","sample_order")

#bind the two dataframes together into one dataframe "cluster_1_and_2"
cluster_1_and_2=rbind(cluster1_rect, cluster2_rect)

#add on a column defining the corresponding cluster for the assignment probability 
cluster_1_and_2$cluster[1:280]="cluster1"
cluster_1_and_2$cluster[281:560]="cluster2"

#convert the plot variable into a factor
cluster_1_and_2$plot=as.factor(cluster_1_and_2$plot)
levels(cluster_1_and_2$plot)
str(cluster_1_and_2)

#Order the plot factor according to geographic order
cluster_1_and_2$plot <- factor(cluster_1_and_2$plot, levels = geog_order2)
head(cluster_1_and_2)


cols <- c(cluster1="darkgrey",cluster2="gray98") 
assign_plot=ggplot(TESS_K5_assig_prob, aes(x = sample_order, y = assign_p, fill = cluster)) + geom_bar(stat = "identity") +
        scale_fill_manual(values = cols)  +
        geom_vline(xintercept = seq(20,260, by=20),lwd=1.2)+
        theme_bw() +
        #scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0),breaks=c(0.0, 0.2, 0.4,0.6, 0.8, 1.0))+
        scale_x_discrete(name="plot", limits=c(10,30,50,70,90,110,130,150,170,190,210,230,250,270),labels=c(geog_order2))+
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
setEPS()
postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Figure_1_Instruc_w_map/Figure_1_Instruc_w_map.eps", paper="special", width=7, height=4, family="Helvetica", pointsize = 14)
plot_grid(assign_plot, A_canadensis_7_pop_map,  ncol = 2, nrow = 1)
dev.off()

