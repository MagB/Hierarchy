library(dplyr)
library(lme4)
library(car)
library(ggplot2)
library(lme4)
library(vegan)
library(car)
citation("car")
rm(list=ls())

#Using Instruct we found some evidence of structure among the 14 plots. 
#Based on the delta K approach, the optimal K identified by Instruct was 2. 

# Here we evaluate whether the structure identified by Instruct is related to spatial (geographic)
#structure of outcrops or plots 
# We do so by first testing whether plots that are geographically closer are also more likely to be more similar in individual assignment probabilities among individuals within plots. 
#Second we evalute how the variance in assignment probabilities is distributed among outcrops, plots wihtin outcrops and individuals. 

#This script is partitioned into 2 sections. 
#Section 1. Mantel's test . This has 4 parts (plus one "extra")
#Section 1: Part 1 import the Instruct data and generate a pairwise distance matrix based on pairwise plot t-tests.
  #Section 1: Part 2. pull in geogrpahic distance matrix
  #Section 1: Part 3. Perform Mantel's test  
  #Section 1: Part 4. Basic summary stats of assignment probabilites
  #Section 1: "extra" generates a plot of plot to plot cluster assignment distance vs geographic distance

#Section 2. Hierarchicaly partitioning variance in individual assignment probability 
  #Section 2. Part 1: Partition variance components
  #Section 2. Part 2: Test if variance components > 0

#Section 1. Mantel's test
#We test for an association with geography by using a Mantel's test. This is akin to an Isolation by distance (IBD) analysis, 
#but in this case we ask does the pairwise geographic distance between populations correlate to the pairwise distance in cluster assignment probabilities between plots.
#This test requires a matrix of the plot-pairwise geographic distance, as well as a distance matrix of the plot-plot distance in assignment probabilities. 
#the geogrphic distance matrix has already been created in another script and is simply loaded here.
#In Part 1 of this Section we generate the plot-pairwise dissimilarity matrix of assignment probabilities.

#Section 1: Part 1 import the Instruct data and generate a pairwise distance matrix based on pairwise plot t-tests.

#Load Instruct data.
#This is the matrix of individual assignment probabilities obtained from using Istruct followed by Clumpp to obtain a single matrix for K=2
instructdat_k2=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Figure_1_Instruc_w_map/acanadensis_K2.indq", header=FALSE, stringsAsFactors = FALSE)
head(instructdat_k2)
instructdat_k2_assign=instructdat_k2[,c(1,6,7)]
colnames(instructdat_k2_assign)=c("ind_name","cluster1", "cluster2")
head(instructdat_k2_assign)
nrow(instructdat_k2_assign)

#Assign a row_id. This will be used to merge this dataframe with another dataset that contains outcrop, plot and sample information, 
instructdat_k2_assign$row_id=row.names(instructdat_k2_assign)

#Note the instruct output does not show which plots individuals were sampled from. 
#plot_order it the order of the plots in the instruct output file 
plot_order=c("QBED1-1","QBED1-2","QBED2-1","QBED2-2","QLL1-1","QLL1-2","QLL3-1","QLL3-2","QRL1-1","QRL1-2","QRL2-1","QRL2-2","QTUR-1","QTUR-2")


#I merge instruct output data with the microsat data to get individual IDs and outcrop and plot groups
#both files are in the same order. Therefore I make a new column in both files called row_id. row_id is then used to merge the two datasets.

geno08_mpb=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat.csv", header=TRUE, stringsAsFactors = FALSE)
geno08_mpb$row_id=row.names(geno08_mpb)
head(geno08_mpb)

#merge the Instruct data with the outcrop, plot and sample info provided in the geno08_mpb dataset
instructdat_k2_assign_w_plots=full_join(geno08_mpb,instructdat_k2_assign, by="row_id") %>% select(ind, pop,quadrat, plant, cluster1)
head(instructdat_k2_assign_w_plots)

#Make a column in the Instruct dataset identifying plots 
instructdat_k2_assign_w_plots$plot=paste(instructdat_k2_assign_w_plots$pop, instructdat_k2_assign_w_plots$quadrat, sep="_")
unique(instructdat_k2_assign_w_plots$pop)
unique(instructdat_k2_assign_w_plots$plot)

#Add two new columns for transformations of the probability assignment to cluster1. These will be needed in downstream analyses
instructdat_k2_assign_w_plots$cluster1_arc_sin=asin(sqrt(instructdat_k2_assign_w_plots$cluster1)) #arcsin sqroot transformation
instructdat_k2_assign_w_plots$cluster1_logit=logit(instructdat_k2_assign_w_plots$cluster1)#apply a logit transformation to the proportional data


#Make a dissimilarity matrix of difference in individual assignment probabilities between plots
#Here we use a t-statistic as a metric for dissimilarity in assignment probability between two plots

cluster_t_matrix=matrix(0,nrow=14, ncol=14) #generate an empty matrix 
colnames(cluster_t_matrix)=plot_order
rownames(cluster_t_matrix)=plot_order

for(i in unique(instructdat_k2_assign_w_plots$plot)){
        for(j in unique(instructdat_k2_assign_w_plots$plot)){
                y1=instructdat_k2_assign_w_plots$cluster1_logit[instructdat_k2_assign_w_plots$plot==i]
                y2=instructdat_k2_assign_w_plots$cluster1_logit[instructdat_k2_assign_w_plots$plot==j]
                
                # independent 2-group t-test
                paired_plot_t_test=t.test(y1,y2) # where y1 and y2 are numeric vectors of assignment probabilities for each plot
                paired_plot_t_test$statistic
                #propagate matrix with t-statistics
                cluster_t_matrix[i,j]=abs(paired_plot_t_test$statistic)
        }
}  


#Section 1: Part 2. pull in geogrpahic distance matrix
Dist_matrix=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_dist_matrix.csv")[2:15]
colnames(Dist_matrix)=colnames(cluster_t_matrix)
rownames(Dist_matrix)=rownames(cluster_t_matrix)


#Section 1: Part 3. Perform Mantel's test 
set.seed(2)
Mantel_clusters=mantel(Dist_matrix,cluster_t_matrix, method="spearman", permutations=10000)
#Mantel statistic r: 0.02193 
#Significance: 0.38066

#Section 1: Part 4. Basic summary stats of assignment probabilites
#how many individuals have 100% assignment to one or the other cluster? 
#0
instructdat_k2_assign$cluster1[instructdat_k2_assign$cluster1==0]
instructdat_k2_assign$cluster1[instructdat_k2_assign$cluster1==1]

#what % of individuals have assignment probabilities between 0.2 and 0.8? 
#60.4%
length(instructdat_k2_assign$cluster1[instructdat_k2_assign$cluster1>=0.2 & instructdat_k2_assign$cluster1<=0.8])/280


#Section 1: extra This is a plot of the cluster assignment distance and the geographic distance between plot pairs. 
#This is an additional plot provided for visualization of the Mantel's test and is not intended for publication.
# To plot the relationship I need to make a rectangular dataframe
#Need to make rectangle dataframe
count=0
n=0
cluster_assign_distance={}
for(coli in colnames(cluster_t_matrix)){
        n=n+1
        for(rowi in rownames(cluster_t_matrix)[n:length(rownames(cluster_t_matrix))]){
                if(coli==rowi){next}
                new_row=cbind(coli,rowi,cluster_t_matrix[coli,rowi],Dist_matrix[coli,rowi])
                #print(new_row[1,])
                count=count+1
                cluster_assign_distance=rbind(cluster_assign_distance, new_row)
        }
}
cluster_assign_distance=as.data.frame(cluster_assign_distance)

colnames(cluster_assign_distance)=c("site1", "site2", "group_assign", "distance_m")
str(cluster_assign_distance)

#this loop converts the columns to different types of data
for(i in colnames(cluster_assign_distance)){
        cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)]=as.character(cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)])
        if(i=="group_assign") {cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)]=as.numeric(cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)])}
        if(i=="distance_m"){cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)]=as.numeric(cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)])}
}

str(cluster_assign_distance)

cluster_assign_distance$distance_m_centered=(cluster_assign_distance$distance_m-mean(cluster_assign_distance$distance_m, na.rm=TRUE))

#If there was an association between cluster assignment and geography we should see that clusters are more likely to 
# to be assigned to the same group (i.e., cluster assignment of 1 here in this cluster matrix). 
# In other words, if geographic proximity was important in cluster assignment this plot would look like a smile. 

#if using this plot in pub then make sure to use centered distance. 
ggplot(cluster_assign_distance, aes(x = cluster_assign_distance$distance_m, y = cluster_assign_distance$group_assign))+
        geom_point(data = cluster_assign_distance, aes(x = cluster_assign_distance$distance_m, y = cluster_assign_distance$group_assign)) +
        # geom_smooth(method='lm',formula=y~x,se=FALSE)+
        xlab("") + 
        ylab("Cluster assignment")+
        theme_bw() + #scale_x_continuous(limits = c(-2000,2000))+
        #geom_text(data = data.frame(), aes(-1990, 2, label = "A"))+
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


#Section 2. Hierarchical partitioning of variance
#Section 2. Part 1: Partition variance components
#We ask how is the variance in individual assignment probability partitioned among Outcrops and individuals within plots. 

#the logit transformation does a better job at normalizing the residuals. 
assign_prob_lmer<-lmer(instructdat_k2_assign_w_plots$cluster1_logit ~ (1|pop/plot),data=instructdat_k2_assign_w_plots)
#Extract variance components
VarCorr(assign_prob_lmer)

test1=summary(assign_prob_lmer)
names(test1)
test1$coefficients

VarCorr(assign_prob_lmer) #lists the standard deviations
VarCorr(assign_prob_lmer)[1] #this gives varainces
(SampCircle<-as.numeric(VarCorr(assign_prob_lmer)[1]))
(OutcropSystem<-as.numeric(VarCorr(assign_prob_lmer)[2]))
(ResidualVar<-(as.numeric(sigma(assign_prob_lmer)))^2)

vars=c(OutcropSystem,SampCircle,ResidualVar)
pvars_prop<-100*vars/sum(vars)
comps=c("Among outcrops","Between plots within outcrops","Among individuals within plots")

Comps=data.frame(comps,pvars_prop)
Comps

#Section 2. Part 2: Test if variance components > 0
assign_prob_lmer_reml<-lmer(instructdat_k2_assign_w_plots$cluster1_logit ~ (1|pop/quadrat),data=instructdat_k2_assign_w_plots, REML=TRUE)
assign_prob_lmer_no_outcrop<-lmer(instructdat_k2_assign_w_plots$cluster1_logit ~ (1|quadrat),data=instructdat_k2_assign_w_plots, REML=TRUE)
assign_prob_lmer_no_plot<-lmer(instructdat_k2_assign_w_plots$cluster1_logit ~ (1|pop),data=instructdat_k2_assign_w_plots, REML=TRUE)


no_outcrop_pvalue=anova(assign_prob_lmer_reml,assign_prob_lmer_no_outcrop,refit = FALSE)
no_plot_pvalue=anova(assign_prob_lmer_reml,assign_prob_lmer_no_plot,refit = FALSE)

Comps$Pvalue<-c(no_outcrop_pvalue[2,8],no_plot_pvalue[2,8], 'NA') 

