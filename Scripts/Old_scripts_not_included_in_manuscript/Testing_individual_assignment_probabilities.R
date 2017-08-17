library(dplyr)
library(lme4)
#Using Instruct we found some evidence of structure among the 14 plots. Base on the delta K approach, Instruct identified two clusters. 

# Here we evaluate the evidence of genetic structure among the Outcrops and/or plots within Outcrops?
#First, we ask whether  genetic structure is related to geographic proximity of plots
#Second we assess how well differentiated are plots based on the cluster assignments. 


#1. There is spatial genetic structure. would be revealed if individuals were assigned to clusters that correlated with space (i.e. outcrops and/or plots). 
#Instruct suggests that there is structure at K=2, and the ANOVA of assignment probabilities also suggests that there is structure among pops 

#2. Assignment of individuals to clusters that did not correlate with space, would be evidence for genetic structure independent of space
#(e.g.selection screening out genotypes). 
#a. We test for an association with space using two approaches. First we do a Mantel's test to evaluate 
#whether geogrphically close populations are more likely to be assigned to the same cluster. 
#b. We ask how is the variance in individual assignment probability partitioned among Outcrops and individuals within plots. 

#3. Finally, weak assignment of individuals to any cluster (low or equal assignment probability to all clusters) would suggest a lack of genetic structure, spatial or otherwise. 
#at this geographic scale, the 

rm(list=ls()) 
#This is the matrix of individual assignment probabilities obtained from using Istruct followed by Clumpp to obtain a single matrix for K=2
instructdat_k2=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/acanadensis_K2.indq", header=FALSE, stringsAsFactors = FALSE)
head(instructdat_k2)

#Let's see which pops are driving Fst differences
Fst=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_Fst matrix.csv", header=TRUE)
Fst.matrix=as.matrix(Fst)[1:14,1:14]
colnames(Fst.matrix)=colnames(Dist.matrix)
rownames(Fst.matrix)=colnames(Dist.matrix)        

#PArt1: Begin by exploring these hypothesis for K=2 groups as predicted using Instruct based on delta K values. 
#Here the first test is to fit a hierarchical model and ask is an individual's probability of assignment to the blue (or red) cluster 

#first group is cornflower blue and is in V6
#I pull out the first col with ind name and the last two columns which contain the assignment probability

instructdat_k2_assign=instructdat_k2 %>% select(V1, V6, V7)
colnames(instructdat_k2_assign)=c("ind_name","assign_blue_group", "assign_red_group")
head(instructdat_k2_assign)
nrow(instructdat_k2_assign)

#I use the blue assignment probabilty (arc sine tranform possibly) as a response variable
#I apply a mixed hierarchical model that is the same as the floral hierarchy model

#I merge instruct output data with the microsat data to get individual IDs and outcrop and plot groups
#both files are in the same order
geno08_mpb=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat.csv", header=TRUE, stringsAsFactors = FALSE)
geno08_mpb$row_id=row.names(geno08_mpb)
head(geno08_mpb)


instructdat_k2_assign$row_id=row.names(instructdat_k2_assign)

instructdat_k2_assign_w_plots=full_join(geno08_mpb,instructdat_k2_assign, by="row_id") %>% select(ind, pop,quadrat, plant, assign_blue_group)
head(instructdat_k2_assign_w_plots)
unique(instructdat_k2_assign_w_plots$pop)
unique(instructdat_k2_assign_w_plots$plot)

library(lme4)

#apply a arcsin transformation to the blue assignment probability
#note arcsin transformation is actually arcsin of the sqrt of X
#ASIN(SQRT(A2)) 
instructdat_k2_assign_w_plots$assign_blue_group_arc_sin=asin(sqrt(instructdat_k2_assign_w_plots$assign_blue_group))

instructdat_k2_assign_w_plots$plot=paste(instructdat_k2_assign_w_plots$pop, instructdat_k2_assign_w_plots$quadrat, sep="_")


#Does avg assignemnt probability differ among plots?
boxplot(instructdat_k2_assign_w_plots$assign_blue_group ~ pop:quadrat, data=instructdat_k2_assign_w_plots,las=2)
ggplot(instructdat_k2_assign_w_plots, aes(instructdat_k2_assign_w_plots$plot, instructdat_k2_assign_w_plots$assign_blue_group)) + geom_boxplot()



#PART 1 Does geograhic proximity correlate with cluster assignment? In other words, do sites that are closer geographically
#tend to be assigned to the same cluster?
# I tackle this using a Mantel's test. 

#Can I do a Mantel's test, where the cluster matrix is binary

print(paste("pop", 'blue_assign', "red_assign", "sample_size"))
#Here I make a new variabe called group that defines if the plot is clustering as group "red" or group "blue"

for(i in unique(instructdat_k2_assign_w_plots$plot)){
        #count how many individuals are red or blue
        blue_group_alloc=length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i & instructdat_k2_assign_w_plots$assign_blue_group>0.5])
        red_group_alloc=length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i & instructdat_k2_assign_w_plots$assign_blue_group<0.5])
         if(blue_group_alloc/length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])>0.5){group="blue"} else{group="red"}
        
        instructdat_k2_assign_w_plots$group[instructdat_k2_assign_w_plots$plot==i]=group
}

#Step1. get my distance matrix
#step 2. make a matrix of cluster assignment
head(temp)
temp={}
library(dplyr)
for(i in instructdat_k2_assign_w_plots$pop){
       #first make dataset with pop and cluster
       temp= instructdat_k2_assign_w_plots %>% select(plot, group) %>% group_by(plot,group) %>% summarise(count=n())
}

#pull in geogrpahic distance matrix
Dist.matrix=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_dist_matrix.csv")[2:15]
colnames(Dist.matrix)
rownames(Dist.matrix)=colnames(Dist.matrix)
cluster_matrix=matrix(nrow=14, ncol=14)

for(i in c(1:nrow(temp))){
        for(j in c(1:nrow(temp))){
                if(temp$group[i]==temp$group[j]){cluster_matrix[i,j]=0}else {cluster_matrix[i,j]=1}
        }    
}
colnames(cluster_matrix)=c(colnames(Dist.matrix))
rownames(cluster_matrix)=c(colnames(Dist.matrix))

#Mantel's test 
library(vegan)
set.seed(2)
Mantel_clusters=mantel(Dist.matrix,cluster_matrix,method="spear",permutations=10000)

# To plot the relationship I need to make a rectangular dataframe
#Need to make rectangle dataframe
count=0
n=0
cluster_assign_distance={}
for(coli in colnames(cluster_matrix)){
        n=n+1
        for(rowi in rownames(cluster_matrix)[n:length(rownames(cluster_matrix))]){
                if(coli==rowi){next}
                new_row=cbind(coli,rowi,cluster_matrix[coli,rowi],Dist.matrix[coli,rowi])
                #print(new_row[1,])
                count=count+1
                cluster_assign_distance=rbind(cluster_assign_distance, new_row)
        }
}
cluster_assign_distance=as.data.frame(cluster_assign_distance)

colnames(cluster_assign_distance)=c("site1", "site2", "group_assign", "distance_m")
i="site1"
for(i in colnames(cluster_assign_distance)){
        cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)]=as.character(cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)])
        if(i=="group_assign") {cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)]=as.numeric(cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)])}
        if(i=="distance_m"){cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)]=as.numeric(cluster_assign_distance[,which(colnames(cluster_assign_distance)==i)])}
}


str(cluster_assign_distance)
cluster_assign_distance$group_assign=as.factor(cluster_assign_distance$group_assign)
cluster_assign_distance$distance_m_centered=(cluster_assign_distance$distance_m-mean(cluster_assign_distance$distance_m, na.rm=TRUE))

#If there was an association between cluster assignment and geography we should see that clusters are more likely to 
# to be assigned to the same group (i.e., cluster assignment of 1 here in this cluster matrix). 
# In other words, if geographic proximity was important in cluster assignment this plot would look like a smile. 

library(ggplot2)
ggplot(cluster_assign_distance, aes(x = cluster_assign_distance$distance_m_centered, y = cluster_assign_distance$group_assign))+
        geom_point(data = cluster_assign_distance, aes(x = cluster_assign_distance$distance_m_centered, y = cluster_assign_distance$group_assign)) +
        # geom_smooth(method='lm',formula=y~x,se=FALSE)+
        xlab("") + 
        ylab("Cluster assignment")+
        theme_bw() + scale_x_continuous(limits = c(-2000,2000))+
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




#PArt 1. b. 
#I test if the mean assignment probability differes among Outcrops
str(instructdat_k2_assign_w_plots)
#make some factors
instructdat_k2_assign_w_plots$pop=as.factor(instructdat_k2_assign_w_plots$pop)
instructdat_k2_assign_w_plots$quadrat=as.factor(instructdat_k2_assign_w_plots$quadrat)

#Asks does the mean assignment differs between Outcrops and plots.
lm_quad_nested_in_pop <- lm(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~ pop + pop/quadrat,data=instructdat_k2_assign_w_plots)
anova(lm_quad_nested_in_pop)
test_tukey_lm_quad=TukeyHSD(aov(lm_quad_nested_in_pop))


###HERE

names(test_tukey_lm_quad)
tukey_test_data=as.data.frame(test_tukey_lm_quad$`pop`[,4])
tukey_test_data$comparison=row.names(tukey_test_data)

library(dplyr)
test_tukey_lm_quad=tukey_test_data %>% arrange(tukey_test_data[,1])
colnames(tukey_test_data)= c("Pval","comparison")
length(tukey_test_data$Pval[tukey_test_data$Pval<0.05])
length(tukey_test_data$Pval)
#only 8 out of 21 Outcrop to Outcrop comparisons are differnt

#who is in those comparisons
tukey_test_data[tukey_test_data$Pval<0.05,]
# 4 of these inlcude QLL3, 4 
#Basically it is QBED2 and QLL3 that are different from the other populations. 

#try removing QLL3_2 and QBED2

nrow(instructdat_k2_assign_w_plots)
remove_QLL3_QBED2_data=instructdat_k2_assign_w_plots[which(instructdat_k2_assign_w_plots$pop!="QLL3" & instructdat_k2_assign_w_plots$pop!="QBED2"),]
nrow(remove_QLL3_QBED2_data)

lm_quad_nested_in_pop_QLL3_QBED2_removed <- lm(remove_QLL3_QBED2_data$assign_blue_group_arc_sin ~ pop + pop/quadrat,data=remove_QLL3_QBED2_data)
anova(lm_quad_nested_in_pop_QLL3_QBED2_removed)
test_tukey=TukeyHSD(aov(lm_quad_nested_in_pop_QLL3_QBED2_removed))

#



"QRL2:1 1"
[1] "QLL1:2 1"
[1] "QBED1:1 1"
[1] "QBED1:2 1"
[1] "QLL1:1 1"
[1] "QBED2:1 1"

#So in short only QRL3_2 really has a differnt assignment probability than the rest of the groups. 
nrow(tukey_test_data)
14*13/2


str(instructdat_k2_assign_w_plots)
instructdat_k2_assign_w_plots$quadrat=as.factor(instructdat_k2_assign_w_plots$quadrat)
#get a summary of the % of individuals with assignment probability 
#find dominant group for a plot and ask what % of individuals assign to that group
i="QBED1_1"
for(i in unique(instructdat_k2_assign_w_plots$plot)){
        #count how many individuals are red or blue
        blue_group_alloc=length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i & instructdat_k2_assign_w_plots$assign_blue_group>0.65])
        red_group_alloc=length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i & instructdat_k2_assign_w_plots$assign_blue_group<0.35])
        ambiguous=length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])-blue_group_alloc-red_group_alloc
        print(paste(i, blue_group_alloc,red_group_alloc,ambiguous,length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])))
        
        if(blue_group_alloc/length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])>0.5){group="blue"} else{group="red"}
        
        instructdat_k2_assign_w_plots$group[instructdat_k2_assign_w_plots$plot==i]=group
}


#how many populations 
# I wonder if we should compare this ratio against a binomial distribution. The binomial distribution is our null 
print(paste("pop", 'blue_assign', "red_assign", "sample_size"))
for(i in unique(instructdat_k2_assign_w_plots$plot)){
        #count how many individuals are red or blue
        blue_group_alloc=length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i & instructdat_k2_assign_w_plots$assign_blue_group>0.5])
        red_group_alloc=length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i & instructdat_k2_assign_w_plots$assign_blue_group<0.5])
        print(paste(i, blue_group_alloc,red_group_alloc,length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])))
        print( binom.test(c(blue_group_alloc, red_group_alloc), 20, 0.5))
        if(blue_group_alloc/length(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])>0.5){group="blue"} else{group="red"}
        
        instructdat_k2_assign_w_plots$group[instructdat_k2_assign_w_plots$plot==i]=group
}

#pull out assignments for the 4 plots that are sig diff than 0
different=c("QRL1_2","QLL3_2", "QBED2_1", "QBED1_2")
i="QRL1_2"
getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(paste("plot", "avg_assign", "mode_assign"))
for(i in different){
        instructdat_k2_assign_w_plots[instructdat_k2_assign_w_plots$plot==i,]
        average_assign=mean(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])
        mode_assign=getmode(instructdat_k2_assign_w_plots$assign_blue_group[instructdat_k2_assign_w_plots$plot==i])
        print(paste(i, average_assign,mode_assign))
        }


