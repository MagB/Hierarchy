library(dplyr)
library(lme4)

#This is the matrix of individual assignment probabilities obtained from using Istruct followed by Clumpp to obtain a single matrix for K=2
instructdat_k2=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/acanadensis_K2.indq", header=FALSE, stringsAsFactors = FALSE)
head(instructdat_k2)

# There are 3 possible scenarios of genetic diversity within and among the outcrops and plots in this stud

#1. Spatial genetic structure would be revealed if individuals were assigned to clusters that correlated with space (i.e. outcrops and/or plots). 

#2. Assignment of individuals to clusters that did not correlate with space, would be evidence for genetic structure independent of space (e.g. through distance-independent assortative mating). 

#3. Finally, weak assignment of individuals to any cluster (low or equal assignment probability to all clusters) would suggest a lack of genetic structure, spatial or otherwise. 


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

blue_assign_lmer<-lmer(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~ (1|pop/quadrat),data=instructdat_k2_assign_w_plots,REML=TRUE)

summary(blue_assign_lmer)
#Extract variance components
VarCorr(blue_assign_lmer)

test1=summary(blue_assign_lmer)
names(test1)
test1$coefficients

VarCorr(blue_assign_lmer) #lists the standard deviations

VarCorr(blue_assign_lmer)[1] #this gives varainces
(SamplingCircle<-as.numeric(VarCorr(blue_assign_lmer)[1]))
(Outcrop<-as.numeric(VarCorr(blue_assign_lmer)[2]))
(ResidualVar<-(as.numeric(sigma(blue_assign_lmer)))^2)

#Find proportion of variance attributed to each random effect
blue_vars=c(SamplingCircle,Outcrop,ResidualVar)
blue_prop<-100*blue_vars/sum(blue_vars)
comps=c("Among Sampling Circles","Among Outcrops","Residual Variance")

Comps=data.frame(comps,hkpvars_prop)
Comps


#Test if var comps are different from 0
####Section 2: Part 1: Test if Var!=0 using LL test
#test hypothesis that VarComp >0
blue_assign_lmer1<-lmer(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~ (1|pop/quadrat),data=instructdat_k2_assign_w_plots,REML=TRUE)
blue_assign_lmer2<-lmer(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~ (1|quadrat),data=instructdat_k2_assign_w_plots,REML=TRUE)
blue_assign_lmer3<-lmer(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~ (1|pop),data=instructdat_k2_assign_w_plots,REML=TRUE)

OutcropPvalue=anova(blue_assign_lmer1,blue_assign_lmer2,refit = FALSE)
SampCirclePvalue=anova(blue_assign_lmer1,blue_assign_lmer3,refit = FALSE)



#Does avg assignemnt probability differ among plots?
blue_assign_lm<-lmer(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~ pop + (1|quadrat)  ,data=instructdat_k2_assign_w_plots)



library(lmerTest)
anova(blue_assign_lm)
boxplot(instructdat_k2_assign_w_plots$assign_blue_group ~ pop:quadrat, data=instructdat_k2_assign_w_plots,las=2)

res1 <- lm(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~ pop + pop/quadrat,data=instructdat_k2_assign_w_plots)

res2 <- lm(instructdat_k2_assign_w_plots$assign_blue_group_arc_sin ~plot,data=instructdat_k2_assign_w_plots)
anova(res2)
test_tukey=TukeyHSD(aov(res2), "plot")

head(instructdat_k2_assign_w_plots)

anova(res1)
TukeyHSD(aov(res1), "pop")
test_tukey=TukeyHSD(aov(res1), "pop:quadrat")
names(test_tukey)
tukey_test_data=as.data.frame(test_tukey$`pop:quadrat`[,4])
tukey_test_data$comparison=row.names(tukey_test_data)
library(dplyr)
tukey_test_data=tukey_test_data %>% arrange(tukey_test_data[,1])
colnames(tukey_test_data)= c("Pval","comparison")
length(tukey_test_data$Pval[tukey_test_data$Pval<0.05])

#only 13 comparisons are differnt

#who is in those comparisons
tukey_test_data[tukey_test_data$Pval<0.05,]
tukey_test_data[1:10,]
for(i in unique(unlist(strsplit(tukey_test_data$comparison,"-")))){
        lowest_pvals=tukey_test_data[tukey_test_data$Pval<0.0005,]
        comp_list=unlist(strsplit(lowest_pvals$comparison,"-"))
        print(paste(i,length(comp_list[which(comp_list %in% i)])))
}
#so of the 14 populations, QLL3_2 (8), QRL1_2 (4), QBED2_1(3),QRL2:1 2  ,QLL1:2 2 are the most different from the other populations
0.05/91
#or if bonferroni corrected alpha/Ncomparisons
#I noly have 8 compariaons that are differnt
#QLL3_2 is in 6 of those comparisons, QRL1_2 is in 2 and these are in 1
#QRL1:2-QBED2:1 is the only 1 of these 8 that doesnt include QLL3_2



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


