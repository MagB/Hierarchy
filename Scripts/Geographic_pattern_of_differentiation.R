library(dplyr)
library(ggfortify)


rm(list=ls())
#The goal of this script is to perform Mantel's tests on pairs of 3 distance matrices.
#Two tests search for a correlation between 1. geographic distance and morphological distance, and 2. genetic distance and geographic distance.
#The third test searches for a correlation between genetic and morphological pairwise population distance. 

#This script is divided into 4 Sections. 
#Section 1: All 3 matrices are either generated or loaded into the script
#1. PArt 1. performs PCA to generate matrix of floral morphology distance 
#2. Part2. loads matrix of Linearized Fst distance (created in Arlequin)
#3. Part 3. loads matrix of Euclidean geographic distance (created in a script called "Euclidean_geog_distance_matrix.R)


#Section 1. Find floral morphology distance
#the data used is "Hierstruct_floral_morphology.csv"
floral08=read.csv("/Users/Maggie/Dropbox/AndyWong/1.\ Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_floral_morphology.csv",header=T)
head(floral08)
dim(floral08)


#Step1. For each plant, calculate mean floral trait of the 2 flowers that were measured per plant.

#note that there are 4 plants with only 1 measurement.

avefloral = floral08 %>% select(OutcropSystem, SampCircle,PlantID,Herkogamy.mm ,PistilLength.mm, SepalLength.mm, SpurLength.mm) %>% group_by(PlantID) %>% summarise(OutcropSystem=first(OutcropSystem), SampCircle=first(SampCircle),Herkogamy.mm=mean(Herkogamy.mm,rm.na=TRUE), PistilLength.mm=mean(PistilLength.mm,rm.na=TRUE),SepalLength.mm=mean(SepalLength.mm,rm.na=TRUE),SpurLength.mm=mean(SpurLength.mm,rm.na=TRUE) )
# create new dataset without missing data. one plant QRL1-1-1T  is missing spur length data
floral<-na.omit(avefloral)

#Part 2: PCA of floral traits
#From Rbloggers:Since skewness and the magnitude of the variables influence the resulting PCs, it is good practice to apply skewness transformation, center and scale the variables prior to the application of PCA.
floral.pca<-prcomp(floral[,4:7], scale=TRUE, center=TRUE)
head(floral.pca) # so this produces PC1, PC2, PC3 and PC4 for all four traits (loading shown)
summary(floral.pca) # So first 2 accounts for 77.9% of variation. Good enough. 

#Visualize the PCA with Outcrops coloured
#http://rpubs.com/sinhrks/plot_pca
autoplot(prcomp(floral[,4:7]))
autoplot(prcomp(floral[,4:7]), data = floral, colour = 'OutcropSystem',loadings = TRUE,loadings.length=26, loadings.label = TRUE, loadings.label.size = 3,frame = TRUE, frame.type = 'norm')

floral$pc1=predict(floral.pca)[,1]#this is the first PC extracted. 
floral$pc2=predict(floral.pca)[,2]#this is the second PC extracted. 

floral$SampCircleID<-paste(floral$OutcropSystem, floral$SampCircle, sep='_')

#Part 3, find average of PCA loadings of PC1 and PC2 for each plot (14 plots in total)
floral.pc=floral %>% group_by(SampCircleID) %>% summarise(PC2=mean(pc2), PC1=mean(pc1))

 
Morph.matrix=matrix(0,ncol=14, nrow=14)
colnames(Morph.matrix)=floral.pc$SampCircleID
rownames(Morph.matrix)=floral.pc$SampCircleID
i="QBED1_1"
j="QBED1_1"
for(i in floral.pc$SampCircleID){
        for(j in floral.pc$SampCircleID){
                euclid_dist=sqrt((floral.pc$PC1[floral.pc$SampCircleID==i]-floral.pc$PC1[floral.pc$SampCircleID==j])^2+(floral.pc$PC2[floral.pc$SampCircleID==i]-floral.pc$PC2[floral.pc$SampCircleID==j])^2)
                print(i)
                print(j)
                print(euclid_dist)
                Morph.matrix[i,j]=euclid_dist
        }
}
class(Morph.matrix)


   

#Section 2. Pull in Euclidean Geographic Distances

Dist.matrix=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_dist_matrix.csv")[2:15]

Dist.matrix=as.matrix(Dist.matrix)
rownames(Dist.matrix)=colnames(Dist.matrix)

#Section 3. Fst pairwise distance matrix
#Fst is derived from Arlequin analysis. The Fst output file from Arlequin was renamed as "Hierstruct_Fst matrix.csv"
#AW originally conducted the analysis in Arlequin. MPB redid the analysis.  

Fst=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_Fst matrix.csv", header=TRUE)
Fst.matrix=as.matrix(Fst)[1:14,1:14]
colnames(Fst.matrix)=colnames(Dist.matrix)
rownames(Fst.matrix)=colnames(Dist.matrix)        
        
# Section 4. Mantel test
library(vegan)

Mantel_Fst=mantel(Dist.matrix,Fst.matrix,method="spear",permutations=10000)
Mantel_Morph=mantel(Dist.matrix,Morph.matrix,method="spear",permutations=10000)
Mantel_Fst_Morph=mantel(Fst.matrix,Morph.matrix,method="spear",permutations=10000)


Mantel_Fst
Mantel_Morph
Mantel_Fst_Morph

#Save the permuted correlation coefficients. These saved correlations will be used to make a plot
#of the distribution of permuted r values. This is Supplemental Figure 2
#Note that the test statistic (the estiamted r) is saved as the 10 0001 observation for each dataset

Mantel_Fst_perm_r=as.data.frame(Mantel_Fst$perm)
Mantel_Fst_perm_r=rbind(Mantel_Fst_perm_r,Mantel_Fst$statistic )

colnames(Mantel_Fst_perm_r)="permuted_r"
write.csv(Mantel_Fst_perm_r, "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supplemental_Figure_2_Mantel_test/Mantel_Fst.csv")


#Save the permuted correlation coefficients. These saved correlations will be used to make a plot
#of the distribution of permuted r values. This is Supplemental Figure 2
Mantel_Morph_perm_r=as.data.frame(Mantel_Morph$perm)
Mantel_Morph_perm_r=rbind(Mantel_Morph_perm_r,Mantel_Morph$statistic )

colnames(Mantel_Morph_perm_r)="permuted_r"
write.csv(Mantel_Morph_perm_r, "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supplemental_Figure_2_Mantel_test/Mantel_Morph.csv")

#Save the permuted correlation coefficients. These saved correlatoins will be used to make a plot
#of the distribution of permuted r values. This is Supplemental Figure 2
Mantel_Fst_Morph_perm_r=as.data.frame(Mantel_Fst_Morph$perm)
Mantel_Fst_Morph_perm_r=rbind(Mantel_Fst_Morph_perm_r,Mantel_Fst_Morph$statistic )
Mantel_Fst_Morph_perm_r[10001,]
colnames(Mantel_Fst_Morph_perm_r)="permuted_r"
write.csv(Mantel_Fst_Morph_perm_r, "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supplemental_Figure_2_Mantel_test/Mantel_Fst_Morph.csv")

#Generate a rectangular matrix for each pairwise plot comparisoin and the corresponding Euclidean distance, Fst and Morph Distance. 
#Need to make rectangle dataframe
count=0
n=0
pairwise_distance={}

#merging Dist.matrix, Fst.matrix, Morph.matrix
for(coli in colnames(Dist.matrix)){
        n=n+1
        #coli="QBED1_1"
        #rowi="QBED1_2"
        for(rowi in rownames(Dist.matrix)[n:length(rownames(Dist.matrix))]){
                if(coli==rowi){next}
                new_row=cbind(coli,rowi,Dist.matrix[coli,rowi],Fst.matrix[coli,rowi],Morph.matrix[coli,rowi])
                print(new_row[1,])
                count=count+1
                pairwise_distance=rbind(pairwise_distance, new_row)
        }
}
pairwise_distance=as.data.frame(pairwise_distance)
library(dplyr)
filter(pairwise_distance, coli=="QLL3_2", rowi=="QTUR_2")
head(pairwise_distance)
colnames(pairwise_distance)=c("popA", "popB", "Distance.m",	"Fst",	"Morph.ED")
write.csv(pairwise_distance, "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Figure_2_IBD/Hierstruct_IBD_final.csv")
