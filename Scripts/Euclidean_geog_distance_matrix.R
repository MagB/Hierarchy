Distance=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_plot_distances.csv",header=T)
head(Distance)

Distance$SampCircleID=paste(Distance$Outcrop, Distance$SampCircle, sep='_')

#propagate a distance matrix
distance_matrix=matrix(0,ncol=14, nrow=14)
colnames(distance_matrix)=Distance$SampCircleID
rownames(distance_matrix)=Distance$SampCircleID

for(coli in colnames(distance_matrix)){
        #matrix is [row,col]
        for(rowi in rownames(distance_matrix)){
               if(rowi==coli){distance_matrix[rowi,coli]=0}
                else{
                    if(unlist(strsplit(rowi, "_"))[1]==unlist(strsplit(coli, "_"))[1] & unlist(strsplit(coli, "_"))[2]!=unlist(strsplit(rowi, "_"))[2]){distance_matrix[rowi,coli]=30}
                        else{distance_matrix[rowi,coli]=((Distance$UTM_E.m.[Distance$SampCircleID==rowi]-Distance$UTM_E.m.[Distance$SampCircleID==coli])^2+ (Distance$UTM_N.m.[Distance$SampCircleID==rowi] - Distance$UTM_N.m.[Distance$SampCircleID==coli])^2)^0.5}
               
                        distance_matrix[rowi,coli]=round(distance_matrix[rowi,coli],0)
                 }
        }
}

class(distance_matrix)
write.csv(distance_matrix, file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_dist_matrix.csv")


