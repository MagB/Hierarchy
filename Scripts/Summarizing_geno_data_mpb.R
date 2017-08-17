library(dplyr)
#This script produces the results in Supplementary tables 2 and 3
geno08_mpb=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat.csv", header=TRUE, stringsAsFactors = FALSE)
head(geno08_mpb)

locus=c("100.2.16", "11.20.1", "200.2.4", "25.3.33", "25.6-16", "50-7", "7-27.1","C-01", "D20")
geno08_mpb %>% summarise_each(funs(length))

#Change all "NA" values to real NA
for(i in seq(5, ncol(geno08_mpb))){
        geno08_mpb[,i][geno08_mpb[,i]=="NA"]=NA
}


# 20 individuals were genotypes per plot, but not all individuals have a genotype for all 9 loci.
#This produces a dataframe summarizing the number of samples with missing genotypes by plot and locus.
geno_counts={}
geno08_mpb$SampleID=paste(geno08_mpb$pop, geno08_mpb$quadrat)

for(i in unique(geno08_mpb$SampleID)){
        #pull out data for this plot i
         temp =filter(geno08_mpb,SampleID==i)
         
         colSums(is.na(temp[5:13])==TRUE)
         new_row=(unname(colSums(is.na(temp[5:13])==TRUE)))
         class(i)
         new_row=c(i, as.numeric(new_row), sum(new_row))
         geno_counts=rbind(geno_counts,new_row)
}
colnames(geno_counts)=c("SampleID",colnames(geno08_mpb[,5:13]), "All_missing_genotypes")
row.names(geno_counts)=NULL

#Part 1: Summary by locus
# Number of individuals genotyped per locus
geno_counts={}
for(i in seq(5, ncol(geno08_mpb))){
        #print(colnames(geno08_mpb)[i])
        print(paste(locus[i-4], length(geno08_mpb[!(is.na(geno08_mpb[,i])),i]), sep=" "))
        geno_counts[i-4]=length(geno08_mpb[!(is.na(geno08_mpb[,i])),i])        
}
mean(geno_counts)
sd(geno_counts)

#how many alleles per locus?
new_alleles={}
counts_data={}
counter=0
for(z in seq(5, ncol(geno08_mpb)-1) ){
        new_alleles={}
        new_genotyes={}
        counter=0
        for(i in unique(geno08_mpb[,z])){
                counter=counter+1
                #print(strsplit(i,":"))
                new_alleles[counter]=as.numeric(strsplit(i,":")[[1]][1])
                counter=counter+1
                new_alleles[counter]=as.numeric(strsplit(i,":")[[1]][2])
                
        }   
        locus_name=colnames(geno08_mpb)[z]
        #number of alleles
        allele_count=length(unique(new_alleles[!is.na(new_alleles)]))
        #size range
        size_range=(range(new_alleles[!is.na(new_alleles)])[2]-range(new_alleles[!is.na(new_alleles)])[1])
        new_row=(c(locus_name,allele_count,size_range))
        counts_data=rbind(counts_data, new_row)
}
colnames(counts_data)=c("locus", "num_alleles", "size_range")


#count up heterozygotes observed
print(c("locus", "He"))
for(z in seq(5, (ncol(geno08_mpb)-1) )){
        new_genotyes={}
        counter=0
        #grab column of genotype for 1 locus
       my_locus= geno08_mpb[,z]
       
        allele1= unlist(strsplit(my_locus[!is.na(my_locus)],":"))[seq(1,length(unlist(strsplit(my_locus[!is.na(my_locus)],":"))),2)]
        allele2=unlist(strsplit(my_locus[!is.na(my_locus)],":"))[seq(2,length(unlist(strsplit(my_locus[!is.na(my_locus)],":")))+1,2)]
        
        obs_hets=seq(1,length(allele1))*0
        obs_hets[allele1!=allele2]=1
       print(paste(colnames(geno08_mpb)[z],round(sum(obs_hets)/length(obs_hets),2)))
}

#find He by locus
counter=0
for(z in seq(5, ncol(geno08_mpb)-1) ){
        new_genotyes={}
        counter=counter+1
        #grab column of genotype for 1 locus
        my_locus= geno08_mpb[,z]
        
        allele1= unlist(strsplit(my_locus[!is.na(my_locus)],":"))[seq(1,length(unlist(strsplit(my_locus[!is.na(my_locus)],":"))),2)]
        allele2=unlist(strsplit(my_locus[!is.na(my_locus)],":"))[seq(2,length(unlist(strsplit(my_locus[!is.na(my_locus)],":")))+1,2)]
        genotypes={}
        genotypes=c(allele1,allele2)
        allele_freqs=as.data.frame(table(genotypes))
        allele_freqs$freq_prop2=(allele_freqs$Freq/sum(allele_freqs$Freq))^2
        
        
        He=1-sum(allele_freqs$freq_prop2)
        print(paste(colnames(geno08_mpb)[z],He))
}


#Part2: Summary by plot
#recall that there are 7 populations each split into 2 plots which are ~30m apart
#For each plot, find the number of unique alleles for each locus and average number of alleles per plot

for(population in unique(geno08_mpb$SampleID)){
        #pull out data for one population
        locus_sum=0
        pop=geno08_mpb[geno08_mpb$SampleID==population,]
        counter=0
        unique_alleles=0
        #loop over each locus
        for(z in seq(5, ncol(pop)-1) ){
                new_alleles={}
                #this for loop makes a list of all the unique alleless
                 for(i in unique(pop[,z])){
                        counter=counter+1
                        #print(strsplit(i,":"))
                        new_alleles[counter]=as.numeric(strsplit(i,":")[[1]][1])
                        counter=counter+1
                        new_alleles[counter]=as.numeric(strsplit(i,":")[[1]][2])
                
                 }   
                #prints the population name the locus name, the number of unique alleles
                print(c(population,colnames(pop)[z] ,length(unique(new_alleles[!is.na(new_alleles)]))))
               #unique alleleles
                 unique_alleles=unique_alleles+length(unique(new_alleles[!is.na(new_alleles)]))
           
        }
        avg_alleles=round(unique_alleles/9,2)
       print(c("average alleles per plot", avg_alleles) )
}

#Calculate He per plot
library(stringr)

for(population in unique(geno08_mpb$SampleID)){
        #pull out data for one plot
        pop=geno08_mpb[geno08_mpb$SampleID==population,]
        unique_alleles=0
        p2=0
        p=0
        #loop over each locus
        p_all=0
        for(z in seq(5, ncol(pop)-1) ){
                new_alleles={}
                counter=0
                
                #this for loop makes a list of all the unique alleless
                #it starts by looping over unique genotypes
                #get a list of unique alleles, then use grep to count how many of each alleles there are in total
                for(i in unique(pop[,z])){
                        counter=counter+1
                        #print(strsplit(i,":"))
                        allele1=strsplit(i,":")[[1]][1]
                        new_alleles[counter]=(allele1)

                        allele2=strsplit(i,":")[[1]][2]
                        counter=counter+1
                        new_alleles[counter]=(allele2)
                }   
               
                #total alleles
                total_alleles=length(pop[,z][!is.na(pop[,z])])*2
        
                #loops over each allele
                #for each unique allele find its frequency
                #then 
                sum_p=0
                p2=0
                for(g in unique(new_alleles)[!is.na(unique(new_alleles))]){
                        p= sum(str_count(pop[,z],g),na.rm=T)/total_alleles
                        p2=p2+(p)^2
                }
                #sum up all allele frequency sums for all loci
                p_all=p_all+p2
                
                
        }
        #p_all=1-p_all
        #print(population)
        #print(colnames(pop)[z])
        p_all=p_all/9
        p_all=1-p_all
        print(round(p_all,3))
}

