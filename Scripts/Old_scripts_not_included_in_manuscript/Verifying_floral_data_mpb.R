library(dplyr)

#The AW data has two plots for QRL (QRL1-1, QRL1-2, QRL2-1, QRL2-2), whereas the Eckert data calls this area QRL-1, QRL-2 QRL-3 and QRL-4.
#I also find for the observations the measurement of SpurLength differs by 10mm between the cge and aw datasets (aw is 10mm bigger):
#row  MeasureID: 
#254  QLL1-2-2G-2: AW changed obs1 to 42 from 32 
#381 QLL3-2-N61-1: AW changed 1st obs to 29 from 20.
#521  QRL1-3-74-1: AW changed 1st obs to 29 from 19
#646  QTUR-2-2I-2: AW changed 2nd obs to 31 from 21.
#647  QTUR-2-2J-1 : AW changed 1st ob to 29 from 20. 

#It's a bit odd that in all cases were second observation was "updated" that it was increased. Even if these were wrong, it affects 5 out of approx 670 observations so whatever error this introduces it is minimal:
#Overall cge data compared to AW data is close enough to proceed.

cge_floral=read.csv("~/Dropbox/AndyWong/1.\ Hierarchical\ Structure/data/flower\ data/Nested\ ANOVA/Old\ R\ codes/CGE\ analysis/cgefloral08.csv", stringsAsFactors = F, header = T) 
str(cge_floral)
aw_floral=read.csv("~/Dropbox/AndyWong/1.\ Hierarchical\ Structure/data/flower\ data/Nested\ ANOVA/floral08.csv", stringsAsFactors = F, header = T)
head(aw_floral)
str(aw_floral)
dim(cge_floral)
dim(aw_floral)

cge_floral=cge_floral %>% arrange(counter1)
aw_floral= aw_floral %>% arrange(counter1)

cge_floral %>% group_by(OutcropSystem, SampCircle) %>% summarise(n=n())

aw_floral %>% group_by(OutcropSystem,SampCircle) %>% summarise(n=n())


temp=full_join(cge_floral,aw_floral, by="counter2")
dim(temp)

#let's check if variables are different
i=6

for(i in seq(1,16)){
        print(i)
        print(temp[c(i,i+17)][temp[,i]!=temp[,i+17],])
}

temp[c(254,381,521,646,647),]
