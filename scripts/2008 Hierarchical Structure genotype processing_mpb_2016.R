# 2008 genotype processing - for STRUCTURE, InStruct, Arlequin, HierFstat, and GStudio


#Geno08 is the data for the microsatellites amplified and scored by Andy Wong in 2008. The goal here is to characterize hierearchical genetic diversity among 7 outcrops of
#Aquilegia canandensis. At each of the 7 outcrops, 20 plants from 2 circular plots (measuring 10m in radius) were surveyed for phenotpyic and genetic traits. 4 floral traits were measured 
#280 plants in total, 

#progrmas to check out:
#INEST
#popgen
#gene pop
library(dplyr)

#on a single randomly selected flower and each plant was genotypes for 9 microsatellite loci. For fine-scale genotypic diversity an additional locus (100.2-8) was screened. 

#First Andy removes data that is not part of the data for this paper. I found 2 microsat screens that were repeated in the data. I remove them after 
#the section where andy removes data for years other than 2008 and for locus 100.2-8
rm(list=ls())
geno08=read.csv("../../Current R actives/allgenotype2008.csv", header=TRUE, stringsAsFactors = FALSE)
geno08[duplicated(geno08),]

head(geno08)
str(geno08)
summary(geno08)
#location of Andy's original data
#read.csv("~/Desktop/Current R actives/allgenotype2008.csv", header=TRUE)->geno08

#Let's find which observations have duplicates
dups_data= geno08 %>% group_by(RxnID) %>% summarise(n=n())
dups_data[dups_data$n>1,]

duplicated_samps=c(unlist(dups_data[dups_data$n>1,1]))
duplicated_samps[1]

#now let's look at those duplciated samples in the original dataset
geno08[geno08$RxnID %in% duplicated_samps,]


#Looks like andy just entered data for two samples in 4 times. This is weird. 

#Let's get rid of these duplicates:
geno08=geno08[!(rownames(geno08) %in% c(522, 523,524, 526, 527, 528)),]

#here Andy removes all the microsats with value 100.2.8. There are 280 of these that are removed. This marker is only used to explore fine-scale genetic diversity. 
#The reason for exlcuding this locus from the higher scales (like between plot diversity) is not explained by Andy. I would like to know why and including justification for its exlcusion in the manuscript.

#It's possible that this locus 100.2-8 has missing data or is monomorphic.
#nrow(geno08[geno08$microsat=="100.2-8",])

geno08[geno08$microsat!="100.2-8",]->geno08


#Now he makes a dataset where status==0. I guess status==0 indicates failed reactions?


geno08NA<-geno08[geno08$status==0,]

dim(geno08)


#One problem is that there is one row where status =0 but alleles are not NA.
# this is probably a coding error. Check is allele 208 is out of range for this population at this locus.
geno08NA[!(is.na(geno08NA$allele1)),]
length(geno08NA$allele1[is.na(geno08NA$allele1)])

#So are there 257 missing unique reactions or 256? ANdy thinks there are 257 failed reactions. 
dim(geno08NA) #257 missing unique reactions
geno08Good<-geno08[geno08$status!=0,]

#In what Andy calls Good data there is also one row where the status==1 but the allele1 and allele2 are NA. Is this a data entry problem or something else?
geno08Good[is.na(geno08Good$allele1),]

#Here Andy calculates the fraction of reactions that failed
dim(geno08NA)[1]/dim(geno08Good)[1] # this is still pretty high. 
#but the actual fraction should be calculated as:
dim(geno08NA)[1]/dim(geno08)[1] # this is still pretty high. 
#should have 2520 microsats screened but Andy has 2526
9*7*2*20
length(geno08$RxnID)
length(unique(geno08$RxnID))
#Andy has duplicated reactions for some samples!!!!

head(geno08Good)



#I assume this is where Andy replaces the failed reactions. #What doesn't make sense to me is what is the column "Type".
#For the 2008 data, the options are 1, 2 and R. What do those categories mean?
#Also I think this dataset contains data for another project. He only uses a subset here.
replace=read.csv("../../Current R actives/Genotypes.csv", header=TRUE, stringsAsFactors = F)

#read.csv("~/Desktop/Current R actives/Genotypes.csv", header=TRUE)->replace
head(replace)
str(replace)
dim(replace)
class(replace$Year)
levels(replace$Type)
class(replace$status)
replace08<-replace[replace$Year=="2008" & replace$Type!="Seed" & replace$Type!="Seedling" & replace$Type!="V" & replace$Type!="Vegetative" & replace$Type!="Veg" & replace$status>0 & replace$microsat!="100.2-8",]
dim(replace08) #322 replacement reactions that worked. Seems a little low, but considering that we're missing 257 rxns in total, this is pretty good. Should just about do it. 
#why r there more replacements than missing data?

head(replace08)
replace08$Round<-rep(4,length(replace08$pop))
paste(replace08$pop,replace08$plant,replace08$microsat,sep="-")->replace08$RxnID
replace08$rxn<-rep("+1",length(replace08$pop))
head(replace08)
dim(geno08Good)
dim(replace08)
replace08[c(18,2,3,5:8,17),]

#HereAndy just pulls out the columns from the replace dataset that are common to the original dataset. 
replace08<-replace08[,c(18,2:4,5:8,17,12:14,19)]

head(replace08)
dim(replace08)
head(geno08NA)
dim(geno08NA)
geno08NA[,c(1:2,4)]->geno08NA

#Now Andy figures out how many reactions failed a 2nd time.
#He pulls out data where status==0
replace08NA<-replace[replace$Year=="2008" & replace$Type!="Seed" & replace$Type!="Seedling" & replace$Type!="V" & replace$Type!="Vegetative" & replace$Type!="Veg" & replace$status==0 & replace$microsat!="100.2-8",]

# I  DO NOT UNDERSTAND WHAT THE VAR ROUND does?
replace08NA$Round<-rep(5,length(replace08NA$pop))
paste(replace08NA$pop,replace08NA$plant,replace08NA$microsat,sep="-")->replace08NA$RxnID
replace08NA$rxn<-rep("+1",length(replace08NA$pop))
head(replace08NA)

dim(geno08Good)
dim(replace08NA) #only 38 that didn't work out of all that's meant for replacement. That's pretty good. 
#replace08NA[,c(18,2,3,5:8,17)]
replace08NA<-replace08NA[,c(18,2:4,5:8,17,12:14,19)]
head(replace08NA)
dim(replace08NA)

#geno08NA is the data of failed reactions the first time
#replace08 is data for second reaction. These still include some failed reactions. 
dim(replace08)
dim(geno08NA)


# what I need to do is to match the replace08 rxns to the geno08NA rxns and rbind these to the geno08 data. Most importantly is to excise the geno08NA rxns from the geno08 data. 
length(geno08NA$RxnID)
length(replace08$RxnID)

#WHY ARE THERE MORE REPLACEMENT REACTIONS THAT MISSING DATA?
#are there duplicates?
duplicated_reaplcements= replace08 %>% group_by(RxnID) %>% summarise(n=n()) %>% filter(n>1)


dup_replacements=c(unlist(duplicated_reaplcements[1]))
replace08[replace08$RxnID %in% dup_replacements,]

#what is different between the ID'd that need replacing and those Andy said were replacements?
#check that the bins are identical 

length(replace08$RxnID[replace08$RxnID %in% geno08NA$RxnID])
#there's only 240 replacement reactions that match the needed replacements indicated in geno08NA
#How many of the replacement reactions dont't match the list of needed replacements: 82
length(replace08$RxnID[!(replace08$RxnID %in% geno08NA$RxnID)])
322-240
replace08[replace08$RxnID=="QBED1-33-200.2-4",]
replace08[replace08$RxnID=="QBED1-4-D20",]
# I think some of the replacements were added directly to the "allgenotype2008" data. and that's why there are two identical entires

#now check how many of the needed replacemenents are not in the replacement list: SO below ANdy suggests 18 didn't work a second time, but 
#it seems that he just didn't do 18. so not the same thing.
length(geno08NA$RxnID[!(geno08NA$RxnID %in% replace08$RxnID)])

257-18

replace08$RxnID[grep("QBED1.*43",replace08$RxnID)] 
%in% replace08$RxnID

#But there is a QBED1-N43*-D20



match(geno08NA$RxnID,replace08$RxnID)->matching
summary(matching) #only 18 NAs! That's pretty darn good. 

#So basically out of the ones that didn't work still in our geno08 data base, only 18 rxns were missing from the replace08 data base. This doesn't necessarily mean they didn't work. Either a reaction didn't work or it was just missing from the list. Likely didn't work. 

#first find all unique rxnID in geno08 because they should give us the "master list" of all required reactions. 

head(geno08)
dim(geno08) #includes all good and bad rxns. 
unique(geno08$RxnID)->unique.ID
length(unique.ID) # out of 2526 rxns, 2520 are unique. This is great. 
unique(geno08[,c(1:5)])->uniqueRxn

# then now we bind working rxns to this list. First we need a complete list of working rxns. (replace08 and geno08Good)

head(geno08Good)
dim(geno08Good)
geno08Good[,c(1,10:13)]->replace1
head(replace1)
head(replace08)
replace08[,c(1,10:13)]->replace2
head(replace2)
dim(replace2)
rbind(replace1,replace2)->replace.all
head(replace.all)
dim(replace.all)[1]-dim(replace1)[1]-dim(replace2)[1] #excellent. checks out. 

merge(uniqueRxn,replace.all)->geno.replaced
dim(uniqueRxn)
dim(geno.replaced) #an additional 58 lines. Must be RxnID that doesn't exist in the original uniqueRxn data set. 


head(geno.replaced)

unique(geno.replaced)->geno.replaced.unique
dim(geno.replaced)[1]-dim(geno.replaced.unique)[1]
#six additional lines chopped because they were exact line duplicates. 
dim(geno.replaced.unique)[1]-dim(uniqueRxn)[1] #still 52 outstanding rxns. Now let's see how many of these didn't work. 
summary(geno.replaced.unique) # looks like we've got 2 NAs in this dataset. That's pretty good. 

# first export and see what we've got
write.csv(geno.replaced.unique, file="~/Desktop/Current R Actives/2008GenoReplaced.csv")

# NAs are real and are generated by the fact that they were NAs to begin with and remain NAs still after 5 rounds. n=2. 
# there are repeat rows which may account for the extra 52 lines. Need to remove duplicates but can't use the duplicate function because row data maybe different. (repeated rxns with slightly different data) Need to purify using RxnID. (HOW THOUGH?)

geno.replaced.unique->geno08
head(geno08)
geno08[,c(1,2)]->geno08ID
head(geno08ID)
geno08ID[duplicated(geno08ID),]->duplicated
dim(duplicated)
duplicated
# now how do we get rid of these duplicates? If we create a list of non-dups, with a counter variable, then match the counter variable? 

#geno08$counter<-c(1:length(geno08$RxnID))
geno08[,c(1:2)]->geno08ID
#geno08[,-c(8:10)]->geno08ID
head(geno08ID)
geno08ID[duplicated(geno08ID),]->duplicated
dim(duplicated) # so was able to eliminate a few duplicates, but still got 63 duplicates. Now can perhaps hand-eliminate. In contrast to the 70 we had before. So now we can remove 7 dups because of differences in the status and rxn variable. However, there are still 63 dups that have non-identical information in microsat, allele1 and allele2. No diff in quadrat info. Essentially rxns that were unnecessarily repeated. Hand removal is only viable option... unless we can somehow index the data.frame, which is not hard at all! row.names comes to the rescue! 

row.names(duplicated)->to.eliminate
to.eliminate
#fix(geno08)
#geno08$del<-rep(0,length(geno08$RxnID))
match(row.names(geno08), row.names(duplicated))->geno08$todel

head(geno08)

ifelse(geno08$todel!='NA',1,0)->geno08$del
dim(geno08)
head(geno08)
fix(geno08) #good up to this point. The next step selection creates problems, data set essentially contains NAs. 

# let's try write.csv and read.csv in quick succession to see if we can deal with this. If not, we'll just hand-eliminate the ones that have 1. 

#write.csv(geno08, file="~/Desktop/geno08.csv")

#rm(list=ls())

#read.csv("~/Desktop/geno08.csv", header=TRUE)->geno08
#geno08$X<-NULL

geno08new<-geno08[-which(geno08$del==1),]
dim(geno08new) #yay! Now we've completed the data set. Sitting at 2502. 
head(geno08new) #and solved the NA problem!
# now the question is where is that 2 sample coming from? 
fix(geno08new)

geno08new[-which(geno08new$allele1!='NA'),]->geno08NA
dim(geno08NA)#2 out of 2502 rows are incomplete. Thus, we have a success rate of 1-2/2502
geno08NA
geno08new->geno08
head(geno08)
table(geno08$status) #this is really good news. Very few rxns are unsure. 

geno08$RxnID->RxnID
table(duplicated(RxnID))#none are duplicated. Very good. 

# now we start binning. First we split data into multiple data.frames for each marker totallying to 9 markers. 

head(geno08)
class(geno08$microsat)
length(levels(geno08$microsat))
levels(geno08$microsat)

paste(geno08$pop,geno08$plant,geno08$quadrat,sep="-")->geno08$PlantID

geno08[,c(1,12,2:7)]->geno08 # here we trim data to necessary style.
head(geno08)
dim(geno08)

geno08[,c(1:7)]->geno1
geno08[,c(1:6,8)]->geno2

geno1$allele1->geno1$allele
geno1$allele1<-NULL
geno2$allele2->geno2$allele
geno2$allele2<-NULL
geno1$rep<-rep(1,length(geno1$RxnID))
geno2$rep<-rep(2,length(geno1$RxnID))
head(geno1)
head(geno2)

rbind(geno1,geno2)->geno
dim(geno)
head(geno)
levels(geno$microsat)
length(levels(geno$microsat))

table(geno$microsat) # now this distribution is a little weird. I think the standard number should be 550 or 560. Some are missing a few yet without ANY NAs. 

# rounding (binning) happens right here. 

geno[geno08$microsat=="100.2-16",]->G1
geno[geno08$microsat=="11-20.1",]->G2
geno[geno08$microsat=="200.2-4",]->G3
geno[geno08$microsat=="25.3-33",]->G4
geno[geno08$microsat=="25.6-16",]->G5
geno[geno08$microsat=="50-7",]->G6
geno[geno08$microsat=="7-27.1",]->G7
geno[geno08$microsat=="C-01",]->G8
geno[geno08$microsat=="D20",]->G9

library(MASS)

# for G1 100.2-16 (Done)
truehist(G1$allele, h=0.1)
abline(v=seq(from=179, to=241,by=2))
abline(v=seq(from=180, to=240,by=2))

G1$alleleR<-round(G1$allele)
truehist(G1$alleleR, h=0.1)
abline(v=seq(from=179, to=241,by=2)) # it appears G1 is less likely an odd locus. 
abline(v=seq(from=180, to=240,by=2)) # this is it. 

G1[with(G1,order(G1$allele)),]->G1sort

G1sort$counter<-seq(1:(length(G1sort$pop)))

plot(G1sort$allele~G1sort$counter)
abline(h=seq(from=180, to=240,by=2))
G1$alleleB<-rep(1,(length(G1$pop)))
head(G1)
G1$test<-G1$alleleR%%2
hist(G1$test) # majority is even numbers.
ifelse(G1$test==0,G1$alleleR,G1$alleleR-1)->G1$alleleB

class(G1$alleleB)
table(G1$alleleB)

# for G2 11-20.1 (DONE)
truehist(G2$allele, h=0.1)
abline(v=seq(from=94, to=140,by=2)) #this looks more correct. 
abline(v=seq(from=95, to=131,by=2))

G2$alleleR<-round(G2$allele)
truehist(G2$alleleR, h=0.1)
abline(v=seq(from=94, to=140,by=2)) # this reveals 3 clusters below 97, between 105 - 106 and between 110-112. 
abline(v=seq(from=95, to=131,by=2))

G2[with(G2,order(G2$allele)),]->G2sort

G2sort$counter<-seq(1:(length(G2sort$pop)))

plot(G2sort$allele~G2sort$counter, ylim=c(90,135))
abline(h=seq(from=94, to=140,by=2)) # it appears most alleles are evens. 
G2$alleleB<-rep(1,(length(G2$pop)))
G2$test<-G2$alleleR%%2

G2.1<-G2[G2$alleleR==95,]
G2.2<-G2[G2$alleleR==107,]
G2.3<-G2[G2$alleleR==111,]
G2.4<-G2[G2$alleleR==101 | G2$alleleR==117 | G2$alleleR==128 | G2$alleleR==96 | G2$alleleR==106
| G2$alleleR==112 ,]

dim(G2.4)[1]+dim(G2.3)[1]+dim(G2.2)[1]+dim(G2.1)[1]-dim(G2)[1] # checks out

G2.1$alleleB<-ifelse(G2.1$alleleR==95,96,0)
G2.2$alleleB<-ifelse(G2.2$alleleR==107,106,0)
G2.3$alleleB<-ifelse(G2.3$alleleR==111,112,0)
G2.4$alleleB<-ifelse(G2.4$test==0,G2.4$alleleR,G2.4$alleleR)

rbind(G2.1,G2.2)->G2.all
rbind(G2.all,G2.3)->G2.all
rbind(G2.all,G2.4)->G2.all

G2.all->G2
head(G2)
# now checking clusters
class(G2$alleleB)
table(G2$alleleB)
as.factor(G2$alleleB)

# for G3 200.2-4 (DONE)
truehist(G3$allele, h=0.1)
abline(v=seq(from=200, to=280,by=2))
abline(v=seq(from=201, to=281,by=2)) # looks like from below 220 it's odd, between 220 and 240 it's even, then switches back to odd beyond 240. 

G3$alleleR<-round(G3$allele)
truehist(G3$alleleR, h=0.1)
abline(v=seq(from=200, to=280,by=2)) # looks like binning rules will be different for different range
abline(v=seq(from=201, to=281,by=2))

G3[with(G3,order(G3$allele)),]->G3sort

G3sort$counter<-seq(1:(length(G3sort$pop)))

plot(G3sort$allele~G3sort$counter, ylim=c(200,281))
abline(h=seq(from=201, to=281,by=2)) # it appears most alleles are evens. 

G3$alleleB<-rep(1,(length(G3$pop)))

G3$test<-G3$alleleR%%2
hist(G3$test) # majority is odd numbers. But fairly 50-50. 

# but we can subset this and see if our hypothesis is right. 

G3[G3$alleleR<220,]->G3.1
G3[G3$alleleR>=220 & G3$alleleR<=240,]->G3.2
G3[G3$alleleR>240,]->G3.3

hist(G3.1$test) # mostly odd
hist(G3.2$test) # mostly even
hist(G3.3$test) # mostly odd. We were right at the divisions. 

ifelse(G3.1$alleleR==216,217,G3.1$alleleR)->G3.1$alleleB
truehist(G3.1$alleleB, h=0.1)
ifelse(G3.2$test==0,G3.2$alleleR,G3.2$alleleR+1)->G3.2$alleleB
truehist(G3.2$alleleB, h=0.1)
ifelse(G3.3$test==1,G3.3$alleleR,ifelse(G3.3$alleleR<245,G3.3$alleleR-1,G3.3$alleleR+1))->G3.3$alleleB
truehist(G3.3$alleleB, h=0.1)

rbind(G3.1,G3.2)->G3.all
rbind(G3.all,G3.3)->G3.all

G3.all->G3
head(G3)
# now checking clusters
class(G3$alleleB)
table(G3$alleleB)
as.factor(G3$alleleB)

# for G4 25.3-33 (DONE)
# (equal numbers of even and odds. Very weird. Looks like we need to bin arbitrarily and just stick with it)
truehist(G4$allele, h=0.1)
abline(v=seq(from=160, to=230,by=2))
abline(v=seq(from=161, to=231,by=2)) # looks like from below 220 it's odd, between 220 and 240 it's even, then switches back to odd beyond 240. 

G4$alleleR<-round(G4$allele)
truehist(G4$alleleR, h=0.1)
abline(v=seq(from=160, to=230,by=2)) # looks like binning rules will be different for different range
abline(v=seq(from=161, to=231,by=2))

G4[with(G4,order(G4$allele)),]->G4sort

G4sort$counter<-seq(1:(length(G4sort$pop)))

plot(G4sort$allele~G4sort$counter, ylim=c(160,230))
abline(h=seq(from=161, to=231,by=2)) # it appears most alleles are evens. 
G4$alleleB<-rep(1,(length(G4$pop)))

G4$test<-G4$alleleR%%2
hist(G4$test) # quite even. This could be a problem. 50-50. 

ifelse(G4$test==0,G4$alleleR,G4$alleleR+1)->G4$alleleB
truehist(G4$alleleB, h=0.1)

# now checking clusters
class(G4$alleleB)
table(G4$alleleB)
as.factor(G4$alleleB)

# for G5 25.6-16 (DONE)
truehist(G5$allele, h=0.1)
#abline(v=seq(from=160, to=270,by=2))
abline(v=seq(from=161, to=271,by=2), col="red") # Lots of weird alleles, but mostly odd, this is good. 
# looks like we need to bin down. 

G5$alleleR<-round(G5$allele)
truehist(G5$alleleR, h=0.1, col="blue")
#abline(v=seq(from=160, to=270,by=2), col="red")
abline(v=seq(from=161, to=271,by=2), col="red") #wee. This is good, just bin up or down. 

G5$test<-G5$alleleR%%2
hist(G5$test) # Further support odd numbers.

G5[with(G5,order(G5$allele)),]->G5sort

G5sort$counter<-seq(1:(length(G5sort$pop)))

plot(G5sort$allele~G5sort$counter)
abline(h=seq(from=161, to=271,by=2)) 
G5$alleleB<-rep(1,(length(G5$pop)))

ifelse(G5$test==1,G5$alleleR,G5$alleleR-1)->G5$alleleB
truehist(G5$alleleB, h=0.1)

# now checking clusters
class(G5$alleleB)
table(G5$alleleB)
as.factor(G5$alleleB)

# for G6 50-7 (DONE)
dim(G6)
truehist(G6$allele, h=0.1)
#abline(v=seq(from=160, to=270,by=2))
abline(v=seq(from=191, to=261,by=2), col="red") # some very large alleles, but mostly odd, this is good. 
# looks like we need to bin down. 

#Need to split data into bigger than 210 and smaller than 210
G6$alleleR<-round(G6$allele)
G6$test<-G6$alleleR%%2
hist(G6$test) # mostly odds. 

G6$alleleB<-rep(1,(length(G6$pop)))

G6[G6$allele>=212,]->G6.1

G6[G6$allele<212,]->G6.2

truehist(G6.1$allele, h=0.1)
abline(v=seq(from=199, to=261,by=2), col="red") #mostly even

truehist(G6.2$allele, h=0.1)
abline(v=seq(from=199, to=261,by=2), col="red") #mostly odd

ifelse(G6.1$alleleR==252,251,G6.1$alleleR)->G6.1$alleleB
truehist(G6.1$alleleB, h=0.1)
abline(v=seq(from=199, to=261,by=2), col="red")

ifelse(G6$alleleR==252,251,ifelse(G6$allele<195,193,ifelse(G6$allele>202 & G6$allele<204,203,ifelse(G6$allele>204 & G6$allele<206,205,ifelse(G6$allele>198 & G6$allele<200,199,ifelse(G6$allele>200 & G6$allele<202,201, ifelse(G6$allele>206 & G6$allele<210,207,G6$alleleR)))))))->G6$alleleB
dim(G6) 

truehist(G6.2$allele, h=0.1)
abline(v=seq(from=191, to=211,by=2), col="red")
abline(v=seq(from=190, to=210,by=2), col="blue")

truehist(G6$alleleB, h=0.1, xlim=c(191,211))
abline(v=seq(from=191, to=211,by=2), col="red")
abline(v=seq(from=190, to=210,by=2), col="blue")

# now checking clusters
class(G6$alleleB)
table(G6$alleleB)

# for G7 7-27.1 (DONE)
dim(G7)
truehist(G7$allele, h=0.1)
abline(v=seq(from=140, to=150,by=2),col="red")

G7$alleleR<-round(G7$allele)
truehist(G7$alleleR, h=0.1)
abline(v=seq(from=140, to=150,by=2),col="red") # wow this is the simplest one ever. 


G7$test<-G7$alleleR%%2
hist(G7$test) # all even! 

G7$alleleB<-G7$alleleR

# now checking clusters
class(G7$alleleB)
table(G7$alleleB)

# for G8 C-01 (DONE)

dim(G8)
truehist(G8$allele, h=0.1)
abline(v=seq(from=110, to=160,by=2),col="red")

G8$alleleR<-round(G8$allele)

truehist(G8$alleleR, h=0.1)
abline(v=seq(from=110, to=160,by=2),col="red") # ok not so simple. It appears that there are two versions of each allele, spanning between odd numbers. 
G8$test<-G8$alleleR%%2
ifelse(G8$allele<120 & G8$allele>118,118,ifelse(G8$allele<118 & G8$allele>110,116,ifelse(G8$alleleR==120,120,ifelse(G8$allele<124 & G8$allele>120,122,ifelse(G8$allele<126 & G8$allele>124,124,ifelse(G8$allele<130 & G8$allele>126,128,ifelse(G8$allele<134 & G8$allele>130,132,ifelse(G8$allele<136 & G8$allele>134,135,ifelse(G8$allele<140 & G8$allele>136,138,G8$alleleR)))))))))->G8$alleleB
truehist(G8$alleleB, h=0.1)
abline(v=seq(from=110, to=160,by=2),col="red")

# now checking clusters
class(G8$alleleB)
table(G8$alleleB)

# for G9 D20 (DONE)
dim(G9)
truehist(G9$allele, h=0.1)
abline(v=seq(from=160, to=220,by=2),col="red") # looks like it's mostly even, and LOTS to bin down. 
abline(v=seq(from=161, to=221,by=2),col="blue")

G9$alleleR<-round(G9$allele)
truehist(G9$alleleR, h=0.1)
abline(v=seq(from=160, to=220,by=2),col="red") # so can't just average it out and then bit down. Then it'll lead to false assignments. Must bin down with hard deliniations. 

ifelse(G9$allele<181 ,G9$alleleR,ifelse(G9$allele<183 & G9$allele>181,182,ifelse(G9$allele<185 & G9$allele>183,184,ifelse(G9$allele<187 & G9$allele>185,186,ifelse(G9$allele<189 & G9$allele>187,188,ifelse(G9$allele<191 & G9$allele>189,190,ifelse(G9$allele<193.5 & G9$allele>191,192,ifelse(G9$allele<195.6 & G9$allele>193.5,194,ifelse(G9$allele<198 & G9$allele>195.6,196,ifelse(G9$allele<200 & G9$allele>198,198,ifelse(G9$allele<201.5 & G9$allele>200,200,ifelse(G9$allele<203 & G9$allele>201.5,202,ifelse(G9$allele<205 & G9$allele>203,204,ifelse(G9$allele<207 & G9$allele>206,206,ifelse(G9$allele>210,212,G9$alleleR)))))))))))))))->G9$alleleB
truehist(G9$alleleB, h=0.1)
abline(v=seq(from=170, to=220,by=2),col="red")

# now checking clusters
class(G9$alleleB)
table(G9$alleleB)

# once binnig is finished, we reconstitute the dataframe:
head(G9)

G1<-G1[,c(1:10)]
G2<-G2[,c(1:10)]
G3<-G3[,c(1:10)]
G4<-G4[,c(1:10)]
G5<-G5[,c(1:9,11)]
G6<-G6[,c(1:9,11)]
G7<-G7[,c(1:9,11)]
G8<-G8[,c(1:9,11)]

rbind(G1,G2)->genoB
rbind(genoB,G3)->genoB
rbind(genoB,G4)->genoB
rbind(genoB,G5)->genoB
rbind(genoB,G6)->genoB
rbind(genoB,G7)->genoB
rbind(genoB,G8)->genoB
rbind(genoB,G9)->genoB

head(genoB)

tapply(genoB$plant, genoB$pop,length)
aggregate(genoB[,c(4)],by=list(pop=genoB$pop,microsat=genoB$microsat),length)->sample
sample[which(sample$x<80),]->sampleadd
80-sample$x->add
sum(add)/2
aggregate(genoB[,c(6)],by=list(pop=genoB$pop,plant=genoB$plant),length)->sample2
sample2[which(sample2$x<18),]->sample2add

18-sample2$x->add
sum(add)/2

write.csv(sampleadd, file="~/Desktop/sampleadd.csv")
write.csv(sample2add, file="~/Desktop/sample2add.csv")
write.csv(genoB, file="~/Desktop/genoB.csv")
# now need to ADD rows of NA data for 18 missing combinations of plant-microsat to make a completely square data set. DONE. This is done by hand. 

# still need to double check if the data exists in any of my data base. 
# the following originally generated data for Arlequin, but now will generate data for HierFstat.
rm(list=ls())
read.csv("~/Dropbox/AndyWong/Hierarchical Structure/Genotype processing/genoB.csv", header=TRUE)->genoB
head(genoB)
genoB$X<-NULL
dim(genoB)
1-20/5040

tapply(genoB$plant, genoB$pop,length) # now perfectly squared. 
aggregate(genoB[,c(4)],by=list(pop=genoB$pop,microsat=genoB$microsat),length)->sample
sample[which(sample$x<80),]->sampleadd
80-sample$x->add
sum(add)/2
aggregate(genoB[,c(6)],by=list(pop=genoB$pop,plant=genoB$plant),length)->sample2
sample2[which(sample2$x<18),]->sample2add

18-sample2$x->add
sum(add)/2
tail(genoB)

#read.csv("~/Desktop/allgenotype2008.csv", header=TRUE)->geno08
#head(geno08)
#dim(geno08)
#read.csv("~/Desktop/Genotypes.csv", header=TRUE)->replace
#head(replace)
#dim(replace)


#replace[replace$Year=="2008" & replace$pop=="QRL2" & replace$plant=="4S" & replace$microsat=="50-7",]
#geno08[geno08$Year=="2008" & geno08$pop=="QRL2" & geno08$plant=="4S" & geno08$microsat=="50-7",]
# looks like the ones we still have as NAs, which wasn't able to clearly amplify. 

# now we need to: 1) replace NAs with ? 2) replace * with st 3) place data side by side, Pop_Quad_Plant identity code. For alrequin. 

genoB$PlantQuadID<-paste(genoB$pop, genoB$quadrat, genoB$plant, sep="-")
genoC<-genoB[,c(11,3,5,4,6,10,8)]
head(genoC)
tail(genoC)

# first arrange into shape then eliminate unnecessary data. 
#separate by markers

genoC[genoC$microsat=="100.2-16",]->G1
genoC[genoC$microsat=="11-20.1",]->G2
genoC[genoC$microsat=="200.2-4",]->G3
genoC[genoC$microsat=="25.3-33",]->G4
genoC[genoC$microsat=="25.6-16",]->G5
genoC[genoC$microsat=="50-7",]->G6
genoC[genoC$microsat=="7-27.1",]->G7
genoC[genoC$microsat=="C-01",]->G8
genoC[genoC$microsat=="D20",]->G9

head(G1)
G1$alleleB->G1$g1
G1[,c(1:3,8,7)]->G1
head(G2)
G2$alleleB->G2$g2
G2[,c(1:3,8,7)]->G2
head(G3)
G3$alleleB->G3$g3
G3[,c(1:3,8,7)]->G3
head(G4)
G4$alleleB->G4$g4
G4[,c(1:3,8,7)]->G4
head(G5)
G5$alleleB->G5$g5
G5[,c(1:3,8,7)]->G5
head(G6)
G6$alleleB->G6$g6
G6[,c(1:3,8,7)]->G6
head(G7)
G7$alleleB->G7$g7
G7[,c(1:3,8,7)]->G7
head(G8)
G8$alleleB->G8$g8
G8[,c(1:3,8,7)]->G8
head(G9)
G9$alleleB->G9$g9
G9[,c(1:3,8,7)]->G9

merge(G1,G2, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
#merge(G3,G5, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
#merge(genoA,G3, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
merge(genoA,G4, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
#merge(genoA,G5, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
#merge(genoA,G6, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
merge(genoA,G7, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
merge(genoA,G8, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
#merge(genoA,G9, by=c("PlantQuadID","pop","quadrat", "rep"))->genoA
head(genoA)
tail(genoA)
dim(genoA)
#paste(genoA$pop,genoA$quadrat,sep="-")->genoA$pop_quadrat
genoA$pop<-NULL
genoA$quadrat<-NULL
#genoA$rep<-NULL
#genoA$PlantQuadID<-NULL
#as.factor(genoA$pop_quadrat)->genoA$pop_quadrat
#levels(genoA$pop_quadrat)
#ifelse(genoA$pop_quadrat=="QBED1-1",1,ifelse(genoA$pop_quadrat=="QBED1-2",2,ifelse(genoA$pop_quadrat=="QBED2-1",3,ifelse(genoA$pop_quadrat=="QBED2-2",4,ifelse(genoA$pop_quadrat=="QLL1-1",5,ifelse(genoA$pop_quadrat=="QLL1-2",6,ifelse(genoA$pop_quadrat=="QLL3-1",7,ifelse(genoA$pop_quadrat=="QLL3-2",8,ifelse(genoA$pop_quadrat=="QRL1-1",9,ifelse(genoA$pop_quadrat=="QRL1-2",10,ifelse(genoA$pop_quadrat=="QRL2-1",11,ifelse(genoA$pop_quadrat=="QRL2-2",12,ifelse(genoA$pop_quadrat=="QTUR-1",13,ifelse(genoA$pop_quadrat=="QTUR-2",14,genoA$pop_quadrat))))))))))))))->genoA$pop_ID
#genoA<-genoA[,c(1,12,2:10)]
#genoA<-genoA[,c(7,8,1:6)]
write.csv(genoA, file="~/Dropbox/AndyWong/HierFstat2008.csv", col.names=TRUE,row.names=FALSE)
write.table(genoA, file="~/Dropbox/AndyWong/InStruct2008pop.txt", col.names=TRUE,row.names=FALSE)
write.table(genoA, file="~/Dropbox/AndyWong/InStruct2008popquad.txt", col.names=TRUE,row.names=FALSE)
write.table(genoA, file="~/Dropbox/AndyWong/Structure2008popquadTrim.txt", col.names=TRUE,row.names=FALSE)
write.table(genoA, file="~/Dropbox/AndyWong/Structure2008popquadTrimRev.txt", col.names=TRUE,row.names=FALSE)

#fix(genoA)
#Next, replace * with st, NA with ? and then need to eliminate PlantQuadID and rep every other row. 
genoA$rep->genoA$rep2

head(genoA)

ifelse(genoA$rep2==2,"",genoA$PlantQuadID)->genoA$PlantQuadID
ifelse(genoA$rep2==2,"",genoA$rep)->genoA$rep
genoA$rep2<-NULL

write.table(genoA, file="~/Dropbox/AndyWong/Arlequin2008Trim.txt", col.names=TRUE,row.names=FALSE)


#**********************************

read.table("~/Dropbox/AndyWong/InStruct2008pop.txt",header=TRUE)->genoA
head(genoA)
genoA[genoA$PlantQuadID=="QLL3-2-68"]