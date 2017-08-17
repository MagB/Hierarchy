library(dplyr)
library(nlme)
library(lme4)

#This script is the analysis which partitions phenotypic variation in 4 floral traits in a hierarchical manner.
#Results are presented in 
#Table 2. Partitioning of variation in floral morphology and allele frequencies at nine microsatellite loci among hierarchical samples of Aquilegia canadensis. 
#Section 3 of this script (test for fixed effect of floral age and position) the results of those analyses are presented in Supplemental Table S4.

#Nested analysis of AW & Stephen's flower data using Lme
#this script was written by CGE, updated by AW and verified and documented by MPB,
#MPB also updated the mixed models to lmer rather than lme

# the objective of this analysis is to 
#1) Estimate Variance Components for the three spatial categories where the plants were sampled and 
#2) Test whether the individual variance components are statistically significantly different from 0. 
#3) Test for fixed effect of floral age and position

#MPB separates the objective of this script into 3 sections labelled:
#Section 1: estimation of the variance components
        #this section is organized by each floral trait
#Section 2:  Test the significance of the individual variance components by removing the various nested spatial variables sequentially.
        # This section is orgaized by trait. REML is used to estimate var comps
        # MPB uses two tests (Log likelihood test and confirm using bootstrap) to evaluate whether Var comps are  != 0. 
#Section 3. Test the fixed effect of NDehAnth (number of dehisced anthers, proxy for floral age) and FlowerID (floral position on stem).
        #here ML is used to estimate fixed effects. 

rm(list=ls())
floral08<-read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_floral_morphology.csv",header=T)

head(floral08)
#lmer models

#Section 1:
#In this section, we estimate the variance components
#this section is organized by each floral trait

#Herkogamy
hkcmod_lmer<-lmer(floral08$Herkogamy.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08)

#Extract variance components
VarCorr(hkcmod_lmer)

test1=summary(hkcmod_lmer)
names(test1)
test1$coefficients

VarCorr(hkcmod_lmer) #lists the standard deviations
VarCorr(hkcmod_lmer)[1] #this gives varainces
(Flower<-as.numeric(VarCorr(hkcmod_lmer)[1]))
(SampCircle<-as.numeric(VarCorr(hkcmod_lmer)[2]))
(OutcropSystem<-as.numeric(VarCorr(hkcmod_lmer)[3]))
(ResidualVar<-(as.numeric(sigma(hkcmod_lmer)))^2)

#Find proportion of variance attributed to each random effect
hkvars=c(OutcropSystem,SampCircle,Flower,ResidualVar)
hkpvars_prop<-100*hkvars/sum(hkvars)
comps=c("Among outcrops","Between plots within outcrops","Among flowers within plots","Measurement error")

Comps=data.frame(comps,hkpvars_prop)
Comps

#Sepal Length


selcmod=lmer(floral08$SepalLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08)

#Extract variance components
VarCorr(selcmod)

test1=summary(selcmod)
test1$coefficients

VarCorr(selcmod) #lists the standard deviations
VarCorr(selcmod)[1] #this gives varainces
(Flower<-as.numeric(VarCorr(selcmod)[1]))
(SampCircle<-as.numeric(VarCorr(selcmod)[2]))
(OutcropSystem<-as.numeric(VarCorr(selcmod)[3]))
(ResidualVar<-(as.numeric(sigma(selcmod)))^2)

#Find proportion of variance attributed to each random effect
selcvars=c(OutcropSystem,SampCircle,Flower,ResidualVar)
selcvars_prop<-100*selcvars/sum(selcvars)

Comps$selcvars_prop<-selcvars_prop
Comps



#Spur length

splcmod = lmer(floral08$SpurLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08)

#Extract variance components

VarCorr(splcmod) #lists the standard deviations
VarCorr(splcmod)[1] #this gives varainces
(Flower<-as.numeric(VarCorr(splcmod)[1]))
(SampCircle<-as.numeric(VarCorr(splcmod)[2]))
(OutcropSystem<-as.numeric(VarCorr(splcmod)[3]))
(ResidualVar<-(as.numeric(sigma(splcmod)))^2)

#Find proportion of variance attributed to each random effect
splcvars=c(OutcropSystem,SampCircle,Flower,ResidualVar)
splcvars_prop<-100*splcvars/sum(splcvars)

Comps$splcvars_prop=splcvars_prop
Comps

#Pistil Length


plcmod<-lmer(floral08$PistilLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08)

#Extract variance components
VarCorr(plcmod)

test1=summary(plcmod)
names(test1)
test1$coefficients

VarCorr(plcmod) #lists the standard deviations
VarCorr(plcmod)[1] #this gives varainces
(Flower<-as.numeric(VarCorr(plcmod)[1]))
(SampCircle<-as.numeric(VarCorr(plcmod)[2]))
(OutcropSystem<-as.numeric(VarCorr(plcmod)[3]))
(ResidualVar<-(as.numeric(sigma(plcmod)))^2)

#Find proportion of variance attributed to each random effect
plcvars=c(OutcropSystem,SampCircle,Flower,ResidualVar)
plcvars_prop<-100*plcvars/sum(plcvars)

Comps$plcvars_prop=plcvars_prop
Comps



#Section 2:
# Test the significance of the individual variance components by removing the various nested spatial variables sequentially.
# This section is orgaized by trait. Each section for each trait is subpartitioned into 2 tests.
# I also use two tests. The first is a Log Likelihood test, where I compare the difference in LL between the full and reduced model
#this difference is then compared against a Chi-square distribution with 1 df to evalaute whether the variance component is statistically significantly different from 0.

####Herkogamy: Section 2: Part 1: Test if Var!=0 using LL test
#test hypothesis that VarComp >0
hkcmod1<-lmer(floral08$Herkogamy.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08, REML=TRUE)
hkcmod2<-lmer(floral08$Herkogamy.mm ~ floral08$NDehAnth+(1|SampCircleID/FlowerID),data=floral08, REML=TRUE)
hkcmod3<-lmer(floral08$Herkogamy.mm ~ floral08$NDehAnth+(1|OutcropSystem/FlowerID),data=floral08, REML=TRUE)
hkcmod4<-lmer(floral08$Herkogamy.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID),data=floral08, REML=TRUE)


hkOutcropPvalue=anova(hkcmod1,hkcmod2,refit = FALSE)
hkSampCirclePvalue=anova(hkcmod1,hkcmod3,refit = FALSE)
hkFlowerIDPvalue=anova(hkcmod1,hkcmod4,refit = FALSE)

Comps$hkPvalue<-c(hkOutcropPvalue[2,8],hkSampCirclePvalue[2,8],hkFlowerIDPvalue[2,8], 'NA') 

#Section 2: Part 2: Test if Var!=0 using parametric bootstrap
#Although there is nothing to indicate that using a simple LL test is inappropriate in this case, I chose to compare the 
#results of the LL test to a parametric bootstrap.


#Test OutcropSystem
#I put the parametric bootstrap in a function to simplify code for remaining traits below.
parametric_bootstrap_OutcropSystem=function(my_data,full_model=hkcmod1, reduced_model=hkcmod2){
        (lrstatobs<-as.numeric(2*(logLik(full_model)-logLik(reduced_model))))
        nreps<-1000
        lrstat<-numeric(nreps)
        head(data)
        for (i in 1:nreps) {
                y<-unlist(simulate(reduced_model))
                rmod<-lmer(y~1 +NDehAnth+ (1|OutcropSystem/SampCircleID/FlowerID), data=my_data, REML=TRUE,na.action=na.omit)
                bmod<-lmer(y~1 + NDehAnth+(1|SampCircleID/FlowerID), data=my_data, REML=TRUE,na.action=na.omit)
                lrstat[i]<-as.numeric(2*(logLik(rmod)-logLik(bmod)))
        }
        hist(lrstat, breaks=50)
        mean(lrstat>lrstatobs)  #There is quite a large difference in p-val from using a straight-up LL test (find difference in LL between full and reduced model and test against Chi Square distribution wtih 1 df)
        return( mean(lrstat>lrstatobs) )
}
para_hkc_outcrop=parametric_bootstrap_OutcropSystem(floral08, hkcmod1, hkcmod2)



#SamplCircle test
parametric_bootstrap_SampCircleID=function(mydata,full_model, reduced_model){
        (lrstatobs<-as.numeric(2*(logLik(full_model)-logLik(reduced_model))))
        nreps<-1000
        lrstat<-numeric(nreps)
        head(data)
        for (i in 1:nreps) {
                y<-unlist(simulate(reduced_model))
                rmod<-lmer(y~1 +NDehAnth+ (1|OutcropSystem/SampCircleID/FlowerID), data=mydata, REML=TRUE)
                bmod<-lmer(y~1 + NDehAnth+(1|OutcropSystem/FlowerID), data=mydata, REML=TRUE)
                lrstat[i]<-as.numeric(2*(logLik(rmod)-logLik(bmod)))
        }
        hist(lrstat, breaks=50)
        mean(lrstat>lrstatobs)  #There is quite a large difference in p-val from using a straight-up LL test (find difference in LL between full and reduced model and test against Chi Square distribution wtih 1 df)
        return( mean(lrstat>lrstatobs) )
}
para_hkc_samp=parametric_bootstrap_SampCircleID(floral08, hkcmod1, hkcmod3)


#FlowerID test

parametric_bootstrap_FlowerID=function(mydata,full_model, reduced_model){
        (lrstatobs<-as.numeric(2*(logLik(full_model)-logLik(reduced_model))))
        nreps<-1000
        lrstat<-numeric(nreps)
        head(data)
        for (i in 1:nreps) {
                y<-unlist(simulate(reduced_model))
                rmod<-lmer(y~1 +NDehAnth+ (1|OutcropSystem/SampCircleID/FlowerID), data=mydata, REML=TRUE)
                bmod<-lmer(y~1 +NDehAnth+(1|OutcropSystem/SampCircleID), data=mydata, REML=TRUE)
                lrstat[i]<-as.numeric(2*(logLik(rmod)-logLik(bmod)))
        }
        hist(lrstat, breaks=50)
        mean(lrstat>lrstatobs)  #There is quite a large difference in p-val from using a straight-up LL test (find difference in LL between full and reduced model and test against Chi Square distribution wtih 1 df)
        return( mean(lrstat>lrstatobs) )
}
para_hkc_flr=parametric_bootstrap_FlowerID(floral08, hkcmod1, hkcmod4)




####Pistil length: Section 2: Part 1: Test if Var!=0 using LL test
#test hypothesis that VarComp >0
plcmod<-lmer(PistilLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08, REML=TRUE)
plcmod2<-lmer(PistilLength.mm ~ floral08$NDehAnth+(1|SampCircleID/FlowerID),data=floral08, REML=TRUE)
plcmod3<-lmer(PistilLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/FlowerID),data=floral08, REML=TRUE)
plcmod4<-lmer(PistilLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID),data=floral08, REML=TRUE)

plOutcropPvalue=anova(plcmod,plcmod2, refit=FALSE)
plSampCirclePvalue=anova(plcmod,plcmod3,refit=FALSE)
plFlowerIDPvalue=anova(plcmod,plcmod4,refit=FALSE)

Comps$plPvalue<-c(plOutcropPvalue$'p-value'[2],plSampCirclePvalue$'p-value'[2],plFlowerIDPvalue$'p-value'[2],'NA')
Comps

####Pistil length: Section 2: Part 2: Test if Var!=0 using parametric bootstrap

para_plcmod_outcrop=parametric_bootstrap_OutcropSystem(floral08, plcmod, plcmod2)
para_plcmod_samp=parametric_bootstrap_SampCircleID(floral08, plcmod, plcmod3)
para_plcmod_flr=parametric_bootstrap_FlowerID(floral08, plcmod, plcmod4)

####Sepal length: Section 2: Part 1: Test if Var!=0 using LL test
#test hypothesis that VarComp >0

selcmod<-lmer(SepalLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08, REML=TRUE)
selcmod2<-lmer(SepalLength.mm ~ floral08$NDehAnth+(1|SampCircleID/FlowerID),data=floral08, REML=TRUE)
selcmod3<-lmer(SepalLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/FlowerID),data=floral08, REML=TRUE)
selcmod4<-lmer(SepalLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID),data=floral08, REML=TRUE)

selOutcropPvalue=anova(selcmod,selcmod2, refit=FALSE)
selSampCirclePvalue=anova(selcmod,selcmod3, refit=FALSE)
selFlowerIDPvalue=anova(selcmod,selcmod4, refit=FALSE)

Comps$selPvalue<-c(selOutcropPvalue$'p-value'[2],selSampCirclePvalue$'p-value'[2],selFlowerIDPvalue$'p-value'[2],'NA')
Comps

####Sepal length: Section 2: Part 2: Test if Var!=0 using parametric bootstrap
para_sel_outcrop=parametric_bootstrap_OutcropSystem(floral08, selcmod, selcmod2)
para_sel_samp=parametric_bootstrap_SampCircleID(floral08, selcmod, selcmod3)
para_sel_flr=parametric_bootstrap_FlowerID(floral08, selcmod, selcmod4)


####Spur length: Section 2: Part 1: Test if Var!=0 using LL test
#test hypothesis that VarComp >0

splcmod<-lmer(SpurLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID/FlowerID),data=floral08, REML=TRUE)
splcmod2<-lmer(SpurLength.mm ~ floral08$NDehAnth+(1|SampCircleID/FlowerID),data=floral08, REML=TRUE,na.action=na.omit)
splcmod3<-lmer(SpurLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/FlowerID),data=floral08, REML=TRUE)
splcmod4<-lmer(SpurLength.mm ~ floral08$NDehAnth+(1|OutcropSystem/SampCircleID),data=floral08, REML=TRUE)

floral08[is.na(floral08$SpurLength.mm),]


splOutcropPvalue=anova(splcmod,splcmod2, refit=FALSE)
splSampCirclePvalue=anova(splcmod,splcmod3, refit=FALSE)
splFlowerIDPvalue=anova(splcmod,splcmod4, refit=FALSE)

Comps$splPvalue<-c(splOutcropPvalue$'p-value'[2],splSampCirclePvalue$'p-value'[2],splFlowerIDPvalue$'p-value'[2],'NA')
Comps

 
####Spur length: Section 2: Part 2: Test if Var!=0 using parametric bootstrap
para_spl_outcrop=para_splcmod_outcrop=parametric_bootstrap_OutcropSystem(floral08[complete.cases(floral08),], splcmod, splcmod2)
para_spl_samp=parametric_bootstrap_SampCircleID(floral08[complete.cases(floral08),], splcmod, splcmod3)
para_spl_flr=parametric_bootstrap_FlowerID(floral08[complete.cases(floral08),], splcmod, splcmod4)



# here we stitch it together and re-name the variables so they're more useful, while re-ordering the columns such that p values are right next to the variance components for each trait. 

VarComps<-data.frame('SpatialGroup'=Comps$comps,'HerkogamyVarComp'=Comps$hkcppnvars,'HerkogamyPvalue'=Comps$hkPvalue, 'PistilVarComp'=Comps$plcppnvars,'PistilPvalue'=Comps$plPvalue,'SepalVarComp'=Comps$selcppnvars,'SepalPvalue'=Comps$selPvalue,'SpurVarComp'=Comps$splcppnvars,'SpurPvalue'=Comps$splPvalue)

VarComps


#Section 3. Testing the fixed effect of NDehAnth
#when testing fixed effects use ML, but use REML for better estimation of variance components
hkcmod10 = lmer(Herkogamy.mm~NDehAnth+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
hkcmod11 = lmer(Herkogamy.mm~+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(hkcmod10)
anova(hkcmod10,hkcmod11)



plcmod10<-lmer(PistilLength.mm~NDehAnth+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
plcmod11<-lmer(PistilLength.mm~+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(plcmod10)
anova(plcmod10,plcmod11)

selcmod10<-lmer(SepalLength.mm~NDehAnth+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
selcmod11<-lmer(SepalLength.mm~+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(selcmod10)
anova(selcmod10,selcmod11)

splcmod10<-lmer(SpurLength.mm~NDehAnth+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
splcmod11<-lmer(SpurLength.mm~+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(splcmod10)
anova(splcmod10,splcmod11)

#Test for fixed effect of flower position:
hkcmod10_fl_pos<-lmer(Herkogamy.mm~FlwPos+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
hkcmod11_fl_pos<-lmer(Herkogamy.mm~+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(hkcmod10_fl_pos)
anova(hkcmod10_fl_pos,hkcmod11_fl_pos)

plcmod10_fl_pos<-lmer(PistilLength.mm~FlwPos+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
plcmod11_fl_pos<-lmer(PistilLength.mm~(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(plcmod10_fl_pos)
anova(plcmod10_fl_pos,plcmod11_fl_pos)

selcmod10_fl_pos<-lmer(SepalLength.mm~FlwPos+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
selcmod11_fl_pos<-lmer(SepalLength.mm~(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(selcmod10_fl_pos)
anova(selcmod10_fl_pos,selcmod11_fl_pos)

splcmod10_fl_pos<-lmer(SpurLength.mm~FlwPos+(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
splcmod11_fl_pos<-lmer(SpurLength.mm~(1|FlowerID),REML=FALSE,na.action=na.omit,data=floral08)
summary(splcmod10_fl_pos)
anova(splcmod10_fl_pos,splcmod11_fl_pos)
