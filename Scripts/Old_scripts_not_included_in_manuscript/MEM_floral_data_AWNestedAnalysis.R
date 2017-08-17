#Nested analysis of AW & Stephen's flower data using Lme
#this script was written by CGE, updated by AW and verified and documented by MPB

# the objective of this analysis is to 1) Estimate Variance Components for the three spatial categories where the plants were sampled and 2) Test whether the individual variance components are statistically significantly different from 0. 
library(dplyr)
rm(list=ls())
#read.csv("~/Dropbox/AndyWong/floral08.csv", header=TRUE)->floral08
floral08<-read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/flower data/Nested ANOVA/floral08.csv",header=T)

# For Chris
floral08 <- read.csv("/Users/chriseckert/Dropbox/Shared/AndyWong/1.\ Hierarchical\ Structure/Maggie_Hierarchy/data/floral08.csv", header = TRUE)


dim(floral08)
head(floral08)
summary(floral08)
floral08$X<-NULL
floral08$SampCircle<-as.factor(floral08$SampCircle)
floral08$Plant<-as.factor(floral08$Plant)
floral08$rep<-as.factor(floral08$rep)

# ***never use attach
attach(floral08)


#Verify some parts of the data:
#According to ANdy there is supposed to be 280 plants measured in total, so how are there 
7*2*20 # so why are there 341 plants?
#More than 20 plants per plot were measured for floral traits
#Find the number of times each plants was measured
temp1=floral08 %>% group_by(SampCircleID,FlowerID) %>%  summarise(n=n())
#there are 4 plants which were only measured once.
temp1[temp1$n!=2,]

# Find the number of plants sampled for each outcrop
floral08 %>% group_by(OutcropSystem) %>%  summarise(n=n())
temp2=floral08 %>% group_by(SampCircleID) %>%  summarise(n=length(unique(FlowerID)))

# Find trait means and sd for each plot:

# First we have to average replicate observations
rep_means <- floral08 %>% group_by(OutcropSystem, SampCircleID, FlowerID) %>% summarise(Herkogamy.mm = mean(Herkogamy.mm), PistilLength.mm = mean(PistilLength.mm), SepalLength.mm = mean(SepalLength.mm), SpurLength.mm = mean(SpurLength.mm, na.rm=TRUE))
dim(floral08)
dim(rep_means)
rep_means <- data.frame(rep_means)

# Now we analyse flower means within plots
plot_mean_trait <- rep_means %>% group_by(SampCircleID) %>% summarise(n = length(Herkogamy.mm), mean_Herk=mean(Herkogamy.mm), sd_Herk=sd(Herkogamy.mm), CV_Herk = (sd_Herk/mean_Herk)*100, mean_Pistil=mean(PistilLength.mm), sd_Pistil=sd(PistilLength.mm), CV_Pistil = (sd_Pistil/mean_Pistil)*100, mean_Sepal = mean(SepalLength.mm), sd_Sepal = sd(SepalLength.mm), CV_Sepal = (sd_Sepal/mean_Sepal)*100, mean_Spur = mean(SpurLength.mm, na.rm=TRUE), sd_Spur = sd(SpurLength.mm, na.rm=TRUE), CV_Spur = (sd_Spur/mean_Spur)*100)

(plot_mean_trait <- data.frame(plot_mean_trait))

write.csv(plot_mean_trait, file = "plot_mean_trait.csv")

outcrop_mean_trait= floral08 %>% group_by(OutcropSystem) %>% summarise(mean_Herk=mean(Herkogamy.mm), sd_Herk=sd(Herkogamy.mm), mean_Pistil=mean(PistilLength.mm), sd_Pistil=sd(PistilLength.mm), mean_sep=mean(SepalLength.mm), sd_sep=sd(SepalLength.mm), mean_spur=mean(SpurLength.mm, na.rm=TRUE), sd_spur=sd(SpurLength.mm, na.rm=TRUE))

#Here I test if population trait means differ among outcrops. 
for(i in colnames(floral08[12:15])){
        temp=floral08[,which(colnames(floral08)==i)]
        lme1=lme(temp~NDehAnth + OutcropSystem ,random=~1|SampCircleID/FlowerID,na.action=na.omit,method="ML")
        lme2=lme(temp~NDehAnth  ,random=~1|SampCircleID/FlowerID,na.action=na.omit,method="ML")
        print(i)
        print(anova(lme1,lme2))
        #print(summary(aov(temp ~ SampCircleID, data=floral08)))
}
#only difference between populations is for Spur length
lme_spur=lme(SpurLength.mm~NDehAnth + OutcropSystem ,random=~1|SampCircleID/FlowerID,na.action=na.omit,method="REML")
summary(lme_spur)







hist(NDehAnth)
par(mfrow=c(2,2))
hist(Herkogamy.mm)
hist(PistilLength.mm)
hist(SepalLength.mm)
hist(SpurLength.mm)

plot(Herkogamy.mm,PistilLength.mm)
plot(SepalLength.mm,SpurLength.mm)
plot(Herkogamy.mm,SepalLength.mm)
plot(Herkogamy.mm,SepalLength.mm)

plot(NDehAnth,Herkogamy.mm)
plot(NDehAnth,PistilLength.mm)
plot(NDehAnth,SepalLength.mm)
plot(NDehAnth,SpurLength.mm)

cor.test(NDehAnth,Herkogamy.mm)
cor.test(NDehAnth,PistilLength.mm)
cor.test(NDehAnth,SepalLength.mm)
cor.test(NDehAnth,SpurLength.mm)

comps<-c("Among outcrops","Between plots within outcrops","Among flowers within plots","Measurement error")

quartz(width=5,height=7,family="Helvetica",type="pdf", file='/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Supp_Figure_S1_boxplot_traits.pdf')
quartz(width=5,height=7,family="Helvetica")

par(mfrow=c(4,1))
par(oma=c(8,4,2.5,0.5))
par(mar=c(0,2,0,0))
plot(Herkogamy.mm~SampCircleID, xaxt='n')
text(2,9,"Herkogamy",cex=1.25)
plot(PistilLength.mm~SampCircleID, xaxt='n')
text(1,28,"Pistil",cex=1.25)
plot(SepalLength.mm~SampCircleID, xaxt='n')
text(1,20,"Sepal",cex=1.25)
plot(SpurLength.mm~SampCircleID)
text(1,40,"Spur",cex=1.25)
mtext(side=1,line=3,cex=1.25,"Samping Circles ID")
mtext(side=2,line=3, at=69,cex=1.25,"Traits (mm)")
dev.off()

#Estimating variance components
#library(nlme)
library(lme4)

# these models estimate the variance components. 

hkcmod<-lme(Herkogamy.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit, data=floral08)

#testing sept 20
hkcvars<-c(as.numeric(VarCorr(hkcmod)[2,1]),as.numeric(VarCorr(hkcmod)[4,1]),as.numeric(VarCorr(hkcmod)[6,1]),as.numeric(VarCorr(hkcmod)[7,1]))
test1=summary(hkcmod)
names(test1)
test1$coefficients
names(test1$varcor)
str(test1$varcor)
test1$varcor
(sam<-1.61942)
(tech<-0.14215)
(lab<-0.22678)
res=0.15588 
(me<-(as.numeric(sigma(hkcmod)))^2)

hkvars<-c(lab, tech, sam, res)
hkpvars<-100*hkvars/sum(hkvars)



VarCorr(hkcmod)
names(hkcmod)
summary(hkcmod)
VarCorr(hkcmod)
#test model assumptions:
hist(residuals(hkcmod))
qqnorm(residuals(hkcmod))
shapiro.test(residuals(hkcmod)) #residuals not normally distribted (did we expect them to be?)
plot(residuals(hkcmod)~fitted(hkcmod))
abline(h=0, col="red")
plot(abs(residuals(hkcmod))~fitted(hkcmod))
cor.test(abs(residuals(hkcmod)),fitted(hkcmod)) #Significant correlation between predicted values and magnitude of residual
plot(residuals(hkcmod)~floral08$NDehAnth)

plcmod<-lme(PistilLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit)
#test model assumptions:
hist(residuals(plcmod))
qqnorm(residuals(plcmod))
shapiro.test(residuals(plcmod)) #residuals not normally distribted (did we expect them to be?)
plot(residuals(plcmod)~fitted(plcmod))
abline(h=0, col="red")
plot(abs(residuals(plcmod))~fitted(plcmod))
cor.test(abs(residuals(plcmod)),fitted(plcmod)) #Significant correlation between predicted values and magnitude of residual
plot(residuals(plcmod)~floral08$NDehAnth)
abline(h=0, col="red")

selcmod<-lme(SepalLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit)
#test model assumptions:
hist(residuals(selcmod))
qqnorm(residuals(selcmod))
shapiro.test(residuals(selcmod)) #residuals not normally distribted (did we expect them to be?)
plot(residuals(plcmod)~fitted(selcmod))
abline(h=0, col="red")
plot(abs(residuals(selcmod))~fitted(selcmod))
abline(h=0, col="red")
cor.test(abs(residuals(selcmod)),fitted(selcmod)) #Significant correlation between predicted values and magnitude of residual
plot(residuals(selcmod)~floral08$NDehAnth)
abline(h=0, col="red")

splcmod<-lme(SpurLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit)
hist(residuals(splcmod))
qqnorm(residuals(splcmod))
shapiro.test(residuals(splcmod)) #residuals not normally distribted (did we expect them to be?)
plot(residuals(splcmod)~fitted(splcmod))
abline(h=0, col="red")
plot(abs(residuals(splcmod))~fitted(splcmod))
abline(h=0, col="red")
cor.test(abs(residuals(splcmod)),fitted(splcmod)) #Significant correlation between predicted values and magnitude of residual
plot(residuals(splcmod)~floral08$NDehAnth)
abline(h=0, col="red")

# here we extract the above estimated variance components from the models

hkcvars<-c(as.numeric(VarCorr(hkcmod)[2,1]),as.numeric(VarCorr(hkcmod)[4,1]),as.numeric(VarCorr(hkcmod)[6,1]),as.numeric(VarCorr(hkcmod)[7,1]))

hkcppnvars<-100*hkcvars/sum(hkcvars)
Comps<-data.frame(comps,hkcppnvars)
Comps






plcvars<-c(as.numeric(VarCorr(plcmod)[2,1]),as.numeric(VarCorr(plcmod)[4,1]),as.numeric(VarCorr(plcmod)[6,1]),as.numeric(VarCorr(plcmod)[7,1]))

plcppnvars<-100*plcvars/sum(plcvars)
Comps$plcppnvars<-plcppnvars
Comps

selcvars<-c(as.numeric(VarCorr(selcmod)[2,1]),as.numeric(VarCorr(selcmod)[4,1]),as.numeric(VarCorr(selcmod)[6,1]),as.numeric(VarCorr(selcmod)[7,1]))

selcppnvars<-100*selcvars/sum(selcvars)
Comps$selcppnvars<-selcppnvars
Comps

splcvars<-c(as.numeric(VarCorr(splcmod)[2,1]),as.numeric(VarCorr(splcmod)[4,1]),as.numeric(VarCorr(splcmod)[6,1]),as.numeric(VarCorr(splcmod)[7,1]))

splcppnvars<-100*splcvars/sum(splcvars)
Comps$splcppnvars<-splcppnvars
Comps


# Now we test the significance of the individual variance components by removing the various nested spatial variables sequentially.
# for herkogamy

hkcmod<-lme(Herkogamy.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit)
hkcmod2<-lme(Herkogamy.mm~NDehAnth,random=~1|SampCircleID/FlowerID,na.action=na.omit)
hkcmod3<-lme(Herkogamy.mm~NDehAnth,random=~1|OutcropSystem/FlowerID,na.action=na.omit)
hkcmod4<-lme(Herkogamy.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID,na.action=na.omit)

anova(hkcmod,hkcmod2)->hkOutcropPvalue
anova(hkcmod,hkcmod3)->hkSampCirclePvalue
anova(hkcmod,hkcmod4)->hkFlowerIDPvalue

Comps$hkPvalue<-c(hkOutcropPvalue$'p-value'[2],hkSampCirclePvalue$'p-value'[2],hkFlowerIDPvalue$'p-value'[2],'NA') 
Comps


# for pistil length

plcmod<-lme(PistilLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit)
plcmod2<-lme(PistilLength.mm~NDehAnth,random=~1|SampCircleID/FlowerID,na.action=na.omit)
plcmod3<-lme(PistilLength.mm~NDehAnth,random=~1|OutcropSystem/FlowerID,na.action=na.omit)
plcmod4<-lme(PistilLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID,na.action=na.omit)

anova(plcmod,plcmod2)->plOutcropPvalue
anova(plcmod,plcmod3)->plSampCirclePvalue
anova(plcmod,plcmod4)->plFlowerIDPvalue

Comps$plPvalue<-c(plOutcropPvalue$'p-value'[2],plSampCirclePvalue$'p-value'[2],plFlowerIDPvalue$'p-value'[2],'NA')
Comps

# for sepal length

selcmod<-lme(SepalLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit)
selcmod2<-lme(SepalLength.mm~NDehAnth,random=~1|SampCircleID/FlowerID,na.action=na.omit)
selcmod3<-lme(SepalLength.mm~NDehAnth,random=~1|OutcropSystem/FlowerID,na.action=na.omit)
selcmod4<-lme(SepalLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID,na.action=na.omit)

anova(selcmod,selcmod2)->selOutcropPvalue
anova(selcmod,selcmod3)->selSampCirclePvalue
anova(selcmod,selcmod4)->selFlowerIDPvalue

Comps$selPvalue<-c(selOutcropPvalue$'p-value'[2],selSampCirclePvalue$'p-value'[2],selFlowerIDPvalue$'p-value'[2],'NA')
Comps

# for spur length

splcmod<-lme(SpurLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID/FlowerID,na.action=na.omit)
splcmod2<-lme(SpurLength.mm~NDehAnth,random=~1|SampCircleID/FlowerID,na.action=na.omit)
splcmod3<-lme(SpurLength.mm~NDehAnth,random=~1|OutcropSystem/FlowerID,na.action=na.omit)
splcmod4<-lme(SpurLength.mm~NDehAnth,random=~1|OutcropSystem/SampCircleID,na.action=na.omit)

anova(splcmod,splcmod2)->splOutcropPvalue
anova(splcmod,splcmod3)->splSampCirclePvalue
anova(splcmod,splcmod4)->splFlowerIDPvalue

Comps$splPvalue<-c(splOutcropPvalue$'p-value'[2],splSampCirclePvalue$'p-value'[2],splFlowerIDPvalue$'p-value'[2],'NA')
Comps

# here we stitch it together and re-name the variables so they're more useful, while re-ordering the columns such that p values are right next to the variance components for each trait. 

VarComps<-data.frame('SpatialGroup'=Comps$comps,'HerkogamyVarComp'=Comps$hkcppnvars,'HerkogamyPvalue'=Comps$hkPvalue, 'PistilVarComp'=Comps$plcppnvars,'PistilPvalue'=Comps$plPvalue,'SepalVarComp'=Comps$selcppnvars,'SepalPvalue'=Comps$selPvalue,'SpurVarComp'=Comps$splcppnvars,'SpurPvalue'=Comps$splPvalue)

VarComps

write.csv(VarComps, file="~/Dropbox/AndyWong/VarComps.csv")

#To test the fixed effect of NDehAnth

hkcmod10<-lme(Herkogamy.mm~NDehAnth,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(hkcmod10)
hkcmod11<-lme(Herkogamy.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
anova(hkcmod10,hkcmod11)



plcmod10<-lme(PistilLength.mm~NDehAnth,random=~1|FlowerID,na.action=na.omit,method="ML")
plcmod11<-lme(PistilLength.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(plcmod10)
anova(plcmod10,plcmod11)

selcmod10<-lme(SepalLength.mm~NDehAnth,random=~1|FlowerID,na.action=na.omit,method="ML")
selcmod11<-lme(SepalLength.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(selcmod10)
anova(selcmod10,selcmod11)

splcmod10<-lme(SpurLength.mm~NDehAnth,random=~1|FlowerID,na.action=na.omit,method="ML")
splcmod11<-lme(SpurLength.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(splcmod10)
anova(splcmod10,splcmod11)

#Test for fixed effect of flower position:
hkcmod10_fl_pos<-lme(Herkogamy.mm~FlwPos,random=~1|FlowerID,na.action=na.omit,method="ML")
hkcmod11_fl_pos<-lme(Herkogamy.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(hkcmod10_fl_pos)
anova(hkcmod10_fl_pos,hkcmod11_fl_pos)




plcmod10_fl_pos<-lme(PistilLength.mm~FlwPos,random=~1|FlowerID,na.action=na.omit,method="ML")
plcmod11_fl_pos<-lme(PistilLength.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(plcmod10_fl_pos)
anova(plcmod10_fl_pos,plcmod11_fl_pos)

selcmod10_fl_pos<-lme(SepalLength.mm~FlwPos,random=~1|FlowerID,na.action=na.omit,method="ML")
selcmod11_fl_pos<-lme(SepalLength.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(selcmod10_fl_pos)
anova(selcmod10_fl_pos,selcmod11_fl_pos)

splcmod10_fl_pos<-lme(SpurLength.mm~FlwPos,random=~1|FlowerID,na.action=na.omit,method="ML")
splcmod11_fl_pos<-lme(SpurLength.mm~1,random=~1|FlowerID,na.action=na.omit,method="ML")
summary(splcmod10_fl_pos)
anova(splcmod10_fl_pos,splcmod11_fl_pos)
