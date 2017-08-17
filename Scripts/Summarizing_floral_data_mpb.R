library(lme4)
#Here I test if population trait means differ among outcrops. 
#This script also makes Table S5
rm(list=ls())

floral08=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_floral_morphology.csv", header=TRUE)
head(floral08)
# Find the number of plants sampled for each outcrop
sample_size=floral08 %>% group_by(SampCircleID) %>%  summarise(n=length(unique(FlowerID)))


# Find trait means and sd for each plot:

# Step1. average replicate observations
rep_means <- floral08 %>% group_by(OutcropSystem, SampCircleID, FlowerID) %>% summarise(NDehAnth=mean(NDehAnth),FlwPos=mean(FlwPos), Herkogamy.mm = mean(Herkogamy.mm), PistilLength.mm = mean(PistilLength.mm), SepalLength.mm = mean(SepalLength.mm), SpurLength.mm = mean(SpurLength.mm, na.rm=TRUE))
nrow(rep_means)

rep_means <- data.frame(rep_means)


# Find sample size and trait means, CV and sd within plots
plot_mean_trait <- rep_means %>% group_by(SampCircleID) %>% summarise(n = length(Herkogamy.mm), mean_Herk=mean(Herkogamy.mm), sd_Herk=sd(Herkogamy.mm), CV_Herk = (sd_Herk/mean_Herk)*100, mean_Pistil=mean(PistilLength.mm), sd_Pistil=sd(PistilLength.mm), CV_Pistil = (sd_Pistil/mean_Pistil)*100, mean_Sepal = mean(SepalLength.mm), sd_Sepal = sd(SepalLength.mm), CV_Sepal = (sd_Sepal/mean_Sepal)*100, mean_Spur = mean(SpurLength.mm, na.rm=TRUE), sd_Spur = sd(SpurLength.mm, na.rm=TRUE), CV_Spur = (sd_Spur/mean_Spur)*100)
(plot_mean_trait <- data.frame(plot_mean_trait))

#Find flower means within plots
plot_mean_trait <- rep_means %>% group_by(SampCircleID) %>% summarise(n = length(Herkogamy.mm), mean_Herk=mean(Herkogamy.mm), sd_Herk=sd(Herkogamy.mm), CV_Herk = (sd_Herk/mean_Herk)*100, mean_Pistil=mean(PistilLength.mm), sd_Pistil=sd(PistilLength.mm), CV_Pistil = (sd_Pistil/mean_Pistil)*100, mean_Sepal = mean(SepalLength.mm), sd_Sepal = sd(SepalLength.mm), CV_Sepal = (sd_Sepal/mean_Sepal)*100, mean_Spur = mean(SpurLength.mm, na.rm=TRUE), sd_Spur = sd(SpurLength.mm, na.rm=TRUE), CV_Spur = (sd_Spur/mean_Spur)*100)
(plot_mean_trait <- data.frame(plot_mean_trait))

write.csv(plot_mean_trait, file = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/plot_mean_trait.csv")

#
for(i in colnames(floral08[12:15])){
        
        temp= floral08 %>% select(NDehAnth, SampCircleID,FlowerID,which(colnames(floral08)==i)) %>% filter(complete.cases(.))
        lme1=lmer(temp[,4]~temp[,1]+ temp[,2] +(1|temp[,3]),data=temp,REML=FALSE, na.action = na.omit)
        lme2=lmer(temp[,4]~temp[,1]+ (1|temp[,3]),data=temp,REML=FALSE, na.action = na.omit)
        summary(lme1)
        print(anova(lme1,lme2))
        #print(summary(aov(temp ~ SampCircleID, data=floral08)))
}
#only difference between populations is for Spur length
lme_spur=lme(SpurLength.mm~NDehAnth + OutcropSystem ,random=~1|SampCircleID/FlowerID,na.action=na.omit,method="REML")
summary(lme_spur)

par(cex=.6)

with(floral08, interaction.plot(floral08$NDehAnth, floral08$SampCircleID,floral08$Herkogamy.mm,
        lty = c(1, 12), lwd = 3,col=rainbow(14),
        ylab = "Herkogamy", xlab = "NDehAnth", trace.label = "group"))


data.aov <- aov(floral08$Herkogamy.mm~ floral08$NDehAnth + floral08$SampCircleID+ Error(floral08$FlowerID/floral08$SampCircleID), data = floral08)


temp_ks= floral08 %>% summarise(new_mean=mean(Herkogamy.mm,SepalLength.mm))
my_data=floral08
my_data$new_column=rowMeans(my_data[,which(colnames(my_data)=="Herkogamy.mm" |colnames(my_data)=="SepalLength.mm" )])

my_data$NDehAnth-my_data$Herkogamy.mm
is.na(my_data$Herkogamy.mm)

my_data$Herkogamy.mm[1]=NA
