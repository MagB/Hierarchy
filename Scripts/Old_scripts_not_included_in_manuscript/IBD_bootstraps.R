library(ggplot2)
library(gridExtra)
library(grid)
require(cowplot)



#The point of this script is to bootstrap the parameter estimates and Fstatistics of a linear model of Genetic/Trait Distance plotted against Geogrpahic distance
#In other words this tests if the relationship between genetic/morphological distance and geographic distance is linear, or non-linear. 

#The data used in this script is the same which is used to make the IBD plot, with Fst pairwise distance or the Morph distance (calculated as the euclidean distance of PCA1 and PCA2 of floral traits)
#Just plot of Fst vs centered Distance

IBD=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_1_IBD/Hierstruct_IBD_final.csv", header=T)

#Section 1: Fit univariate and model with quadratic term to the IBD relationships
#First center the predictor variable, distance.
IBD$Distance_centered=(IBD$Distance.m-mean(IBD$Distance.m, na.rm=TRUE))
IBD$Morph_centered=(IBD$Morph.ED-mean(IBD$Morph.ED, na.rm=TRUE))

#univariate models
Morph_regression_uni<-lm(Morph.ED ~ Distance_centered, data=IBD)
summary(Morph_regression_uni)

Fst_regression_uni<-lm(Fst ~ Distance_centered, data=IBD)
summary(Fst_regression_uni)

Fst_vs_morph_uni<-lm(Fst ~ Morph.ED, data=IBD)
summary(Fst_vs_morph_uni)


#nonlinear models
Morph_regression_quad<-lm(Morph.ED ~ Distance_centered+ I(Distance_centered^2), data=IBD)
summary(Morph_regression_quad)

Fst_regression_quad<-lm(Fst ~ Distance_centered+ I(Distance_centered^2), data=IBD)
summary(Fst_regression_quad)


Fst_vs_morph_quad<-lm(Fst ~ Morph_centered + I(Morph_centered^2), data=IBD)
summary(Fst_vs_morph_quad)


##Section 2: There are 2 goals of this section. First, we fit lm model of Fst and Morphological distance versus pairwise geographic distance. Second, we generate 95% CI based on 10 0000 bootstrap randomization of the original data. 

#Part1. Find raw parameter estimates of regression coefficients for the assocaition between Fst, Morphological distance and geographic distance
#Part 2. Generates 95% CI 

#Part 1. Find raw univariate regression predictors for Fst and Morhph.ED
#this function calculates the raw regression coefficients for whatever traits the user defines, the default is the 6 traits we typically use.
#For all analyses distance is centered
raw_regression_univariate <- function(data=IBD,predictor=c("Distance.m"),y=c( "Fst", "Morph.ED")){
        
        if(!(is.data.frame(data))){stop("invalid dataset name")}  
        output_file=data.frame(response=as.character(), intercept=numeric(0), estimate=numeric(0),Fstatistic=numeric(0), stringsAsFactors=FALSE)
        colnames(output_file)=c("response", "intercept","estimate", "Fstat")
        
        
        predictor_columns=c(which(colnames(data) %in% predictor))
        y_var_pos=c(which(colnames(data) %in% y))
        for (i in y_var_pos) {
                myvars=c(predictor_columns, i)
                
                subdata=data[,myvars]
                #subdata=subdata[complete.cases(subdata), ]
                print(dim(subdata))
                #this standardizes the predictore variable
                subdata$centered_predictor=(subdata[,1]-mean(subdata[,1], na.rm=TRUE))
                
                #run regerssion
                regress_raw_coef<-as.numeric(lm(subdata[,2]~subdata$centered_predictor , data=subdata)$coef)
                Fstat=as.numeric(summary(lm(subdata[,2]~subdata$centered_predictor , data=subdata))$fstatistic[1])
                
                newrow=c(colnames(data[i]), regress_raw_coef, Fstat)
                
                output_file=rbind(output_file, data.frame(response=colnames(data[i]), intercept=regress_raw_coef[1],estimate=regress_raw_coef[2], Fstatistic=Fstat))
                
        }
        #colnames(output_file)=c("trait", "intercept","estimate")
        return(output_file)
}

#the function raw_regression_univariate will also prints out the sample sizes
#make sure you do not have a typo in the variable names!
univariate_estimates=raw_regression_univariate(IBD,"Distance.m",c("Fst", "Morph.ED"))

#Get regression for Fst vs Morph
univariate_estimates_Fst_morph=raw_regression_univariate(IBD,"Morph.ED","Fst")

#Part 2. Generate 95%CI
univariate_regression_boot_CI <- function(bootnum=10, data,predictor=c("Distance.m"),y= "Fst"){set.seed(2)
        predictor_columns=c(which(colnames(data) %in% predictor))
        y_var_pos=which(colnames(data) %in% y)
        my_var=c(predictor_columns,y_var_pos)

        subdata=data[,my_var]
        newrow={}
        for(number in c(1:bootnum)){
                
                #this creates a randomized dataset
                #get a randomized dataset
                #we randomize distance
                boot_distance_data={}
                boot_distance_data <- subdata[sample(1:nrow(subdata), nrow(subdata), replace=TRUE),]#this samples with replacement within this data set that consists of only the trait and fitness value for the desired trait
                
                #center the predictor
                boot_distance_data$centered=(boot_distance_data[,which(colnames(boot_distance_data) %in% predictor)]-mean(boot_distance_data[,which(colnames(boot_distance_data) %in% predictor)], na.rm=TRUE))
                #head(boot_distance_data)
                #perform regression on sampled data              
                regress_raw<-lm(boot_distance_data[,which(colnames(boot_distance_data) %in% y)]~boot_distance_data$centered   , data=boot_distance_data)
                
                c<-capture.output(summary(regress_raw))
                
                row_w_Fstat=grep("F-st", c)
                Fstat_val=strsplit( c[row_w_Fstat], " ")[[1]]
                Fstat_val2=Fstat_val[grep("0.",Fstat_val)]
                
                newrow=rbind(newrow, c(coef(regress_raw),Fstat_val2[1]))
                #colnames(newrow)=c("intercept", paste())    
        }
        colnames(newrow)=c("Intercept", "X_var", "Fstatistic")
        newrow=as.data.frame(newrow)
        for(col_id in c(1:ncol(newrow))){
                newrow[,col_id]=as.numeric(as.character(newrow[,col_id]))
        }
        return(newrow)
}

#make 10 000 bootstrapped datasets for the univariate model using centered distance as a predicto
#note that the centering of distance occurs within the univariate_regression_boot_CI function
set.seed(2)
univariate_bootstrapped_Fst=univariate_regression_boot_CI(10000, IBD, "Distance.m", "Fst")
univariate_bootstrapped_Morph=univariate_regression_boot_CI(10000, IBD, "Distance.m", "Morph.ED")
univariate_bootstrapped_Fst_v_Morph=univariate_regression_boot_CI(10000, IBD, "Morph.ED", "Fst")

quantile(univariate_bootstrapped_Fst$X_var, c(0.025, 0.975))
quantile(univariate_bootstrapped_Fst$Fstatistic, c(0.025, 0.975))

quantile(univariate_bootstrapped_Morph$X_var, c(0.025, 0.975))
quantile(univariate_bootstrapped_Morph$Fstatistic, c(0.025, 0.975))

quantile(univariate_bootstrapped_Fst_v_Morph$X_var, c(0.025, 0.975))
quantile(univariate_bootstrapped_Fst_v_Morph$Fstatistic, c(0.025, 0.975))

#Part 3. Fit a nonlinear term to the model genetic/morph distance versus geographic distance.
#Note that we currently do not show the results of the models which include the quadratic term of centered distance
#We did find a statistically significant nonlinear term describing the relationship between Fst distance and geographic distance
#Although a nonlinear function best fits the data, we can think of no reasonable biological interpretation of this trend.

#This fucntion estimates the raw regression parameters
raw_regression_nonlinear <- function(data=IBD,predictor=c("Distance.m"), y=c( "Fst", "Morph.ED")){
        output_file=data.frame(Trait=as.character(),Intercept=as.numeric(), Y=as.numeric(), Y2=as.numeric(),Fstats=as.numeric(), stringsAsFactors=TRUE)
        colnames(output_file)=c("Trait", "intercept", "X_var","X_var2", "Fstat")
        predictor_columns=c(which(colnames(data) %in% predictor))
        y_var_pos=c(which(colnames(data) %in% y))
        
        for (i in y_var_pos) { 
                
                myvars=c(predictor_columns, i)
                #this makes a dataset with only the traits of interest
                subdata=data[,myvars]
                #this removes any incomplete data from the subset data
                subdata=subdata[complete.cases(subdata), ]
                print(dim(subdata))
                
                #this standardizes the x variable
                subdata[,1]=(subdata[,1]-mean(subdata[,1], na.rm=TRUE))
                
                regress_raw<-lm(subdata[,2] ~ subdata[,1]+ I(subdata[,1]^2), data=subdata)
                
                c<-capture.output(summary(regress_raw))
                row_w_Fstat=grep("F-st", c)
                Fstat_val=strsplit( c[row_w_Fstat], " ")[[1]]
                Fstat_val2=Fstat_val[grep("0.",Fstat_val)]
                estimates=c(colnames(subdata)[2], coef(regress_raw)[[1]],coef(regress_raw)[[2]],coef(regress_raw)[[3]],Fstat_val2[1] )
                
                
                names(estimates)=c("Trait", "Intercept"  ,     "X_var"   ,   "X_var2", "Fstatistic" )
                output_file=rbind(output_file,estimates,stringsAsFactors=FALSE)
                
        }
        colnames(output_file)=c("Trait", "Intercept"  ,     "X_var"   ,   "X_var2", "Fstatistic" )
        
        return(output_file)    
        
}

nonlinear_regression_estimates=raw_regression_nonlinear(IBD, "Distance.m","Fst" )
nonlinear_regression_estimates=raw_regression_nonlinear(IBD )

regression_nonlinear_boot_CI <- function(bootnum=10, data,predictor=c("Distance.m"),y= "Fst"){  set.seed(2)
        newrow={}
        newrow=data.frame(Intercept=as.numeric(0), Y_var=as.numeric(0), Y_var2=as.numeric(0), Fstatistic=as.numeric(0))
        colnames(newrow)=c("intercept", "X_var","X_var2", "Fstatistic")
        predictor_columns=c(which(colnames(data) %in% predictor))
        y_var_pos=which(colnames(data) %in% y)
        my_var=c(predictor_columns,y_var_pos)
        #subdata={}
        subdata=data[,my_var]
        head(subdata)
        # distance_subdata=data[,predictor_columns]
        #y_var_subdata=data[,y_var_pos]
        #this finds the columns in the subset data that has the response variable
        #        y_positions=c(which(colnames(subdata) %in% y))
        
        newrow={}
        
        for(number in c(1:bootnum)){
                
                #this creates a randomized dataset
                #get a randomized dataset
                #we randomize distance
                boot_distance_data={}
                boot_distance_data <- subdata[sample(1:nrow(subdata), nrow(subdata), replace=TRUE),]#this samples with replacement within this data set that consists of only the trait and fitness value for the desired trait
                head(boot_distance_data)
                head(subdata)
                #center the predictor
                boot_distance_data$centered=(boot_distance_data$Distance.m-mean(boot_distance_data$Distance.m, na.rm=TRUE))
                
                
                #now I do the multivariate regression
                
                regress_raw<-lm(boot_distance_data[,which(colnames(boot_distance_data) %in% y)]~boot_distance_data$centered  +I(boot_distance_data$centered^2) , data=boot_distance_data)
                
                c<-capture.output(summary(regress_raw))
                
                row_w_Fstat=grep("F-st", c)
                Fstat_val=strsplit( c[row_w_Fstat], " ")[[1]]
                Fstat_val2=Fstat_val[grep("0.",Fstat_val)]
                
                newrow=rbind(newrow, c(coef(regress_raw),Fstat_val2[1]))
                #colnames(newrow)=c("intercept", paste())    
        }
        colnames(newrow)=c("Intercept", "X_var","X_var2", "Fstatistic")
        newrow=as.data.frame(newrow)
        for(col_id in c(1:ncol(newrow))){
                newrow[,col_id]=as.numeric(as.character(newrow[,col_id]))
        }
        return(newrow)
}

set.seed(2)
quad_data_Fst_CI=regression_nonlinear_boot_CI(10000,IBD, "Distance.m", "Fst")
quad_data_Morph_CI=regression_nonlinear_boot_CI(10000,IBD, "Distance.m", "Morph.ED")


#95% CI
quantile(quad_data_Fst_CI$Fstatistic, c(0.025, 0.975))
quantile(quad_data_Fst_CI$X_var, c(0.025, 0.975))
quantile(quad_data_Fst_CI$X_var2, c(0.025, 0.975))

quantile(quad_data_Morph_CI$Fstatistic, c(0.025, 0.975))
quantile(quad_data_Morph_CI$X_var, c(0.025, 0.975))
quantile(quad_data_Morph_CI$X_var2, c(0.025, 0.975))

#Section 3: Hypothesis testing
#Permutation test to estimate Pvalues

#generate perumuted datasets for simple linear regression
regression_linear_permute <- function(bootnum=10, data,predictor=c("Distance.m"),y= "Fst"){  set.seed(2)
       
        predictor_columns=c(which(colnames(data) %in% predictor))
        y_var_pos=which(colnames(data) %in% y)
        
        
        distance_subdata=data[,predictor_columns]
        y_var_subdata=data[,y_var_pos]

        newrow={}
        
        for(number in c(1:bootnum)){
                
                #this creates a randomized dataset
                #get a randomized dataset
                #we randomize distance
                boot_distance <- distance_subdata[sample(1:length(distance_subdata), length(distance_subdata), replace=FALSE)]#this samples without replacement within this data set that consists of only the trait and fitness value for the desired trait
                
                #center the predictor
                boot_distance_centered=(boot_distance-mean(boot_distance, na.rm=TRUE))
                
                
                #now I do the multivariate regression
                
                regress_raw<-lm(y_var_subdata~boot_distance_centered )
                
                c<-capture.output(summary(regress_raw))
                
                row_w_Fstat=grep("F-st", c)
                Fstat_val=strsplit( c[row_w_Fstat], " ")[[1]]
                Fstat_val2=Fstat_val[grep("0.",Fstat_val)]
                
                newrow=rbind(newrow, c(coef(regress_raw),Fstat_val2[1]))
        }
        colnames(newrow)=c("Intercept", "X_var", "Fstatistic")
        newrow=as.data.frame(newrow)
        for(col_id in c(1:ncol(newrow))){
                newrow[,col_id]=as.numeric(as.character(newrow[,col_id]))
        }
        return(newrow)
}

set.seed(2)
univariate_permuted_Fst=regression_linear_permute(10000,IBD, "Distance.m", "Fst")
univariate_permuted_Morph=regression_linear_permute(10000,IBD, "Distance.m", "Morph.ED")
univariate_permuted_Fst_v_Morph=regression_linear_permute(10000,IBD, "Morph.ED", "Fst")

#generate permuted datasets for regressoin with nonlinear term
regression_nonlinear_permute <- function(bootnum=10, data,predictor=c("Distance.m"),y= "Fst"){  set.seed(2)
        newrow={}
        newrow=data.frame(Intercept=as.numeric(0), Y_var=as.numeric(0), Y_var2=as.numeric(0), Fstatistic=as.numeric(0))
        colnames(newrow)=c("intercept", "X_var","X_var2", "Fstatistic")
        
        predictor_columns=c(which(colnames(data) %in% predictor))
        y_var_pos=which(colnames(data) %in% y)

        
        distance_subdata=data[,predictor_columns]
        y_var_subdata=data[,y_var_pos]
        #this finds the columns in the subset data that has the response variable
        y_positions=c(which(colnames(subdata) %in% y))
        
        newrow={}
        
        for(number in c(1:bootnum)){
                
                #this creates a randomized dataset
                #get a randomized dataset
                #we randomize distance
                boot_distance <- distance_subdata[sample(1:length(distance_subdata), length(distance_subdata), replace=FALSE)]#this samples without replacement within this data set that consists of only the trait and fitness value for the desired trait
               
                #center the predictor
                boot_distance_centered=(boot_distance-mean(boot_distance, na.rm=TRUE))
                
                
                #now I do the multivariate regression
                
                regress_raw<-lm(y_var_subdata~boot_distance_centered  +I(boot_distance_centered^2) )

                c<-capture.output(summary(regress_raw))
                
                row_w_Fstat=grep("F-st", c)
                Fstat_val=strsplit( c[row_w_Fstat], " ")[[1]]
                Fstat_val2=Fstat_val[grep("0.",Fstat_val)]
                
                newrow=rbind(newrow, c(coef(regress_raw),Fstat_val2[1]))
                #colnames(newrow)=c("intercept", paste())    
        }
        colnames(newrow)=c("Intercept", "X_var","X_var2", "Fstatistic")
        newrow=as.data.frame(newrow)
        for(col_id in c(1:ncol(newrow))){
                newrow[,col_id]=as.numeric(as.character(newrow[,col_id]))
        }
        return(newrow)
}

set.seed(2)
quad_permuted_Fst=regression_nonlinear_permute (10000,IBD, "Distance.m", "Fst")
quad_permuted_Morph=regression_nonlinear_permute (10000,IBD, "Distance.m", "Morph.ED")



#Get P-values for univariate linear estimates
get_Pval = function(data2, variable, raw_estimate_values){
        subdata=data2[,which(colnames(data2) %in% variable)]
        quants=data.frame(row.names=NULL)
 
        if(raw_estimate_values<0){pval=(length(subdata[subdata<=raw_estimate_values])/length(subdata))}
        else{pval=(length(subdata[subdata>=raw_estimate_values])/length(subdata))}

        p_val=pval  
        
        newrow=c(colnames(data2)[which(colnames(data2) %in% variable)],p_val)
        quants=as.matrix(rbind(quants,newrow),row.name=NULL)
        
        
        quants=as.data.frame(quants, row.names=c(1:length(quants)))
        colnames(quants)=c("trait","p_val")
        
        return(quants)
}

get_Pval(univariate_permuted_Fst, "X_var", raw_estimate_values=0.0000001824572)
get_Pval(univariate_permuted_Fst, "Fstatistic", raw_estimate_values=0.01263198)

get_Pval(univariate_permuted_Morph, "X_var", raw_estimate_values=-0.0000068645)
get_Pval(univariate_permuted_Morph, "Fstatistic", raw_estimate_values=0.04089772)

get_Pval(univariate_permuted_Fst_v_Morph, "X_var", raw_estimate_values=0.01062577)
get_Pval(univariate_permuted_Fst_v_Morph, "Fstatistic", raw_estimate_values=4.62285)

#Get P-values for quadratic estimates
get_Pval(quad_data_Fst, "X_var", raw_estimate_values=0.000001009757945)
get_Pval(quad_data_Fst, variable="X_var2", raw_estimate_values=-0.0000000033939)
get_Pval(quad_data_Fst, "Fstatistic", raw_estimate_values=3.096)


get_Pval(quad_data_Morph, "X_var", raw_estimate_values=0.00000632328729)
get_Pval(quad_data_Morph, variable="X_var2", raw_estimate_values=-0.0000000541027401356584)
get_Pval(quad_data_Morph, "Fstatistic", raw_estimate_values=0.177)


