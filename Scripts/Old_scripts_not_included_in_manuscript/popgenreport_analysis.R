install.packages("PopGenReport")
require(PopGenReport)
library("adegenet")
data(bilby)
summary(bilby)
View(bilby)

?read.genetable()
platy <- read.genetable("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat.csv", ind=1, pop=2, other.min=3, other.max=4, oneColPerAll = FALSE, sep=":", ploidy=2)

 #note you have to leave the uncalled genotypes blank
#NOTE THE SEP in the read.genetable is what separator do you use for the microsat alleles!!!!!!!!
head(platy)
head(platy)
summary( platy)
is.genind(platy)

head(platy$tab)

temp_data=platy$tab



allel.rich(platy, min.alleles = NULL)
popgenreport(platy, mk.allel.rich=TRUE, mk.pdf=FALSE)

1/0.32
platy.complete <- popgenreport(platy, path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)

platy[platy$pop=="QBED1"]

platy.complete.QBED1 <- popgenreport(platy[platy$pop=="QBED1"], path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/QBED1/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)
platy.complete.QBED2 <- popgenreport(platy[platy$pop=="QBED2"], path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/QBED2/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)

platy.complete.QLL1 <- popgenreport(platy[platy$pop=="QLL1"], path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/QLL1/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)
platy.complete.QLL3 <- popgenreport(platy[platy$pop=="QLL3"], path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/QLL3/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)


platy.complete.QRL1 <- popgenreport(platy[platy$pop=="QRL1"], path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/QRL1/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)
platy.complete.QRL2 <- popgenreport(platy[platy$pop=="QRL2"], path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/QRL2/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)

platy.complete.QTUR <- popgenreport(platy[platy$pop=="QTUR"], path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/QTUR/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)


summary(platy.complete)
platy.complete$counts

platy.complete$allel.rich
platy.complete$locihz
platy.complete$hwe

allele.dist(platy, mk.figure=FALSE)
library(adegenet)
library(ape)
is.genind(platy)
summary(platy)
heterozygosity(platy)
library("adegenet")
library("pegas")
library("hierfstat")
installPackages("hierfstat")

data("nancycats", package = "adegenet")  
summary(platy)
View(nancycats)

allele.count(platy)
basic.stats(platy,diploid=TRUE,digits=4)

library(gstudio)
require(devtools)
vignette(gstudio)

install_github("gstudio", "dyerlab", ref = "develop")
install.packages("gstudio")

Gstudmydat <- read_population("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/data_for_publication/2008Geno_mpb_temp.csv",type="column",  locus.columns=5:13)
str(Gstudmydat)

Gstudmydat$loc1
