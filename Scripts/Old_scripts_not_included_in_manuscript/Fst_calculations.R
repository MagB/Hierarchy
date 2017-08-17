require(PopGenReport)
library("adegenet")


microsat_canadensis1 <- read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat.csv")

microsat_canadensis1$pop=paste(microsat_canadensis1$pop, microsat_canadensis1$quadrat, sep="_")
head(microsat_canadensis1)

write.csv(microsat_canadensis1, "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat_by_plot.csv",row.names=FALSE)

microsat_canadensis <- read.genetable("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat_by_plot.csv", ind=1, pop=2, other.min=3, other.max=4, oneColPerAll = FALSE, sep=":", ploidy=2)


microsat_canadensis_complete_popgen_report <- popgenreport(microsat_canadensis, path.pgr = "/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/popgenreport_output/",mk.complete=TRUE, mk.Rcode=TRUE, mk.pdf = FALSE)
