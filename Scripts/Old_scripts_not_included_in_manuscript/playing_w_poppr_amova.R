install.packages("poppr")
library(poppr)
library(adegenet)


mygenclone <- as.genclone(microsat_canadensis, strata=pop)
myhier <- read.table('/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/myhier.txt', header=T)
strata(mygenclone) <-myhier
amova.result <- poppr.amova(mygenclone, ~pop)




mygenclone_seploc <- seploc(mygenclone)
amova_res <- lapply(mygenclone_seploc, poppr.amova, hier=~reg/pop, within=TRUE)





table(strata(microsat_canadensis))



help("df2genind")
popge=genind2df(microsat_canadensis, sep = ":", )
monpop <- read.genalex(popge)



class(microsat_canadensis)
head(popge)

table(strata(popge, ~Pop)) 

strata(microsat_canadensis)=other(microsat_canadensis)$pop
