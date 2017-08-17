
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library("gdata")
library(ggmap)
rm(list=ls())
devtools::install_github("dkahle/ggmap", force=TRUE)
devtools::install_github("hadley/ggplot2")
library(ggplot2)
#This script plots the 7 outcrops used in this study. The plots within the populations are within 30m 
#of one another and thus are too close to one another to show on a map of this scale.

#This script is partitioned into 2 parts.
#Part 1. I convert the UTM coordinates recorded by AW into Lat and Long coordinates
#Part 2. I generate a map using a google base map (terrain) using the ggmap function and plot the populations
#Part 3. 

#PART 1. Convert UTM in to Lat Long for ggmap
#see this site for the code for UTM conversion
#https://sites.google.com/a/lakeheadu.ca/yong-luo/blog/convert-utm-to-longlat-in-r

#import the UTM data
Distance=read.csv("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_plot_distances.csv",header=T)

head(Distance)

#Make new columns for the converted Lat and Long
#x is easting
utmcoor<-SpatialPoints(cbind(Distance$UTM_E.m.,Distance$UTM_N.m.), proj4string=CRS("+proj=utm +zone=18T"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
unlist(unlist(longlatcoor))
class(longlatcoor)
longlatcoor=as.data.frame(longlatcoor)
Distance$lat=longlatcoor$coords.x2
Distance$lon=longlatcoor$coords.x1


#PART 2. Plot the 7 populations onto a google base map using ggmap

#This site contained the code I used to make this plot
#http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
library(dplyr)
dev.off()
Distance=Distance %>% arrange(lat)
sbbox <- make_bbox(lon = Distance$lon, lat = Distance$lat, f = 2)
sq_map <- get_map(location = sbbox, color = "bw",maptype = "terrain", source = "google")
#library(devtools)
#remove.packages('ggplot2')
#install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")
#install.packages("ggmap")
#library(ggplot2)
#library("ggmap")
A_canadensis_7_pop_map=ggmap(sq_map) + geom_point(data = Distance, mapping = aes(x = lon, y = lat), color = "black") + xlab("Longitude") +ylab("Latitude") +  
        annotate('text', x=-76.37, y=44.54563, label = 'QTUR', colour = I('black'), size = 4)+
        annotate('text', x=-76.367, y=44.55494, label = 'QBED1', colour = I('black'), size = 4) +
        annotate('text', x=-76.371, y=44.54990, label = 'QBED2', colour = I('black'), size = 4, fontface="bold") +
        annotate('text', x=-76.371, y=44.53525, label = 'QLL1', colour = I('black'), size = 4) + 
        annotate('text', x=-76.381, y=44.53817, label = 'QLL3', colour = I('black'), size = 4, fontface="bold") +
        annotate('text', x=-76.372, y=44.5219, label = 'QRL1', colour = I('black'), size = 4) +
        annotate('text', x=-76.372, y=44.520, label = 'QRL2', colour = I('black'), size = 4) +
        theme(text=element_text(size=12),axis.text=element_text(size=12), axis.line = element_line(colour = "black"))

ggsave(A_canadensis_7_pop_map, file="A_canadensis_7_pop_map.pdf", width=4, height=4,dpi=300)


