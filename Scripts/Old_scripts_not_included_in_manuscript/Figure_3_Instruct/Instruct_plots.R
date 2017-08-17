#Bar plots of assignment probabilities for K= 2 clusters and K= 11  
#A single plot with two panels is created.
rm(list=ls())

#Instruct (Gao et al 2007) was used to establish optimal cluster number
#ClUMPP (Jakobsson and Rosenberg 2007) was used to obtain a single table of mean individual assignment probababilities for K=12 and K=11.
#It is the CLUMPP output that is used as input data in this script.
#Although the CLUMPP input and output files are housed in the CLUMPP folder, a copy of the CLUMPP output files (used as input for this script)
#are housed in this folder along with this script and figures it produced.

instructdat_k11=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/acanadensis_K11.indq", header=FALSE, stringsAsFactors = FALSE)
head(instructdat_k11)

instructdat_k2=read.table("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/acanadensis_K2.indq", header=FALSE, stringsAsFactors = FALSE)
head(instructdat_k2)

#pop order in indstruct
pop_order=c("QBED1-1","QBED1-2","QBED2-1","QBED2-2","QLL1-1","QLL1-2","QLL3-1","QLL3-2","QRL1-1","QRL1-2","QRL2-1","QRL2-2","QTUR-1","QTUR-2")
instructdat_k2$plot=pop_order

#geographic order
geog_order=c("QBED1-1","QBED1-2","QBED2-1", "QBED2-2", "QTUR-1", "QTUR-2", "QLL3-1", "QLL3-2", "QLL1-1", "QLL1-2", "QRL1-1", "QRL1-2", "QRL2-1", "QRL2-2")


#sort the instruct data according to Latitude
instructdat_k2=instructdat_k2[order(instructdat_k2$plot),]

#Plotting:
quartz(width=6.76,height=8, family="Helvetica")
#postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.eps",  bg = "transparent", family="Helvetica",width=6.76,height=8, horizontal=FALSE,pointsize = 12)
#postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.eps", width=3.38, height=10,  horizontal=FALSE, family="Helvetica", pointsize = 12, bg = "transparent")

par(family="Helvetica")
#par(mfrow=c(2,1)) 
par(mfrow=c(2,1), mai = c(0.5,1,0.5,0.9), oma=c(3,3,3,3))

barplot(t(as.matrix(instructdat_k2[,6:7])), beside=FALSE, border=NA, space=0,col=c("cornflowerblue", "lightcoral"),axes=FALSE, xaxs="i",xaxt='n')
for(i in seq(20,260, 20)){
        abline(v=i, lwd=2)
        }
k=10
for(i in seq(1,14)){
        par(las=2)
        mtext(geog_order[i], side=3.5, line=0.1, at=k, cex=0.75)
        k=k+20
        }
par(las=1)

axis(2,at=seq(0,1,0.2),labels=c("0.0", "0.2", "0.4","0.6", "0.8", "1.0"), las=1, cex.axis=1, mgp=c(3,0.7,0))
par(las=0)

k2_text=expression(italic('K')*"=2")

mtext(side=2, line=3, cex=1, "Coancestry")
mtext(side=1, line=0.5, cex=1, k2_text, adj=0,padj=0)



#rainbow_colours=rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
#rainbow_colours[]
my_colours=c( "mediumseagreen","lightskyblue", "lightcoral","cornflowerblue",        "#fdae61",        "#fee08b",        "orchid",        "#e6f598",        "#abdda4",        "lightcyan","lavender")

barplot(t(as.matrix(instructdat_k11[,6:16])), beside=FALSE, border=NA, space=0,col=my_colours,las=1,axes=FALSE, xaxs="i",xaxt='n')
for(i in seq(20,260, 20)){
        abline(v=i, lwd=2)}
k=10

for(i in seq(1,14)){
        par(las=2)
        mtext(geog_order[i], side=3.5, line=0.1, at=k, cex=0.75)
        k=k+20
}

axis(2,at=seq(0,1,0.2),labels=c("0.0", "0.2", "0.4","0.6", "0.8", "1.0"), las=1, cex.axis=1, mgp=c(3,0.7,0))
par(las=0)

k11_text=expression(italic('K')*"=11")

mtext(side=2, line=3, cex=1, "Coancestry")
mtext(side=1, line=0.5, cex=1, k11_text, adj=0,padj=0)

quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.tiff", type = "tiff", device = dev.cur(),dpi =300,width=10, height=10, pointsize = 14, bg="white")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.jpg", type = "jpg", device = dev.cur(),  dpi =300,width=10, height=10, pointsize = 14, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.png", type = "png", device = dev.cur(),dpi =300,width=10, height=10, pointsize = 14, bg="white")

dev.off()
dev.off()
dev.off()

#Plot with only K=2
quartz(width=3.38,height=3.38, family="Helvetica")
postscript(file="/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.eps",  bg = "transparent", family="Helvetica",width=6.76,height=8, horizontal=FALSE,pointsize = 12)

par(family="Helvetica")
par(mar=c(3,4,3,0.5)+0.1)

barplot(t(as.matrix(instructdat_k2[,6:7])), beside=FALSE, border=NA, space=0,col=c("gray63", "gray90"),axes=FALSE, xaxs="i", xaxt='n')
for(i in seq(20,260, 20)){
        abline(v=i, lwd=2)
}
k=10
for(i in seq(1,14)){
        par(las=2)
        mtext(geog_order[i], side=3.5, line=0.1, at=k, cex=0.75)
        k=k+20
}
par(las=1)

axis(2,at=seq(0,1,0.2),labels=c("0.0", "0.2", "0.4","0.6", "0.8", "1.0"), las=1, cex.axis=1, mgp=c(3,0.7,0))
par(las=0)

k2_text=expression(italic('K')*"=2")

mtext(side=2, line=3, cex=1, "Coancestry")
mtext(side=1, line=0.5, cex=1, k2_text, adj=0,padj=0)

quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.tiff", type = "tiff", device = dev.cur(),dpi =300,width=10, height=10, pointsize = 14, bg="white")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.jpg", type = "jpg", device = dev.cur(),  dpi =300,width=10, height=10, pointsize = 14, bg="transparent")
quartz.save("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Figures/Figure_3_Instruct/Fig3_Instruct.png", type = "png", device = dev.cur(),dpi =300,width=10, height=10, pointsize = 14, bg="white")

dev.off()
dev.off()
dev.off()


#Plot with map and K=2
library(grid)
library(ggplot2)
library(gridBase)

#plotting:
quartz(width=3.38,height=7.38, family="Helvetica")

vp.Centre <- viewport(layout=grid.layout(3,4, widths=unit(c(5,1,2), c("lines", "null", "lines")), 
        heights=unit(c(5, 1, 4,1), c("lines", "null", "lines","null"))))

margin1 <- viewport(layout.pos.col = 1, layout.pos.row = 1,name = "margin1")
margin2 <- viewport(layout.pos.col = 1, layout.pos.row = 2,name = "margin2")
x <- runif(10)
y <- runif(10)

xscale <- extendrange(x)
yscale <- extendrange(y)

plot <- viewport(layout.pos.col = 1, layout.pos.row = 2,name = "plot", xscale = xscale, yscale = yscale)
splot <- vpTree(vp.Centre , vpList(margin1, margin2, plot))
pushViewport(splot)

#vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"),  just=c("left","top"), y=0.5, x=0.5)
#plot base graph
barplot(t(as.matrix(instructdat_k2[,6:7])), beside=FALSE, border=NA, space=0,col=c("gray63", "gray90"),axes=FALSE, xaxs="i", xaxt='n')
for(i in seq(20,260, 20)){
        abline(v=i, lwd=2)
}
k=10
for(i in seq(1,14)){
        par(las=2)
        mtext(geog_order[i], side=3.5, line=0.1, at=k, cex=0.75)
        k=k+20
}
par(las=1)

axis(2,at=seq(0,1,0.2),labels=c("0.0", "0.2", "0.4","0.6", "0.8", "1.0"), las=1, cex.axis=1, mgp=c(3,0.7,0))
par(las=0)

k2_text=expression(italic('K')*"=2")

mtext(side=2, line=3, cex=1, "Coancestry")
mtext(side=1, line=0.5, cex=1, k2_text, adj=0,padj=0)

print(A_canadensis_7_pop_map, vp=vp.Centre)




#Plotting:
dev.off()
plot.new()
quartz(width=6.76,height=8, family="Helvetica")

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
pushViewport(viewport(layout.pos.col = 1,layout.pos.row = 1))
print(A_canadensis_7_pop_map, newpage = FALSE)
popViewport()

#Draw bsae plot
pushViewport(viewport(layout.pos.col = 1,layout.pos.row = 2))
par(fig = gridFIG(), new = FALSE)
barplot(t(as.matrix(instructdat_k2[,6:7])), beside=FALSE, border=NA, space=0,col=c("cornflowerblue", "lightcoral"),axes=FALSE, xaxs="i",xaxt='n')
popViewport()

par(family="Helvetica")
#par(mfrow=c(2,1)) 
par(mfrow=c(2,1), mai = c(0.5,1,0.5,0.9), oma=c(3,3,3,3))

barplot(t(as.matrix(instructdat_k2[,6:7])), beside=FALSE, border=NA, space=0,col=c("cornflowerblue", "lightcoral"),axes=FALSE, xaxs="i",xaxt='n')
for(i in seq(20,260, 20)){
        abline(v=i, lwd=2)
}
k=10
for(i in seq(1,14)){
        par(las=2)
        mtext(geog_order[i], side=3.5, line=0.1, at=k, cex=0.75)
        k=k+20
}
par(las=1)

axis(2,at=seq(0,1,0.2),labels=c("0.0", "0.2", "0.4","0.6", "0.8", "1.0"), las=1, cex.axis=1, mgp=c(3,0.7,0))
par(las=0)

k2_text=expression(italic('K')*"=2")

mtext(side=2, line=3, cex=1, "Coancestry")
mtext(side=1, line=0.5, cex=1, k2_text, adj=0,padj=0)

foo=t(as.matrix(instructdat_k2[,6:7]))
foo=instructdat_k2[,6:7]
x=foo$V6
x=as.data.frame(x)
x$sample=seq(1:280)

x2=foo$V7
x2=as.data.frame(x2)
x2$sample=seq(1:280)
colnames(x)=c("perc", "id")
colnames(x2)=c("perc", "id")

x3=rbind(x, x2)

x3$cluster[1:280]="V6"
x3$cluster[281:560]="V7"

head(x3)
cols <- c(V6="darkgrey",V7="lightgrey") 
ggplot(x3, aes(x = id, y = perc, fill = cluster)) + geom_bar(stat = "identity") 
        +scale_fill_manual(values = cols)  

fill <- c("#5F9EA0", "#E1B378")

g <- 

        ggplot(foo, aes(foo$V6))+geom_bar(aes(fill=row.names(foo)), position="fill")
g + geom_bar(aes(fill = drv))

ggplot(foo) + geom_bar(aes(y=foo$V6, x=row.names(foo)),data=foo,stat="identity")+ scale_fill_manual(values=fill)
        geom_text()
dfr <- data.frame(
        V1 = c(0.1, 0.2, 0.3),
        V2 = c(0.2, 0.3, 0.2),
        V3 = c(0.3, 0.6, 0.5),
        V4 = c(0.5, 0.1, 0.7),
        row.names = LETTERS[1:3]
)
down vote
accepted
First, some data manipulation. Add the category as a variable and melt the data to long format.

dfr$category <- row.names(dfr)
mdfr <- melt(dfr, id.vars = "category")