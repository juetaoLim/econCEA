rm(list=ls())
library(RColorBrewer)
load("~/cea/data/outCEA.Rdata")
deaths <- read.csv("~/cea/data/deaths.csv")
plotdf1 <- table(dftemp)
plotdf1 <- plotdf1[,-c(1:4)]

#first plot of dengue burdens#
pdf("~/cea/plots/fig1.pdf",height=6,width=10)
par(las=1,oma=c(1,4,1,4))
cols <- colorRampPalette(brewer.pal(8, "RdYlBu"))(ncol(plotdf1))
cols <- rev(cols)
barplot(plotdf1, main="",
        xlab="", 
        col=cols,
        axes = F,
        ylim=c(0,37000),
        border=NA)
legend(x="topleft",legend=rev(c("<6",
                            "6-11",
                            "12-13",
                            "14-17",
                            "18-24",
                            "25-34",
                            "35-44",
                            "45-54",
                            "55-64",
                            "65+")),pch=15,col=rev(cols),bty='n')
deathByYear <- colSums(table(deaths))
deathByYear <- deathByYear[-1]
points(x=seq(0.7,13,by=1.2),y=deathByYear*500,pch=16,col="darkseagreen4")
lines(x=seq(0.7,13,by=1.2),y=deathByYear*500,lty=2,col="darkseagreen4")

axis(side=2,col="black",at=seq(0,35000,by=5000),labels=seq(0,35000,by=5000)/1000)
axis(side=4,col="darkseagreen4",col.axis="darkseagreen4",
     at=seq(0,max(deathByYear*1000),by=max(deathByYear*1000)/5),
            labels=round(2*seq(0,max(deathByYear),by=max(deathByYear)/5)))
box()
text("Case Counts (000s)",x=-1.5,y=max(colSums(plotdf1))/2,col="black",xpd=NA,srt=90,cex=1.5)
text("Deaths",x=15,y=max(colSums(plotdf1))/2,col="darkseagreen4",xpd=NA,srt=270,cex=1.5)
# abline(v=seq(0.7,30,by=1.2)) # check margins
dev.off()

load("~/cea/output/cea.RData")
store1 <- array(as.numeric(unlist(store1)), dim=c(dim(store1[[1]]), length(store1)))
store2 <- array(as.numeric(unlist(store2)), dim=c(dim(store2[[1]]), length(store2)))

means1 <- apply(store1,MARGIN=c(1,2),mean)
means2 <- apply(store2,MARGIN=c(1,2),mean)

cols1 <- colorRampPalette(brewer.pal(8, "Blues"))(nrow(means1))
cols2 <- colorRampPalette(brewer.pal(8, "Reds"))(nrow(means1))
pdf("~/cea/plots/fig2.pdf",height=6,width=10)
layout(matrix(c(1,2,2,
                3,4,4), 2, 3, byrow = TRUE))
par(las=1,cex.lab=1.2,cex.axis=1.2,oma=c(0,5,0,0))
barplot(means1[,1:2],horiz=T,col=c(cols1),border=NA,names.arg=c("Total Direct","Total Indirect\n(Human Capital)"),xlim=c(0,1.3),xlab="USD (Billions)")
barplot(as.matrix(means1[,1]), offset=as.matrix(means1[,1]),border=NA, add=T, axes=F, axisnames=F, horiz=T, col=c(cols2))

abline(v=colSums(means1[,1:2]),col="grey",lty=2)
mtext("A",side=3,adj=0,padj=-1,cex=1.2)
box()
barplot(t(means1[,1:2]),col=c("#D93F41","#102F6C"),border=NA,names.arg=seq(10,20),ylim=c(0,0.5),ylab="USD (Billions)")
legend(x="topleft",legend=c("Direct","Indirect (Human Capital)"),pch=15,col=c("#D93F41","#102F6C"),bty='n',cex=1.5)
mtext("B",side=3,adj=0,padj=-1,cex=1.2)
box()
barplot(means2[,1:2],horiz=T,col=c(cols1),border=NA,names.arg=c("Total Direct","Total Indirect\n(Friction Cost)"),xlim=c(0,1.3),xlab="USD (Billions)")
barplot(as.matrix(means2[,1]), offset=as.matrix(means2[,1]),border=NA, add=T, axes=F, axisnames=F, horiz=T, col=c(cols2))

abline(v=colSums(means2[,1:2]),col="grey",lty=2)
mtext("C",side=3,adj=0,padj=-1,cex=1.2)
box()
barplot(t(means2[,1:2]),col=c("#D93F41","#102F6C"),border=NA,names.arg=seq(10,20),ylim=c(0,0.5),ylab="USD (Billions)")
legend(x="topleft",legend=c("Direct","Indirect (Friction Cost)"),pch=15,col=c("#D93F41","#102F6C"),bty='n',cex=1.5)
mtext("D",side=3,adj=0,padj=-1,cex=1.2)
box()
dev.off()

#figure for dalys 
load("~/cea/stacy/DalysBreakdown.RData")
outBreakdown <- lapply(outBreakdown,function(x)x/1000)
outBreakdown <- lapply(outBreakdown,function(x)x[c(2,3,4,1),])
for (i in 1:length(outBreakdown)){colnames(outBreakdown[[i]]) <- c(10:20)}
pdf("~/cea/plots/fig3.pdf",height=6,width=10)


par(las=1,mfrow=c(2,2),mar=c(3.1,4.1,3.1,1.1))
barplot(outBreakdown[[1]],col=c("pink","cadetblue3","#D93F41","#102F6C"),border=NA,ylim=c(0,5.5),ylab="DALYs (000s)")
box()
mtext("A: Age Dependent DW, Constant Expansion",side=3,adj=0,padj=-1,cex=0.9)

barplot(outBreakdown[[2]],col=c("pink","cadetblue3","#D93F41","#102F6C"),border=NA,ylim=c(0,5.5))
box()
mtext("B: Age Dependent DW, Age Dependent Expansion",side=3,adj=0,padj=-1,cex=0.9)

barplot(outBreakdown[[3]],col=c("pink","cadetblue3","#D93F41","#102F6C"),border=NA,ylim=c(0,5.5),ylab="DALYs (000s)")
box()
mtext("C: Constant DW, Constant Expansion",side=3,adj=0,padj=-1,cex=0.9)

barplot(outBreakdown[[4]],col=c("pink","cadetblue3","#D93F41","#102F6C"),border=NA,ylim=c(0,5.5))
box()
mtext("D: Constant DW, Age Dependent Expansion",side=3,adj=0,padj=-1,cex=0.9)
legend(x="topleft",
       legend=rownames(outBreakdown[[1]]),
       pch=15,
       col=c("pink","cadetblue3","#D93F41","#102F6C"),
       bty='n',
       cex=1)
dev.off()