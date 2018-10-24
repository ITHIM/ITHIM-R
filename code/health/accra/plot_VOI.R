library(RColorBrewer)
library(plotrix)

SCEN_SHORT_NAME <- c("base",paste0("scen", 1:5) )

file_name <- paste0('six_by_five_scenarios_1024.Rds')
ithim_object_list <- readRDS(file_name)
evppi <- ithim_object_list$uncertain$now$evppi


x11(); par(mar=c(6,20,3.5,5))
labs <- rownames(evppi)
get.pal=colorRampPalette(brewer.pal(9,"Reds"))
redCol=rev(get.pal(12))
bkT <- seq(max(evppi)+1e-10, 0,length=13) 
cex.lab <- 1.5
maxval <- round(bkT[1],digits=1)
col.labels<- c(0,maxval/2,maxval)
cellcolors <- vector()
for(ii in 1:length(unlist(evppi)))
  cellcolors[ii] <- redCol[tail(which(unlist(evppi[ii])<bkT),n=1)]
color2D.matplot(evppi,cellcolors=cellcolors,main="",xlab="",ylab="",cex.lab=2,axes=F)
fullaxis(side=1,las=2,at=0:4+0.5,labels=SCEN_SHORT_NAME[-2],line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=1)
fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=labs,line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.8)
mtext(3,text='By how much (%) could we reduce uncertainty in\n the outcome if we knew this parameter perfectly?',line=1)
color.legend(5.5,0,5.5+0.3,length(labs),col.labels,rev(redCol),gradient="y",cex=1,align="rb")

evppi.m <- reshape2::melt(evppi)
p <- evppi.m %>% ggplot(aes(Var1, Var2, fill = value)) + geom_tile() + coord_flip() +
 scale_fill_gradient2(low = "orange", high = "red", limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
p
ggplotly(p)
