# Clear workspace
rm(list=ls(all=TRUE))

# Load packages
library(ggsci)

# Load data
dat<-read.csv("Data//Figure_2_LCR_Weirs_Efficiency_dat.csv")

#--------------------------------------------------------------------------------------------------------------------------
# Format data
concrete_dat<-dat[which(dat$Concrete==1),]
concrete_dat1<-concrete_dat[,c("X2.5.","X25.","X50.","X75.","X97.5.")]
tconcrete_dat1<-t(concrete_dat1)
colnames(tconcrete_dat1)<-paste(concrete_dat$Year,concrete_dat$Weir_Site,sep = " ")

no_concrete_dat<-dat[which(dat$Concrete==0),]
no_concrete_dat1<-no_concrete_dat[,c("X2.5.","X25.","X50.","X75.","X97.5.")]
tno_concrete_dat1<-t(no_concrete_dat1)
colnames(tno_concrete_dat1)<-paste(no_concrete_dat$Year,no_concrete_dat$Weir_Site,sep = " ")

# Set up color palettes
sites<-unique(dat$Weir_Site)
greytrans=adjustcolor("grey30",alpha.f=0.3)
colors <- pal_jama(alpha = 0.9)(length(sites))      
colors1 <- colors[1:3]
colors2 <- colors[4:6]

#---------------------------------------------------------------------------------------------------
# Plot

#Save figure to PNG file
png(filename = "Figure_2_LCR_Weirs_Efficiency.png", 
    bg="white",
    width     = 11,
    height    = 8.5,
    units     = "in",
    res       = 300
)

#margin for plotting region
par(mar=c(0.5,0.25,0.5,0.25),
    oma=c(5,5,0.25,0.25),
    mfrow=c(2,1)
)

#Permanent Infrastructure
boxplot( tconcrete_dat1,
         data=tconcrete_dat1,
         at=c(1:3,6:8,11:13,16:18,21:23,26:28,31:33,36:38,41:43,46:48),
         ylim=c(0,1),
         xaxs = "i",
         yaxs = "i",
         staplewex=1.05,
         boxlwd=0.9,
         medlwd=1,
         whisklwd=1,
         outlwd=1,
         staplelwd=1,
         xaxt="n",
         yaxt="n",
         main="",
         ylab="",
         xlab="",
         frame=F,
         range=0,
         col=colors1
        )
axis(1,at=seq(-0.5,49.5,5),
     labels=NA, 
     cex.axis=0.9,las=1)
axis(2,at=seq(0,1,by=0.2),
     labels=seq(0,100,by=20),
     las=2,cex.axis=0.9)
title(outer=F,main="Permanent Infrastructure",col="black",line=-17,adj=0.015,cex.main=0.9,font.main=1)
box(which = "plot", bty = "l")
legend("topleft",
       legend=unique(concrete_dat$Weir_Site),
       fill=unique(colors1),cex=0.9,bty="n")

abline(v=c(4.5,9.5,14.5,19.5,24.5,29.5,34.5,39.5,44.5),lty=2,col=greytrans)

#Temporary Infrastructure
boxplot( tno_concrete_dat1,
         data=tno_concrete_dat1,
         at=c(1:3,6:8,11:13,16:18,21:23,26:28,31:33,36:38,41:43,46:48),
         ylim=c(0,1),
         xaxs = "i",
         yaxs = "i",
         staplewex=1.05,
         boxlwd=0.9,
         medlwd=1,
         whisklwd=1,
         outlwd=1,
         staplelwd=1,
         xaxt="n",
         yaxt="n",
         main="",
         ylab="",
         xlab="",
         frame=F,
         range=0,
         col=colors2
        )

title(outer=F,main="Temporary Infrastructure",col="black",line=-17,adj=0.015,cex.main=0.9,font.main=1)
box(which = "plot", bty = "l")
legend("topleft",
       legend=unique(no_concrete_dat$Weir_Site),
       fill=unique(colors2),cex=0.9,bty="n")

#add axis and format
axis(1,at=seq(-0.5,49.5,5),
     labels=NA, 
     cex.axis=0.9,las=1)
axis(1,at=seq(2,51,5),
     labels=seq(min(dat$Year),max(dat$Year),1), 
     lwd.tick=0, 
     cex.axis=0.9,las=1)

axis(2,at=seq(0,1,by=0.2),
     labels=seq(0,100,by=20),
     las=2,cex.axis=0.9
     )

abline(v=c(4.5,9.5,14.5,19.5,24.5,29.5,34.5,39.5,44.5),lty=2,col=greytrans)

mtext("Year", side=1, outer=TRUE, line=2.5)
mtext("Weir Efficiency (%)", side=2, outer=TRUE, line=3)

#Turn off the device to open png
dev.off()
