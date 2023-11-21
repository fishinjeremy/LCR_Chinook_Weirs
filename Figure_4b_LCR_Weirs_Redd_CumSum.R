# Clear workspace
rm(list=ls(all=TRUE))

# List of required packages
packages <- c("tidyr", "tidyverse", "scales")

# Function to check and install packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)}}

# Apply the function to the list of packages
invisible(sapply(packages, check_and_install))

#----------------------------------------------------------------------------------

# Pull in CSV
DF<-data.frame(read.csv("Data//Cow_Redds.csv"))

# Create a column for weir installed or weir not installed
DF$Weir<-ifelse(DF$Year<2011,0,1)

# Deal with tributary rm/rkm
# Baird enters Coweeman R. at rm 25.8
# Goble enters Coweeman R. at rm 11.0
# Mulholland enters Coweeman R. at rm 18.3
# NF Goble enter Goble at rm 1.7 Goble + Goble enters Coweeman at rm 11.0 = 12.7
# SF Goble enters Goble at rm 4.0 Goble + Goble enters Coweeman at rm 11.0 = 15.0

DF$Final_RM<-ifelse(DF$Stream=="Baird Creek",(DF$rm+25.8),
                ifelse(DF$Stream=="Goble Creek",(DF$rm+11.0),
                  ifelse(DF$Stream=="Mulholland Creek",(DF$rm+18.3),
                     ifelse(DF$Stream=="NF Goble Creek",(DF$rm+12.7),
                        ifelse(DF$Stream=="SF Goble Creek",(DF$rm+15.0),DF$rm)))))
DF$Final_RKM<-(DF$Final_RM*1.60934)

#---------------------------------------------------------------------------------------------------

#sort by spawn year then redd river kilometer
DF<-DF[order(DF$Year,DF$Final_RKM),] 

#add cumsum by spawn year
DF<-DF %>%
     group_by(Year) %>%
     mutate(cumsum = cumsum(Redd_Count))
DF$sum<-ave(DF$Redd_Count,DF$Year,FUN=sum)
DF[,"p_cumsum"] <- DF$cumsum/DF$sum

# Create seperate DF for year with weirs and years without
DF1<-DF[DF$Weir==0,]
DF2<-DF[DF$Weir==1,]

#-------------------------------------------------------------------------------
# Plot
p<-ggplot() +
     geom_line(data=DF1,aes(x=Final_RKM, y=p_cumsum*100, group=factor(Year),color="#00468BFF")) +
     geom_line(data=DF2,aes(x=Final_RKM, y=p_cumsum*100, group=factor(Year),color="#ED0000FF")) +
     scale_color_manual(name="",labels=c("Pre Weir","Post Weir"),values=c("#00468BFF","#ED0000FF"))+
     labs(x="River Kilometer",y="Cumulative Percentage of Redds")+
     annotate("text",x=c(  28,  42.5,  42.5, 33.5,  31,    28,      28,  22,  28,  28,  28,  22,   28),
                     y=c(56,   87.75,  90.5, 68.5,  66,  43.25,     79,  79,  47,  70,  82,  86,  73.5),
                 label=c(2003,  2004,  2005, 2008,2009,  2010,    2011,2012,2013,2014,2015,2016, 2017), size=3)+
     theme(axis.line=element_line(colour="black",linewidth=0.5, linetype="solid"),
           axis.text.x=element_text(color="black"),
           axis.text.y=element_text(color="black"),
           text=element_text(size=12),
           legend.key = element_blank(),
           panel.background = element_rect(fill = 'white', colour = 'White')
           )+
     scale_x_continuous( breaks=pretty_breaks(),expand=c(0,0))+
     scale_y_continuous( breaks=pretty_breaks(),expand=c(0,0))

# Save
ggsave(p, file="Figure_4b_LCR_Weirs_Redd_CumSum.png", width=8.5,height=5.5,
       units="in", dpi=400)