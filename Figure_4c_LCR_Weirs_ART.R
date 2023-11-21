# Clear workspace
rm(list=ls(all=TRUE))

# List of required packages
packages <- c("tidyverse", "ggsci","scales")

# Function to check and install packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)}}

# Apply the function to the list of packages
invisible(sapply(packages, check_and_install))

#---------------------------------------------------------------------------------------------------

# Call in data
dat<-read.csv("Data//Cow_ART.csv")

# Create a column for whether weir was installed or not
dat$Weir<-ifelse(dat$Year<2011,0,1)

#---------------------------------------------------------------------------------------------------
# Plot
p<-(ggplot(data=dat))+
     geom_errorbar(aes(x=Year,ymin=X2.5., ymax=X97.5.))+
     geom_boxplot(aes(x=Year, ymin=X2.5., lower=X25., middle= X50., upper=X75.,
                      ymax=X97.5., fill=factor(Weir),group=Year),stat="identity")+
     labs(x="Year",y="Apparent Residence Time (days)")+
     scale_fill_lancet (name="",labels=c("Pre Weir","Post Weir"))+
     theme(panel.background = element_rect(fill = 'white', colour = 'White'),
           axis.line=element_line(colour="black",linewidth=0.5, linetype="solid"),
           axis.text.x=element_text(color="black"),
           axis.text.y=element_text(color="black"),
           text=element_text(size=12),
           legend.key = element_blank()   
           )+
     scale_x_continuous(breaks=pretty_breaks())+
     scale_y_continuous(breaks=pretty_breaks())

# Save
ggsave(p, file="Figure_4c_LCR_Weirs_ART.png", width=8.5,height=5.5,
       units="in", dpi=400)