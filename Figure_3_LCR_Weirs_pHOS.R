# Clear workspace
rm(list=ls(all=TRUE))

# Load data
dat<-read.csv("Data//Figure_3_LCR_Weirs_pHOS_dat.csv")

# Load packages
library(RColorBrewer)
library(tidyverse) 
library(lemon)
library(scales)
#--------------------------------------------------------------------------------------------------------------------------
# Subset to only columns needed
dat1<-subset(dat,select = -c(NOS_Removals,HOS_Removals))

# Convert from wide to long format
dat2 <- gather(dat1, Location, Estimate, AW_HOS:Non_Weir_Subpop_NOS, factor_key=TRUE)

# Calculate sum and cumsum by year
dat2$comb=paste(dat2$Pop,dat2$Year) #combine mark and spawn year for cum sum
dat2$sum<-ave(dat2$Estimate,dat2$comb,FUN=sum)
dat2[,"p"] <- dat2$Estimate/dat2$sum 

# Set up color palettes
colors1 <- brewer.pal(3,"OrRd")
colors2 <- brewer.pal(3,"PuBuGn")
colors1<-adjustcolor(colors1,alpha=0.9)
colors2<-adjustcolor(colors2,alpha=0.7)
colors3<-c(colors2,colors1)

# Reorder location and populations for figure
dat2$Location <- factor(dat2$Location, levels= c("Non_Weir_Subpop_NOS","BW_NOS","AW_NOS",
                                                 "Non_Weir_Subpop_HOS","BW_HOS","AW_HOS"))
dat2$Pop <- factor(dat2$Pop, levels= c("Elochoman, Skamokawa","Toutle","Kalama",
                                       "Grays, Chinook","Coweeman","Washougal"))
#--------------------------------------------------------------------------------------------------------------------------

# Generate plot
p <- ggplot(dat2, aes(x = Year, y = p * 100, fill = Location)) + 
  geom_area() +
  facet_rep_wrap(~Pop, ncol = 3, repeat.tick.labels = FALSE) +
  scale_fill_manual(values = c(colors3), labels = c(
    "Non weir subpopulation NOS", "Below weir NOS", "Above weir NOS",
    "Non weir subpopulation HOS", "Below weir HOS", "Above weir HOS"
  )) +
  geom_hline(aes(yintercept = pHOS_Goal_HSRG * 100, color = "HSRG pHOS goal", linetype = "HSRG pHOS goal", linewidth = "HSRG pHOS goal")) +
  geom_line(aes(x = Year, y = Pop_pHOS * 100, color = "pHOS", linetype = "pHOS", linewidth = "pHOS")) +
  scale_color_manual(name = "", values = c("pHOS" = "black", "HSRG pHOS goal" = "black"), guide = guide_legend(reverse = TRUE)) +
  scale_linetype_manual(name = "", values = c("pHOS" = 1, "HSRG pHOS goal" = 2), guide = guide_legend(reverse = TRUE)) +
  scale_linewidth_manual(name = "", values = c("pHOS" = 1.75, "HSRG pHOS goal" = 0.8), guide = guide_legend(reverse = TRUE)) +
  labs(title = "", x = "Year", y = "Contribution of Spawners by Location and Origin (%)", fill = "") +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'White'),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(colour = "black"),
    strip.placement = "outside",
    axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.key = element_blank()
  ) +
  scale_x_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0))

# Save the plot
ggsave(p, file = "Figure_3_LCR_Weirs_pHOS.png", width = 11, height = 8.5, units = "in", dpi = 300)

