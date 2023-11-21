# Clear workspace
rm(list=ls(all=TRUE))

# List of required packages
packages <- c("tidyverse", "RColorBrewer", "ggpubr", "cowplot", "stringr", "patchwork")

# Function to check and install packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)}}

# Apply the function to the list of packages
invisible(sapply(packages, check_and_install))

#---------------------------------------------------------------------------------------------------

# Make function for pHOS data table
makepHOS<-function(m){
  m = m
  dat<-expand.grid(
    p=c(1:999/1000),
    w=c(1:999/1000),
    m = m
    )%>%
    as_tibble()%>%
    mutate(pHOS = ((p/m) - (p*w))/(1-p*w))
}
#---------------------------------------------------------------------------------------------------
#assign colors
color_range <- c(0, 0.05, 0.1, 0.3, 1.0)
color_palette <- rev(brewer.pal(c(length(color_range) - 1), "RdYlGn"))

#---------------------------------------------------------------------------------------------------
# Plot 1 (left pane - 97% MM rate)

# Make data
pHOSdat <- makepHOS(m = 0.97)
dat <- expand.grid(p = 1:999 / 1000, w = 1:999 / 1000, m = 0.97)
dat <- dat %>% mutate(pHOS = ((p / m) - (p * w)) / (1 - p * w))
pHOSdat_matrix <- matrix(findInterval(dat$pHOS, color_range, all.inside = TRUE), nrow = nrow(dat))

title <- paste("  97% Mass Mark Rate")

# Plot
p1<-dat %>%
  ggplot(aes(x = w, y = p, z = pHOS)) +
  geom_tile(aes(fill = factor(pHOSdat_matrix)), alpha = 0.35) +
  scale_fill_manual(values = color_palette,
                    labels = c("0-5%","6-10%","11-30%","31-100%"),
                    name = "Upstream Escapement pHOS",
                    guide = guide_legend(override.aes = list(shape = NA))) +
  ggtitle(title) +
  theme(
    plot.title = element_text(vjust = -1,hjust = 0, margin = margin(b = -10), size = 11),
    plot.margin = margin(5, 8, 5, 5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.2, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.key = element_rect(fill = "white")
    ) +
  labs(x = "",
       y = "pMark Arriving to Weir (%)",
       fill = "Upstream Escapement pHOS") +
  scale_y_continuous(labels = c("0","","20","","40","","60","",80,"","100"), breaks = seq(0, 1.0, 0.1),expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(labels = c("0","","20","","40","","60","",80,"","100"), breaks = seq(0, 1.0, 0.1),expand = c(0, 0), limits = c(0, 1)) 

#---------------------------------------------------------------------------------------------------
# Plot 2 (right pane - 100% MM rate)

# Make data
pHOSdat <- makepHOS(m = 1)
dat1 <- expand.grid(p = 1:999 / 1000, w = 1:999 / 1000, m = 1)
dat1 <- dat1 %>% mutate(pHOS = ((p / m) - (p * w)) / (1 - p * w))
pHOSdat_matrix1 <- matrix(findInterval(dat1$pHOS, color_range, all.inside = TRUE), nrow = nrow(dat))

title <- paste("  100% Mass Mark Rate")

# Plot
p2<-dat %>%
  ggplot(aes(x = w, y = p, z = pHOS)) +
  geom_tile(aes(fill = factor(pHOSdat_matrix1)), alpha = 0.35) +  # Set alpha value to 1 for full opacity
  scale_fill_manual(values = color_palette,
                    labels = c("0-5%","6-10%","11-30%","31-100%"),
                    name = "Upstream Escapement pHOS",
                    guide = guide_legend(override.aes = list(shape = NA))) +
  ggtitle(title) +
  theme(
    plot.title = element_text(vjust = -1,hjust = 0, margin = margin(b = -10), size = 11),
    plot.margin = margin(5, 5, 5, 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.2, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.key = element_rect(fill = "white")
    ) +
  labs(x = "",
       y = "",
       fill = "Upstream Escapement pHOS") +
  scale_y_continuous(labels = c("","","","","","","","","","",""), breaks = seq(0, 1.0, 0.1),expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(labels = c("0","","20","","40","","60","",80,"","100"), breaks = seq(0, 1.0, 0.1),expand = c(0, 0), limits = c(0, 1)) 


#---------------------------------------------------------------------------------------------------
# Combine 2 plots
plot<-ggpubr::ggarrange(p1, p2, # list of plots
                        labels = "AUTO", # labels
                        common.legend = T, #common legend
                        legend = "right", # legend position
                        align = "hv", # Align both plots, horizontal and vertical
                        nrow = 1)  # number of rows
plot <- add_sub(plot, "Weir Efficiency (%)", hjust = 1, vjust = -1)
#-------------------------------------------------------------------------------
# Save plot
ggsave(plot, file="Figure_5_Isoclines.png", width=7.5, height=5.5, bg="white",
       units="in", dpi=400)
