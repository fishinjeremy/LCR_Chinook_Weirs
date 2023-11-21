# Clear workspace
rm(list=ls(all=TRUE))

# List of required packages
packages <- c("tidyverse", "RColorBrewer", "viridis", "ggsci", "lemon", "scales", "MuMIn", 
              "cowplot", "reshape", "reshape2", "sp", "sf", "rgdal", "lemon")

# Function to check and install packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)}}

# Apply the function to the list of packages
invisible(sapply(packages, check_and_install))

#---------------------------------------------------------------------------------------------------

# Read in the shapefile
R5_StreamLayer <- readOGR("Data/R5_StreamLayer/StevesStr24k_NAD83_SPS_HARN_Ft.shp")

# Read in redd and weir location data
dat3<-read.csv('Data/Cow_Redds.csv')
dat4<-read.csv('Data/Cow_Weir_Locations.csv')

# Convert stream layer from Washington State Plane South to WGS 1984 (same as redd waypoints)
R5_StreamLayer <- spTransform(R5_StreamLayer, CRS("+init=epsg:4326"))

# Generate a unique ID for each polygon
R5_StreamLayer@data$seq_id <- seq(1:nrow(R5_StreamLayer@data))

# Fortify and merge spatial data
R5_StreamLayer@data$id <- rownames(R5_StreamLayer@data)
R5_StreamLayer_data <- fortify(R5_StreamLayer, region = "id")
R5_StreamLayer_DF <- merge(R5_StreamLayer_data, R5_StreamLayer@data, by = "id")

# Generate random numbers from a uniform distribution
R5_StreamLayer@data$continuous_var <- runif(nrow(R5_StreamLayer@data))

# Filter specific stream names
streams_to_keep <- c("Coweeman River", "Mulholland Creek", "Goble Creek", "Baird Creek", 
                     "North Fork Goble Creek", "South Fork Goble Creek")
R5_StreamLayer_DF2 <- R5_StreamLayer_DF %>%
  filter(STRM_NAME %in% streams_to_keep)

# Create a column for weir installation status
dat3$Weir<-ifelse(dat3$Year<2011,"Pre Weir","Post Weir")

# Reorder dataframe
neworder <- c("Pre Weir","Post Weir")
dat3 <- arrange(transform(dat3,Weir=factor(Weir,levels=neworder)),Weir)

#---------------------------------------------------------------------------------------------------
# Plot
p1 <- ggplot() +
  geom_path(data = R5_StreamLayer_DF2, aes(x = long, y = lat, group = group), color = "#6699cc", linewidth = 0.5) +
  stat_density_2d(data = dat3, aes(x = Longitude, y = Latitude, fill = after_stat(level)),
                  geom = "polygon", bins = 8, alpha = 0.8) +
  geom_point(data=dat4[dat4$YearInstalled==2011,],aes(x=Long, y=Lat,color=factor(YearInstalled)),
                size=3,pch=19)+ #Weir Location
  scale_fill_distiller(name = "No. Redds", palette = "Reds", direction = 1, trans = 'log10') +
  scale_color_manual(name = "", labels = c("Weir Site"), values = c("black")) +
  facet_rep_wrap(~Weir, ncol = 1, repeat.tick.labels = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  annotate("text", x = -122.825, y = 46.160, label = "Coweeman River", size = 3) +
  annotate("text", x = -122.655, y = 46.215, label = "Mulholland Creek", size = 3) +
  annotate("text", x = -122.555, y = 46.200, label = "Baird Creek", size = 3) +
  annotate("text", x = -122.650, y = 46.145, label = "NF Goble Creek", size = 3) +
  annotate("text", x = -122.700, y = 46.120, label = "Goble Creek", size = 3) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        strip.placement = "outside",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        text = element_text(size = 12),
        legend.key = element_blank())+  
  guides(color=guide_legend(override.aes=list(fill="white",linetype=0)))

# Save
ggsave(p1, file = "Figure_4a_LCR_Weirs_Redd_Map.png", width = 8.5, height = 5.5,
       units = "in", dpi = 400)

