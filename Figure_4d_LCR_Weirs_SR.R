# Clear workspace
rm(list=ls(all=TRUE))

# List of required packages
packages <- c("tidyverse", "RColorBrewer", "viridis", "ggsci", "tidyr",
              "lemon", "scales", "MuMIn", "dataRetrieval", "lubridate", "gridExtra")

# Function to check and install packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)}}

# Apply the function to the list of packages
invisible(sapply(packages, check_and_install))

#---------------------------------------------------------------------------------------------------
# Function to get USGS flow data
get_USGS_data<-function(flow_site,flow_params,daily_or_instantaneous,begin_date,end_date){
  flow_params_lookup<-tibble(flow_params=c("discharge","stage_height"),
                             param_codes=c("00060","00065"))%>%
    right_join(tibble("flow_params"=flow_params))%>%
    dplyr::select(param_codes)%>%
    as.vector()%>%
    unlist()
  
  names_lookup_daily<-c("Date" = "date","Flow" ="flow_cfs","GH" ="stage_height")
  names_lookup_inst<-c("Date" ="date","Flow_Inst"="flow_cfs","GH_Inst" ="stage_height" )                    
  
  if(daily_or_instantaneous=="daily"){
    tdat<- readNWISdv(siteNumber=flow_site,
                      parameterCd = flow_params_lookup, 
                      startDate = begin_date,
                      endDate = end_date, 
                      statCd = "00003")%>%
      renameNWISColumns()%>%
      as_tibble()%>%
      rename_with(.fn = ~names_lookup_daily[.x], .cols = intersect(names(.), names(names_lookup_daily)))
  }else{
    tdat <- readNWISuv(siteNumber = flow_site,
                       parameterCd = flow_params_lookup,
                       startDate =begin_date,
                       endDate = end_date)%>%
      renameNWISColumns()%>%
      as_tibble()%>%
      rename_with(.fn = ~names_lookup_inst[.x], .cols = intersect(names(.), names(names_lookup_inst)))
  }
  return(tdat)
}
#---------------------------------------------------------------------------------------------------
# Get flow data
flow<-get_USGS_data(flow_site=14222500,flow_params="discharge",daily_or_instantaneous="daily",
                    begin_date="2005-09-01",end_date="2018-07-01"
)

datF<-flow%>%
  mutate(
    month=month(date),
    Spawn_Year=as.integer(ifelse(month>7,year(date),year(date)-1))
  )

# Group by week, find mean and max by week, and scale flow data
datF<-datF%>%
  dplyr::filter(month>8 | month < 5)%>%
  group_by(Spawn_Year)%>%
  mutate(max=max(flow_cfs,na.rm=T),
         week=week(date)
  )%>%
  ungroup()%>%
  group_by(week,Spawn_Year)%>%
  mutate(mean_week=mean(flow_cfs))%>%
  ungroup()%>%
  group_by(Spawn_Year)%>%
  summarise(max=first(max),
            max_week = max(mean_week) 
  )%>%
  mutate(across(c("max","max_week"), ~scale(.)[,1]))

dat<-data.frame(read.csv("Data/Juv_Coweeman_CK.csv"))%>%
  mutate(logRS=log(CK_SY/CK_adult_AW))%>%
  left_join(datF)

# Generalized Linear Models (GLM)
mod1<-glm(logRS~ CK_adult_AW + max ,data=dat)                  #constant I and S w/ flow (max)
mod2<-glm(logRS~ CK_adult_AW : as.factor(Weir) + max,data=dat) #constant intercept and variable slope w/ flow (max)
mod3<-glm(logRS~ CK_adult_AW + as.factor(Weir) + max,data=dat) #variable intercept and constant slope w/ flow (max)
mod4<-glm(logRS~ CK_adult_AW * as.factor(Weir) + max,data=dat) #variable intercept and slope w/ flow (max)
mod5<-glm(logRS~ CK_adult_AW,data=dat)                         #constant I and S
mod6<-glm(logRS~ CK_adult_AW : as.factor(Weir),data=dat)       #constant intercept and variable slope
mod7<-glm(logRS~ CK_adult_AW + as.factor(Weir),data=dat)       #variable intercept and constant slope
mod8<-glm(logRS~ CK_adult_AW * as.factor(Weir),data=dat)       #variable intercept and slope
results<-model.sel(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)

# Develop avg model
mod<-model.avg(results,fit=T)
mod$fitted.values<-predict(mod,newdata=NULL,se.fit = T)$fit
mod$residuals<-mod$fitted.values-mod4$y

# Develop predictions based on avg model
newdat<-expand.grid("CK_adult_AW"=seq(0,2500,1),"Weir"=c(0,1),"max"=c(0))
newdat$pred<-predict(mod,newdata = newdat,se.fit = T)$fit
newdat$CK_SY<-exp(newdat$pred+log(newdat$CK_adult_AW))

#--------------------------------------------------------------------------------

# Plot
p <- ggplot() +
  geom_line(data = newdat, aes(x = CK_adult_AW, y = CK_SY, color = factor(Weir))) +
  scale_color_lancet(name = "", labels = c("Pre Weir", "Post Weir")) +
  labs(x = "Adult Spawner Abundance Above Weir", y = "Natural-Origin Subyearling Recruits") +
  geom_point(data = dat, aes(x = CK_adult_AW, y = CK_SY, color = factor(Weir)),
             size = 3, inherit.aes = FALSE, show.legend = TRUE, na.rm = TRUE) +
  annotate("text", x = dat$CK_adult_AW, y = dat$CK_SY, label = dat$Spawn_Year, size = 3, vjust = 2.0) +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'White'),
    axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    text = element_text(size = 12),
    legend.key = element_blank()
    ) +
  scale_x_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, 120000), expand = c(0, 0))

# Save the plot
ggsave(p, file = "Figure_4d_LCR_Weirs_SR.png", width = 8.5, height = 5.5, units = "in", dpi = 400)
