library( maps);library(ggplot2);library( plyr) 
# For map data # Get map data for USA 
states_map <- map_data("state") 
# load cdc data
#load("C:/Users/Gary/Google Drive/Presentations_Papers/RMOUG 2015/training_days_2015/cdc.rda")
#load("/Volumes/KINGSTON/RMOUG 2015/training_days_2015/cdc.rda")
#setwd("/Volumes/KINGSTON/RMOUG 2015/training_days_2015")
load("cdc.rda")
source("theme_clean.R")
sw <- cdc[cdc$weight2 < 999,c("x.state","weight2")]
# get average weight for by state
swa <- ddply(sw,.(x.state),summarize,avg.weight = mean(weight2),median.weight = median(weight2))
names(swa) <- c("fips","avg.weight","median.weight")

# merge state weight data with fips
sf <- data(state.fips)
sf <- state.fips[,c("fips","polyname")]
sf$polyname <- as.character(sf$polyname)
sf$polyname[grep('massachusetts:',sf$polyname)] <- 'massachusetts'
sf$polyname[grep('michigan:',sf$polyname)] <- 'michigan'
sf$polyname[grep('new york:',sf$polyname)] <- 'new york'
sf$polyname[grep('north carolina:',sf$polyname)] <- 'north carolina'
sf$polyname[grep('virginia:',sf$polyname)] <- 'virginia'
sf$polyname[grep('washington:',sf$polyname)] <- 'washington'
sf <- unique(sf)
names(sf) <- c("fips","state")
swa <- unique(merge(swa,sf))
weight_map<- merge(states_map,swa,by.x="region",by.y="state")
ggplot( weight_map, aes( x = long, y = lat, group = group, fill = avg.weight )) + 
  geom_polygon( colour ="black") + coord_map("polyconic") + ggtitle("Weight Map") +
  xlab("") + ylab("") + theme_clean() + scale_color_brewer(type="seq", palette=2)
#theme(panel.background = element_blank() ,panel.grid = element_blank(),
  #                              axis.text = element_blank(),axis.ticks.length = unit (0,"cm"))
swa[order(swa$avg.weight),c(3,2,4)]

