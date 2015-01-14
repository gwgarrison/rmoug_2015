source("1-setup.R")
# For map data # Get map data for USA 
states_map <- map_data("state") 
# load cdc data
load("cdc.rda")
sw <- cdc[cdc$weight2 < 999,c("x.state","weight2")]
# get average weight for by state
#swa <- ddply(sw,.(x.state),summarize,avg.weight = mean(weight2),median.weight = median(weight2))
swa <- sw %>% group_by(x.state) %>%
              summarize(avg.weight = mean(weight2),
                        median.weight = median(weight2))

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

#weight_map<- merge(states_map,swa,by.x="region",by.y="state")
weight_map <- tbl_df(inner_join(states_map,swa,by = c("region" = "state")))

ggplot( weight_map, aes( x = long, y = lat, group = group, fill = avg.weight )) + 
  geom_polygon( colour ="black") + coord_map("polyconic") + 
  ggtitle("Weight Map for the United States") +
  xlab("") + ylab("") + theme_clean() + 
  theme(plot.title=element_text(family="Times", face="bold", size=20)) +
  scale_fill_continuous("Average Weight",low="green",high="black")
#  scale_fill_brewer("Average Weight")
  #scale_color_brewer(type="seq", palette=2) + 

swa[order(swa$avg.weight),c(3,2,4)]

