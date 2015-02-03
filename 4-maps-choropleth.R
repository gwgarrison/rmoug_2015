source("1-setup.R")
# For map data # Get map data for USA 
states_map <- map_data("state") 

# load cdc data
system.time(load("cdc.rda"))
system.time(cdc <- tbl_df(cdc))

sw <- cdc[cdc$weight2 < 999,c("x.state","weight2")]
# get average weight for by state
#swa <- ddply(sw,.(x.state),summarize,avg.weight = mean(weight2),median.weight = median(weight2))
swa <- sw %>% group_by(x.state) %>%
              summarize(avg.weight = mean(weight2),
                        median.weight = median(weight2))

swa <- tbl_df(swa)
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
  scale_fill_continuous("Average Weight",low="red",high="black")
#  scale_fill_brewer("Average Weight")
  #scale_color_brewer(type="seq", palette=2) + 

swa[order(swa$avg.weight),c(3,2,4)]

######
#choroplethr demo
library(choroplethr);library(acs);library(choroplethrMaps);
library(zipcode);
inc <- choroplethr_acs(tableId="B19301",map = "state",buckets = 5)
inc
choroplethr_acs(tableId="B20004",map = "state",buckets = 5)
choroplethr_acs(tableId="B01003",map = "state",buckets = 7)
choroplethr_acs(tableId="B01003",map = "county",zoom = "colorado", buckets = 7)
choroplethr_acs(tableId="B19301",map = "county",buckets = 5)

choroplethr_acs(tableId="B19301",map = "county",zoom = "colorado",buckets = 4)

data(county.fips, package="maps")
df = data.frame(region=county.fips$fips, value=sample(100, nrow(county.fips), replace=TRUE))
county_choropleth(df, buckets=2)
data(df_pop_county)
data(df_president)
county_choropleth(df_pop_county, title="US 2012 County Population Estimates", legend="Population")
pres <- state_choropleth(df_president,title="US Presidential Election Results")  + 
  scale_fill_manual(name="Candidate", values=c("blue", "red"), drop=FALSE)
pres
choro = StateChoropleth$new(df_president)
choro$title = "2012 Election Results"
choro$ggplot_scale = scale_fill_manual(name="Candidate", values=c("blue", "red"), drop=FALSE)
choro$render()
grid.arrange(inc,pres)

# past presidential elections
data(df_president_ts)
election_year <- "1988"
dp <- tbl_df(select(df_president_ts,region,contains(election_year)))
names(dp) <- c("region","value")
choro.p = StateChoropleth$new(dp)
choro.p$title = paste(election_year,"Election Results")
choro.p$ggplot_scale = scale_fill_manual(name="Candidate", values=c("blue", "red","green"), drop=FALSE)
choro.p$render()


choro <- choro_president("1976")
suppressWarnings(choro$render())
