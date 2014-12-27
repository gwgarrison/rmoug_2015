# exporatory plots
source("1-setup.R")


# load the Houston flight and airport data into dplyr table, get rid of some unwanted 
# variables with select function
h <- tbl_df(select(hflights,-Year))
ap <- tbl_df(select(airports,faa:alt))

h.ap <- inner_join(h,ap,by = c("Dest" = "faa"))

ggplot(data = h.ap,aes(x = lon,y = lat)) + geom_point() + borders("state")

# get rid of Alaska and Hawaii to make our map look better
ggplot(data = filter(h.ap,lon > -140),aes(x = lon,y = lat)) + geom_point(color = "red") + 
  borders("state") + ggtitle("Destination Airports")

# aggregate houston flight and destination airport data
h.agg <- h.ap %>%
  group_by(Dest,name,lat,lon) %>%
  summarize(flight_count = n(),
            mean_delay = mean(ArrDelay,na.rm = TRUE),
            median_delay = median(ArrDelay,na.rm = TRUE)) %>%
  arrange(name)

h.agg$flight_quant <- cut(h.agg$flight_count,breaks = quantile(h.agg$flight_count),
                          labels = c(1,2,3,4))
            
h1 <- filter(h.agg,lon > -140 & flight_count > 1)

h.busy <- as.data.frame(h1) %>% filter(flight_count > quantile(flight_count,.9) ) %>%
  arrange(flight_count)
  
 

# plot destination airports with point size determined by 
# how busy the destination
g <- ggplot(data = h1,aes(x = lon,y = lat)) + 
  geom_point(aes(size = flight_quant),color = "blue") + 
  borders("state") + ggtitle("Destination Airports") + theme_clean() +
  scale_size_discrete(name = "Busiest Destinations") 
g

g.busy <-  g +  geom_text(data = h.busy,aes(x = lon + 2,y = lat +.5,label = name))
g.busy
