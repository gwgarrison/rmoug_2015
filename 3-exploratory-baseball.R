# exporatory plots
source("1-setup.R")


# load Lahman baseball data into tbl_df data frames
b <- tbl_df(Batting)
f <- tbl_df(Fielding)
t <- tbl_df(Teams)
m <- tbl_df(Master)

# create data frame of batting data, adding position from the
# Fielding dataset
t.year <- t %>%
  group_by(yearID) %>%
  summarize(hr = sum(HR),
            games = sum(G)) %>%
  filter(yearID >= 1918)

t.year <- mutate(t.year,hr_game = hr/games)
names(t.year) <- c("year","hr","games","hr_game")

g <- ggplot(data = t.year,aes(x = year,y = hr_game)) + geom_point() +
  xlab("Year") + ylab("HR/Game") + 
  ggtitle("Homeruns Per/Game by Year") + 
  geom_vline(xintercept = 2003,color = "red")
g
g <- g +geom_smooth(method = 'lm') 
g
g <- g + geom_smooth(method = 'lm') 
g
# change the geom to a line
gline <- ggplot(data = t.year,aes(x = year,y = hr_game)) + geom_line() +
  xlab("Year") + ylab("HR/Game") + 
  ggtitle("Homeruns Per/Game by Year") +
  geom_vline(xintercept = 2003,color = "red")
gline 



# join batting and fielding datasets to get postion
b <- b %>%
  filter(yearID > 1918) %>%
  select(playerID:H,HR:SO) %>%
  group_by(playerID,yearID) %>%
  summarize(games = sum(G),
            ab = sum(AB), r = sum(R), h = sum(H), hr = sum(HR), rbi = sum(RBI),
            sb = sum(SB), cs = sum(CS), bb = sum(BB), so = sum(SO)) %>%
  arrange(playerID,yearID)
  
# get state data
s <- tbl_df(map_data("state"))

data(state.fips)

mb <- m %>%
#  filter(birthYear < 1930) %>%
  group_by(birthState) %>%
  summarize(state_count = n()) %>%
  arrange(birthState)

sf <- tbl_df(state.fips)
sf$polyname <- gsub(":main","",sf$polyname)

players.state <- inner_join(mb,sf,by = c("birthState" = "abb"))

players.state.loc <- inner_join(players.state,s,by = c("polyname" = "region"))

ggplot( data = players.state.loc, aes( x = long, y = lat, 
                                   group = group, fill = state_count)) + 
  geom_polygon( colour ="black") + coord_map("polyconic") + theme_clean() +
  ggtitle("Number Baseball Players by State of Birth") +
  scale_fill_continuous("Player Count")


