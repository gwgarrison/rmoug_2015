# exporatory plots
source("1-setup.R")


# load Lahman baseball data into tbl_df data frames
b <- tbl_df(Batting)
f <- tbl_df(Fielding)
t <- tbl_df(Teams)

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
g <- g + geom_smooth(color = "green") 
g

# join batting and fielding datasets to get postion
b <- b %>%
  filter(yearID > 1918) %>%
  select(playerID:H,HR:SO) %>%
  group_by(playerID,yearID) %>%
  summarize(games = sum(G),
            ab = sum(AB), r = sum(R), h = sum(H), hr = sum(HR), rbi = sum(RBI),
            sb = sum(SB), cs = sum(CS), bb = sum(BB), so = sum(SO)) %>%
  arrange(playerID,yearID)
  

