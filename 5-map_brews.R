source("1-setup.R")
options(warn=-1)


bp <- read.csv("brew_pubs_final.csv",stringsAsFactors=FALSE)
bp.city <- read.csv("brew_pubs_city.csv",stringsAsFactors=FALSE)

# ggmap version
map <- suppressMessages(get_map(location = 'colorado',messaging=FALSE,zoom = 7,maptype="roadmap"))

brew_map_1 <- ggmap(map, extent = 'device') + geom_point(aes(x = lon,y = lat)
                                           ,data = bp,color="blue") +
  xlab("") + ylab("")

# map of brewery locations
brew_map_1
ggsave(brew_map_1,file="micro_brew_map.png")

ggmap(map, extent = 'device') + geom_point(aes(x = lon,y = lat,size=bp.city$pub.count)
                                           ,data = bp.city,color="red") +
  xlab("") + ylab("")

# list brew pubs
bp[order(bp$city),c(2,4)]

bp <- read.csv("brew_pubs_city.csv",stringsAsFactors=FALSE)

map <- suppressMessages(get_map(location = 'colorado',zoom = 7,maptype="roadmap"))
b <- ggmap(map, extent = 'device') + geom_point(aes(x = lon,y = lat,size=bp.city$pub.count)
                                                ,data = bp.city,color="red")
b <- b + labs(size="Microbrewery\nand Brew Pub Count") + xlab("") + ylab("")
#b
suppressMessages(b)
