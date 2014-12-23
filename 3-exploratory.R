# exporatory plots
source("1-setup.R")
# put the diamonds dataset into dplyr table
d <- tbl_df(diamonds)

# histogram for price
g <- ggplot(data = d,aes(x = price)) + geom_histogram(fill = "blue") + xlab("Price") +
  ylab("Record Count") + ggtitle("Histogram of Diamond Price")
g

# lets add some facets
g <- ggplot(data = d,aes(x = price)) + geom_histogram() + xlab("Price") +
  ylab("Record Count") + ggtitle("Histogram of Diamond Price") + 
  facet_wrap(~ cut, ncol = 2)
g

# a box plot showing cut
g <- ggplot(data = d, aes(clarity,price, color = clarity )) + geom_boxplot()
g
#qplot(cut,price, data = d,geom="boxplot",color = cut)  


# basic scatter plot
ggplot(data = d,aes(x = carat,y = price)) + geom_point(color = "orange") +
  ggtitle("Price by Carat")

# give it some color and use alpha to make points more transparent
ggplot(data = d,aes(x = carat,y = price)) + 
  geom_point(color = "blue",alpha = .2) 

# add a regression line (presentation)
g <- ggplot(data = d,aes(x = carat,y = price)) + 
  geom_point(color = "blue",alpha = .2) + geom_smooth(color = "orange") +
  ggtitle("Price by Carat with Regression Line")
g

summary(g)

# get some baseball data
b <- tbl_df(Batting)
f <- tbl_df(Fielding)
bf <- inner_join(b,select(f,playerID,POS),by = "playerID")
# add some color for 
ggplot(data = d,aes(x = carat,y = price, color = cut)) + geom_point()


# load the Houston flight data
h <- tbl_df(hflights)

# load airline data
a <- tbl_df(read.csv("L_UNIQUE_CARRIERS.csv",stringsAsFactors=FALSE))
names(a) <- c("UniqueCarrier","Airline")

h %>% group_by(Cancelled) %>% summarise(n())

# fix ExpressJet (duplicated in airlines)
h$UniqueCarrier <- gsub('XE','EV',h$UniqueCarrier)

# join to the get the Airline name
ha <- left_join(h,a,by = "UniqueCarrier")

# filter to get only cancelled flights
h_cancel <- filter(ha,Cancelled == 1)

g_all <- ggplot(ha,aes(Airline,fill=factor(Cancelled))) + 
  geom_bar() + coord_flip() +
  theme(legend.position="none") +
  scale_fill_manual(values=c("darkblue", "orange"))
#  scale_fill_discrete(name = "Cancellation Reason",
#                      labels = c("Carrier","weather",
#                                 "National Air System","Security")) 

g_cancel <- ggplot(h_cancel,aes(Airline,fill=CancellationCode)) + 
  geom_bar() + coord_flip() +
  scale_fill_discrete(name = "Cancellation Reason",
                      labels = c("Carrier","weather",
                                 "National Air System","Security")) 

g_all
g_cancel
grid.arrange(g_all,g_cancel,nrow = 1)
