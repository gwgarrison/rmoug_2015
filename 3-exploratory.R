# exporatory plots
source("1-setup.R")
# put the diamonds dataset into dplyr table
d <- tbl_df(diamonds)

### histograms
# histogram for price
g <- ggplot(data = d,aes(x = price)) + geom_histogram(fill = "blue",binwidth=100) + xlab("Price") +
  ylab("Record Count") + ggtitle("Histogram of Diamond Price") 
g

# lets limit the x-axis so we can see the clump of data
g +  xlim(c(0,4000))


# lets add some facets
g <- ggplot(data = d,aes(x = price)) + geom_histogram() + xlab("Price") +
  ylab("Record Count") + ggtitle("Histogram of Diamond Price") + 
  facet_wrap(~ cut, ncol = 2)
g

# add a fill which is diamond color, note we did not have to add a legend ggplot
# takes care of this for us
g <- ggplot(data = d,aes(x = price,fill = color)) + geom_histogram() + xlab("Price") +
  ylab("Record Count") + ggtitle("Histogram of Diamond Price") + 
  facet_wrap(~ cut, ncol = 2)
g

### boxplots
# a box plot showing clarity, oddly the diamonds with better clarity seem to be cheaper
g <- ggplot(data = d, aes(clarity,price, color = clarity )) + geom_boxplot() + 
  ggtitle("Diamond Price by Clarity")
g

# like clarity the diamonds with a better cut (Ideal) seem cheaper
g2 <- ggplot(data = d, aes(x = cut, price,color = cut)) + geom_boxplot() +
  ggtitle("Diamond Price by Cut") 
g2

# add a line for the mean price of all diamonds
g2 + geom_hline(y = mean(d$price),linetype = "dashed")
g2 + geom_hline(y = median(d$price),linetype = "dotted",color = "red")
#qplot(cut,price, data = d,geom="boxplot",color = cut)  


### scatter plots
# basic scatter plot
ggplot(data = d,aes(x = carat,y = price)) + geom_point(color = "orange") +
  ggtitle("Price by Carat")

# give it some color and use alpha to make points more transparent
ggplot(data = d,aes(x = carat,y = price)) + 
  geom_point(color = "blue",alpha = .2) + ggtitle("Price by Carat") 

# add a smoother line (presentation)
g <- ggplot(data = d,aes(x = carat,y = price)) + 
  geom_point(color = "blue",alpha = .2) + geom_smooth(color = "orange",size = 2) +
  ggtitle("Price by Carat with Smoother Line")
g

# add some color to see a third variables
gc <- ggplot(data = d,aes(x = carat,y = price, color = cut)) + 
  geom_point() + scale_color_brewer(palette = "Set1")
gc

# plot with log10 scale
g.log10 <- gc + scale_x_log10() + scale_y_log10() +
  scale_color_brewer(palette = "Set3")
g.log10

summary(g)



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
  geom_bar()+  theme(legend.position="none") + xlab("Flight Count") +
  scale_fill_manual(values=c("darkblue", "orange"))

# nice plot, but the airline names are unreadable
g_all

# fixed
g_fixed <- g_all + coord_flip() + ylab("Flight Count") + xlab("Airline")
g_fixed

g_cancel <- ggplot(h_cancel,aes(Airline,fill=CancellationCode)) + 
  xlab("") + ylab("") +
  geom_bar() + coord_flip() +
  scale_fill_discrete(name = "Cancellation Reason",
                      labels = c("Carrier","Weather",
                                 "National Air System","Security")) 

g_cancel
grid.arrange(g_fixed,g_cancel,nrow = 1)

