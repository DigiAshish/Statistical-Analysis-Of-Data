library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)

america <- map_data("state") # extract state wise data from raster library
colnames(america)[5] <- "State" # rename column

setwd("/Users/AshishMAC/Documents/Courses/17-Spring/Stats/Mini Project/Project-2/"); # set working direcotory
election <- read.table("us_2016_election_data.csv", header = T, sep = ",") # extract data from csv
election$State <- tolower(election$State) # change to lower case names so that they can be matched

americanElections <- join(election, america, by="State", type="inner") # join the state info table and election information

americanElections$difference <- americanElections$Trump - americanElections$Clinton
# this contains difference between Trump and Clinton. If the value is positive, Trump won. If is is negative, it means that Clinton won. More postive the value, darker the red color on the map. More negative, darker blue on the map.

states <- data.frame(state.center, state.abb) # put the abbreviation at the center of each states
subset <- tolower(state.name) %in% election$State # exclude Hawaii and Alaska as there is no data for this state
states <- states[subset, ]

summary(americanElections) # shows the highest and lowest difference between the two candidates. We see that it is Clinton : -86.8 and Trump: +46.3

plotElections <- function(data, brks, title){
  #ggplot_diff <- ggplot() + geom_polygon(data = data, aes(x = long, y = lat,fill = americanElections$difference, group = group), color = "black", size = 0.15) + theme_nothing(legend = TRUE) + labs(title = title, fill = "Trump vs Clinton") + geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
  ggplot_diff <- ggplot() + geom_polygon(data = data, aes(x = long, y = lat,fill = difference, group = group), color = "black", size = 0.15) + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", limits = c(-86.8, 46.3)) + theme_nothing(legend = TRUE) + labs(title = title, fill = "Trump vs Clinton") + geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
  ggplot_diff
  }
# low (blue) for Clinton. high(red) for Trump. 

brks.to.use <- seq(-86.8, 86.8, by = 5) # the highest margin Hillary won over Trump was -86.8% and the the highest margin Trump won over Hillary was 46.3%. Since the we need to maintain fairness in colors becoming darker with larger margin on both sides, we choose -86.8 to +86.8

figure.title <- "2016 Presidential Elections"

ggsave(plotElections(americanElections, brks.to.use, figure.title), height = 4, width = 4*1.9, file = "us_2016_election.jpg")
