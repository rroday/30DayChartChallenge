## ---------------------------
##
## Script name: Day1
##
## Author: Rachel Roday
##
## Date Created: 2021-04-01
##
## Copyright (c) Rachel Roday, 2021
## Email: rroday@udel.edu
##
## ---------------------------
##
## Notes: Time to Code: 30 mintues
##
## ---------------------------

# Set wd
setwd("C:/Users/RER/Desktop/DataMonth21")

library(ggplot2)

Books <- c("Fantasy","SciFi","Biographical","Spark Notes","Distopian","Fiction","Poetry","Non-Fiction","Mystery","Science")
Count <- c(3,14,6,2,8,28,2,5,4,12)
data<-as.data.frame(cbind(Books,Count))
data$Count <- as.numeric(data$Count)


# Compute percentages
data$fraction = data$Count / sum(data$Count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$Books, ": ", data$Count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Books)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Books), size=4,colour = "Black") +
  scale_fill_brewer(palette ="Paired")+
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  labs(caption = "Books on my shelf | @rachel_roday")+
  theme(plot.caption = element_text(color = "darkgrey"))
ggsave("Day1.jpeg")


