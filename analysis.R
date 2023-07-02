
# Install readxl R package - do this only once

#install.packages("readxl")


# open libraries

library(readxl)
library(tidyverse)
library(data.table)
library(ggplot2)


# Read data from xl spreadsheet

safety_data <- read_excel("safety data - Dr Apurna Ghosh.xlsx")

# convert date column to date rather than text format using as.Date
# and create additional columns using mutate -(from library tidyverse)
safety_data <- safety_data %>% 
  mutate(
    Date =as.Date(Date,"%d/%m/%Y"),
    month=month(Date),
         year=year(Date))

#
# quick look to see if employee numbers are unique
#

multiple <- safety_data %>%
  group_by(EmployeeID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

print(paste0("There are ",nrow(multiple)," EmployeeID's with multiple events"))
#
# Produce a monthly trend in total events as a scatter plot
#

# Wrangle the data 
# Create a new column of month start dates using as.Date() and paste0() functions
# group the data in groups of months based on month_start column
# produce a new column n_events containing the total number of events using n().
# other common sumarising functions are mean(x , na.rm=TRUE/FALSE), sd(x , na.rm=TRUE/FALSE) -Standard deviation, max(x , na.rm=TRUE/FALSE),
# min(x , na.rm=TRUE/FALSE),quantile(x,prob=0.9)
safety_data <- safety_data %>% 
  mutate(month_start=as.Date(paste0(year,"-",month,"-",01)))
trend <- safety_data %>% 
  group_by(month_start) %>% 
  summarise(n_events=n()) 



# create a basic plot variable using ggplot
p <- trend %>% ggplot(aes(x=month_start,y=n_events))+
  geom_point()
# Display it
plot(p)
# improve the chart with some labels
p <- p +
  labs(title = "Trend of Monthly Events",
       subtitle = "all events",
       x="Date",
       y="Number of Events")
plot(p)
# add a default trend line using geom_smooth

p1 <- p+
  geom_smooth(span=.08)
plot(p1)

# change the look pf the chart
# Change colour of line
p1 <- p+
  geom_smooth(span=.08,colour="blue")
plot(p1)

# remove error region
p1 <- p+
  geom_smooth(span=.08,colour="blue",se=FALSE)
plot(p1)

# Change the colour and intensity of shading
p1 <- p+
  geom_smooth(span=.08,colour="blue",fill="blue",alpha=0.5)
plot(p1)
# Change the colour and intensity of shading
p1 <- p+
  geom_smooth(span=.08,colour="blue",fill="blue",alpha=0.3)
plot(p1)

# This is a great visualisation method as the trend is a central estimate, many trending processes such as 
# rolling averages tend to lag the data lets add a typical 12 month rolling mean to the plot.
# to do this we need to create an additional column and install the "zoo" package for rolling statistics
#install.packages("zoo")

# lets now add a 3 month moving average to the data
library(zoo)
trend <- trend %>% 
  mutate(moving_average=rollmean(n_events,k=3,fill=NA))

p2 <- p1 +
  geom_line(data=trend,aes(x=month_start,y=moving_average),colour="red")
p2

# now plot multiple views on the same chart 
trend <- safety_data %>% 
  mutate(month_start=as.Date(paste0(year,"-",month,"-",01))) %>% 
  group_by(month_start,FullOrPartTime) %>% 
  summarise(n_events=n()) 
p <- trend %>% ggplot(aes(x=month_start,y=n_events,colour=FullOrPartTime,fill=FullOrPartTime))+
  geom_point()
p <- p +
  labs(title = "Trend of Monthly Events",
       subtitle = "all events",
       x="Date",
       y="Number of Events")
print(p)
p1 <- p +
  geom_smooth()
print(p1)


top_10_body_parts <- safety_data %>% 
  mutate(BodyPart=as.character(BodyPart)) %>% 
  group_by(BodyPart) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  slice(1:10)


top_10_body_part_data <- right_join(safety_data,top_10_body_parts)
top_10_body_part_data$BodyPart <- factor(top_10_body_part_data$BodyPart,
                                         levels=top_10_body_parts$BodyPart)

top_10_body_part_trend <- top_10_body_part_data %>% 
  group_by(month_start,BodyPart) %>% 
  summarise(n_events=n()) 

p <- top_10_body_part_trend %>% ggplot(aes(x=month_start,y=n_events,colour=BodyPart,fill=BodyPart))+
  geom_point()
p <- p +
  labs(title = "Trend of Monthly Events",
       subtitle = "Grouped by body part",
       x="Date",
       y="Number of Events")
print(p)
p1 <- p +
  geom_smooth()
print(p1)

p <- top_10_body_part_trend %>% 
  ggplot(aes(x=month_start,y=n_events,colour=BodyPart)) +
  geom_point() +
  facet_wrap(facets = vars(BodyPart))
print(p)
p <- top_10_body_part_trend %>% 
  ggplot(aes(x=month_start,y=n_events,colour=BodyPart)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(facets = vars(BodyPart),scales = "fixed")
print(p)

## Pareto

top_10_body_parts$BodyPart <-
  factor(top_10_body_parts$BodyPart ,
         levels = top_10_body_parts$BodyPart [order(-top_10_body_parts$n)])


p <- top_10_body_parts %>% ggplot(aes(x=BodyPart,y=n),alpha=0.3)+
  geom_col()
print(p)
p <- p+
  theme(axis.text.x = element_text(
    # rotates the x axis labels
    angle = 90,
    hjust = 1,
    vjust = 0.5
  )) 
print(p)



top_10_body_part_annual_trend <- top_10_body_part_data %>% 
  group_by(year,BodyPart) %>% 
  summarise(n_events=n()) 

p <- top_10_body_part_annual_trend %>%
  ggplot(aes(x = BodyPart, y = n_events, fill = BodyPart), alpha = 0.3) +
  geom_col() +
  theme(legend.position = "none",
        axis.text.x = element_text(
          # rotates the x axis labels
          angle = 90,
          hjust = 1,
          vjust = 0.5
        )) +
  facet_wrap(facets = vars(year))
p
