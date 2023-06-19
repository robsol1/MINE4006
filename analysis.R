#install.packages("xlsx")
#install.packages("readxl")                                       # Install readxl R package
library(readxl)
library(tidyverse)
library(data.table)
library(ggplot2)

safety_data <- read_excel("safety data - Dr Apurna Ghosh.xlsx")
safety_data$Date <- as.Date(safety_data$Date, "%d/%m/%Y")
safety_data <- safety_data %>% 
  mutate(
    Date =as.Date(Date,"%d/%m/%Y"),
    month=month(Date),
         year=year(Date))

multiple <- safety_data %>%
  group_by(EmployeeID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

trend <- safety_data %>% 
  mutate(month_start=as.Date(paste0(year,"-",month,"-",01))) %>% 
  group_by(month_start) %>% 
  summarise(n_events=n()) %>% 
  filter(!is.na(month_start))

trend %>% ggplot(aes(x=month_start,y=n_events))+
  geom_point()+
  geom_smooth()
    