## --- Fitbit -----------------------------------------------------------------------------------------------------------------------------------------------------------##
##
## Script name: analysis_most_active_day_of_the_week
##
## Purpose of script: To provide visualizations that help form an answer to the question: "What day of the week was my most most active?"
##
## Author: Luc Bams
##
## Date Created: 2019-05-02
##
## Copyright (c) Luc Bams, 2019
## Email: (private) luc-bams@hotmail.com
##
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------##
##
## Notes:
##   
##
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------##
##
## Set working directory
##
setwd("~/R/Projects/fitbit/")    # Luc's working directory (private)
##
##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------##

##### start: MAKE SURE LATEST DATA IS AVAILABLE ###########################################################################################################################
source('~/R/Projects/fitbit/scripts/data_collection.R', encoding = 'UTF-8')
##### end:   MAKE SURE LATEST DATA IS AVAILABLE ###########################################################################################################################

##### start: LOAD PACKAGES ################################################################################################################################################
library(ggridges)
library(scales)
##### end:   LOAD PACKAGES ################################################################################################################################################

##### start: ANALYSIS #####################################################################################################################################################
##----- start: DATA MANIPULATION ----------------------------------------------------------------------------------------------------------------------------------------##
data <- list()
for(type in c("calories", "distance", "floors", "steps")) {
  data[[type]] <- act_data[[type]] %>% 
    select_(.dots = list("datetime", type, "day", "week")) %>%
    mutate(type = type) %>%
    rename(value = !!type) %>%
    select(datetime, type, value, day, week)
  data[[type]]$value <- as.double(data[[type]]$value)
}

data$hr <- hr_intraday %>%
  select(datetime, hr, day, week) %>%
  mutate(type = "hr") %>%
  rename(value = hr) %>%
  select(datetime, type, value, day, week)
data$hr$value <- as.double(data$hr$value)

data_tidy <- rbind(data$calories, data$distance, data$floors, data$steps, data$hr)
data_tidy$day <- factor(data_tidy$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=TRUE)

remove(data)
##----- end:   DATA MANIPULATION ----------------------------------------------------------------------------------------------------------------------------------------##

##----- start: PLOTTING -------------------------------------------------------------------------------------------------------------------------------------------------##
data_tidy %>% 
  filter(type == "hr") %>%
  ggplot(aes(x= value, y = fct_rev(day))) +
  geom_density_ridges(scale = 2.5) +
  facet_grid(.~type, scales = "free") +
  theme_ridges() + 
  labs(x = '')

data_tidy %>% 
  filter((type != "hr") & (!is.na(datetime))) %>%
  mutate(datetime = as.Date(datetime, tz = "Europe/Amsterdam")) %>%
  group_by(type, datetime) %>%
  summarize(value = sum(value), day = max(day), week = max(week)) %>%
  select(datetime, type, value, day, week) %>%
  ggplot(aes(x= value, y = fct_rev(day))) +
  geom_density_ridges(alpha = 0.7, scale = 2.5) +
  facet_grid(.~type, scales = "free") +
  theme_ridges() + 
  labs(x = '')
##----- end:   PLOTTING -------------------------------------------------------------------------------------------------------------------------------------------------##
##### end:   ANALYSIS #####################################################################################################################################################