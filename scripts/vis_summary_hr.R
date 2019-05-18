## --- Fitbit -----------------------------------------------------------------------------------------------------------------------------------------------------------##
##
## Script name: vis_summary_hr
##
## Purpose of script: Make some visualizations of the summarized heart rate data
##
## Author: Luc Bams
##
## Date Created: 2019-04-19
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

##### start: MAKE DATA RETRIEVAL FUNCTIONS AVAILABLE ######################################################################################################################
source('~/R/Projects/fitbit/scripts/data_collection.R', encoding = 'UTF-8')
##### end:   MAKE DATA RETRIEVAL FUNCTIONS AVAILABLE ######################################################################################################################

##### start: LOAD PACKAGES ################################################################################################################################################
library(ggridges)
library(scales)
##### end:   LOAD PACKAGES ################################################################################################################################################

##### start: PLOTS ########################################################################################################################################################
##----- start: RESTING HEART RATE ---------------------------------------------------------------------------------------------------------------------------------------##
hr_summary %>% 
  filter(zone == "Out of Range") %>% 
  select(date, rest_hr) %>%
  ggplot(aes(x = date, y = rest_hr)) +
  geom_point() +
  geom_line()
##----- end:   RESTING HEART RATE ---------------------------------------------------------------------------------------------------------------------------------------##

##----- start: SINGLE DAY OF HEART RATE DATA ----------------------------------------------------------------------------------------------------------------------------##
hr_intraday %>% 
  filter(as.Date(datetime, tz = "Europe/Amsterdam") > (today() - 2)) %>%
  group_by(datetime = cut(datetime, breaks = "1 min")) %>%
  summarize(hr = round(mean(hr))) %>%
  ungroup() %>%
  mutate(datetime = as.POSIXct(datetime, tz = "Europe/Amsterdam")) %>%
  ggplot(aes(x = datetime, y = hr, color = hr)) +
  scale_color_gradientn(colours = c("green", "yellow", "red")) +
  geom_line()

# hr_intraday %>%
#   filter(as.Date(as.character(datetime)) == as.Date("2019-04-11")) %>%
#   filter(hr >= 35) %>%
#   group_by(datetime = as.POSIXct(cut(datetime, breaks = "5 mins"))) %>%
#   summarize(hr = round(mean(hr))) %>%
#   ungroup() %>%
#   ggplot(aes(x = datetime, y = hr, group = 1)) +
#   geom_point() +
#   geom_line()

hr_intraday %>%
  mutate(week = fct_rev(as.factor(week))) %>%
  ggplot(aes(x= hr, y = week)) +
  geom_density_ridges(scale = 2.5, alpha = 0.7) +
  xlim(30, 190) +
  theme_ridges()

# hr_summary %>%
#   mutate(week = fct_rev(as.factor(week))) %>% 
#   ggplot(aes(x= rest_hr, y = week)) +
#   geom_density_ridges(scale = 2.5, alpha = 0.7) +
#   xlim(40, 60) +
#   theme_ridges()

# hr_summary %>%
#   filter(zone != "Out of Range") %>%
#   ggplot(aes(fill = zone, y = minutes, x = date)) + 
#   geom_bar(stat = "identity")
##----- end:   SINGLE DAY OF HEART RATE DATA ----------------------------------------------------------------------------------------------------------------------------##

# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(RColorBrewer)

temp <- hr_intraday %>%
  filter(as.Date(datetime, tz = "Europe/Amsterdam") > (today() - 2))

# Then you can create the xts format, and thus use dygraph
don = xts(x = temp$hr, order.by = temp$datetime, tzone = "Europe/Amsterdam")
dygraph(don) %>%
  dyOptions(labelsUTC = FALSE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

# Trying to do some cool stuff with spline(). Not sure whether I would actually want to show heart rate data like this.
temp <- ml_int %>% 
  filter(as.character(as.Date(datetime, tz = "Europe/Amsterdam")) == "2019-05-16") %>% 
  filter(type == "heartrate")

spline_int <- as.data.frame(spline(temp$datetime, temp$value))
temp %>% 
  ggplot(aes(x = datetime, y = value)) + geom_point() + 
  geom_line(data = spline_int, aes(x = as.POSIXct(x, tz = "Europe/Amsterdam", origin = "1970-01-01 00:00:02"), y = y))
##### end:   PLOTS ########################################################################################################################################################