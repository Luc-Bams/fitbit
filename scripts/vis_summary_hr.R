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
# hr_intraday %>%
#   filter(as.Date(as.character(datetime)) == (Sys.Date() - 11)) %>%
#   group_by(datetime = cut(datetime, breaks = "5 min")) %>%
#   summarize(avg_hr = round(mean(hr))) %>%
#   ggplot(aes(x = datetime, y = avg_hr)) +
#   geom_line()

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
##### end:   PLOTS ########################################################################################################################################################