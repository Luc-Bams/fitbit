## --- Fitbit -----------------------------------------------------------------------------------------------------------------------------------------------------------##
##
## Script name: data_collection
##
## Purpose of script: Collect all data starting at 2019-01-01 and saving it in CSV files
##
## Author: Luc Bams
##
## Date Created: 2019-04-17
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
source('~/R/Projects/fitbit/scripts/data_collection_functions.R', encoding = 'UTF-8')
##### end:   MAKE DATA RETRIEVAL FUNCTIONS AVAILABLE ######################################################################################################################

##### start: LOAD PACKAGES ################################################################################################################################################
require(lubridate)
##### end:   LOAD PACKAGES ################################################################################################################################################

##### start: COLLECT DATA #################################################################################################################################################
##----- start: INITIAL COLLECTION ---------------------------------------------------------------------------------------------------------------------------------------##
# act_calories <- data.frame(datetime = as.POSIXct(character()),
#                            calories = double(),
#                            level = integer(),
#                            mets = integer())
# act_distance <- data.frame(datetime = as.POSIXct(character()),
#                            distance = double())
# act_steps <- data.frame(datetime = as.POSIXct(character()),
#                         steps = integer())
# act_data <- list(calories = act_calories,
#                  distance = act_distance,
#                  steps = act_steps)
# remove(act_calories, act_distance, act_steps)
#
# hr_summary <- data.frame(date = as.POSIXct(character()),
#                          rest_hr = integer(),
#                          zone = factor(),
#                          max_hr = integer(),
#                          min_hr = integer(),
#                          minutes = integer(),
#                          cal_out = integer())
# 
# hr_intraday <- data.frame(datetime = as.POSIXct(character()),
#                           hr = integer())
#
# weight <- data.frame(date = as.POSIXct(character()),
#                      weight = double(),
#                      bmi = double(),
#                      logId = double())
# 
# act_days <- list(calories = as.character(seq(as.Date(as.character("2019-01-01")), as.Date(Sys.Date() - 1), "days")),
#                  distance = as.character(seq(as.Date(as.character("2019-01-01")), as.Date(Sys.Date() - 1), "days")),
#                  steps = as.character(seq(as.Date(as.character("2019-01-01")), as.Date(Sys.Date() - 1), "days")))
# hr_days <- as.character(seq(as.Date(as.character("2019-01-01")), as.Date(Sys.Date() - 1), "days"))
# weight_days <- as.character(seq(as.Date(as.character("2019-01-01")), as.Date(Sys.Date() - 1), "days"))
##----- end:   INITIAL COLLECTION ---------------------------------------------------------------------------------------------------------------------------------------##

##----- start: SUBSEQUENT COLLECTION ------------------------------------------------------------------------------------------------------------------------------------##
act_data <- list(calories = read_csv("data/act_calories.csv", col_types = "?dii", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 distance = read_csv("data/act_distance.csv", col_types = "?d", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 elevation = read_csv("data/act_elevation.csv", col_types = "?d", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 floors = read_csv("data/act_floors.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 steps = read_csv("data/act_steps.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 minutesVeryActive = read_csv("data/act_minutesVeryActive.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 minutesLightlyActive = read_csv("data/act_minutesLightlyActive.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 minutesFairlyActive = read_csv("data/act_minutesFairlyActive.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 minutesSedentary = read_csv("data/act_minutesSedentary.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 caloriesBMR = read_csv("data/act_caloriesBMR.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")),
                 activityCalories = read_csv("data/act_activityCalories.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam")))
hr_summary <- read_csv("data/hr_summary.csv", col_types = "?ifiiii", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam"))
hr_intraday <- read_csv("data/hr_intraday.csv", col_types = "?i", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam"))
weight <- read_csv("data/weight.csv", col_types = "?ddd", locale = locale(date_names = date_names_lang("en"), tz = "Europe/Amsterdam"))

act_days <- list(calories = as.character(seq(as.Date(as.character(max(act_data[["calories"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 distance = as.character(seq(as.Date(as.character(max(act_data[["distance"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 elevation = as.character(seq(as.Date(as.character(max(act_data[["elevation"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 floors = as.character(seq(as.Date(as.character(max(act_data[["floors"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 steps = as.character(seq(as.Date(as.character(max(act_data[["steps"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 minutesVeryActive = as.character(seq(as.Date(as.character(max(act_data[["minutesVeryActive"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 minutesFairlyActive = as.character(seq(as.Date(as.character(max(act_data[["minutesFairlyActive"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 minutesLightlyActive = as.character(seq(as.Date(as.character(max(act_data[["minutesLightlyActive"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 minutesSedentary = as.character(seq(as.Date(as.character(max(act_data[["minutesSedentary"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 caloriesBMR = as.character(seq(as.Date(as.character(max(act_data[["caloriesBMR"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")),
                 activityCalories = as.character(seq(as.Date(as.character(max(act_data[["activityCalories"]]$datetime, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days")))
hr_days <- as.character(seq(as.Date(as.character(max(hr_summary$date))) + 1, as.Date(Sys.Date()), "days"))
weight_days <- as.character(seq(as.Date(as.character(max(weight$date, na.rm = TRUE))) + 1, as.Date(Sys.Date()), "days"))
##----- end:   SUBSEQUENT COLLECTION ------------------------------------------------------------------------------------------------------------------------------------##

##----- start: ACTIVITY -------------------------------------------------------------------------------------------------------------------------------------------------##
for(activity in c("calories", "distance", "elevation", "floors", "steps", "minutesVeryActive", "minutesFairlyActive", "minutesLightlyActive", "minutesSedentary", 
                  "caloriesBMR", "activityCalories")) {
  if(length(act_days[[activity]]) > 1) {
    for(day in act_days[[activity]]) {
      if(day != as.character(Sys.Date())) {
        temp <- get_activity(date = day, activity = activity)
        act_data[[activity]] <- rbind(act_data[[activity]], temp)
        print(paste0("Gathered ", activity, " data for ", day))
      }
    }
    
    write_csv(act_data[[activity]], path = paste0("data/act_", activity, ".csv"))
    
    remove(temp)
  }
  
  act_data[[activity]] <- act_data[[activity]] %>%
      mutate(day = lubridate::wday(datetime, label = TRUE, locale = "English_United States", abbr = FALSE),
             week = week(datetime))
}

remove(act_days, activity, day)
##----- end:   ACTIVITY -------------------------------------------------------------------------------------------------------------------------------------------------##

##----- start: HEART RATE -----------------------------------------------------------------------------------------------------------------------------------------------##
if(length(hr_days) > 1) {
  for(day in hr_days) {
    if(day != as.character(Sys.Date())) {
      temp <- get_hr(date = day, detail_level = "1sec")
      hr_summary <- rbind(hr_summary, temp[["summary"]])
      hr_intraday <- rbind(hr_intraday, temp[["intraday_hr"]])
      print(paste0("Gathered heart rate data for ", day))
    }
  }
  
  write_csv(hr_summary, path = "data/hr_summary.csv")
  write_csv(hr_intraday, path = "data/hr_intraday.csv")
  
  remove(temp)
}

hr_summary <- hr_summary %>%
  mutate(day = lubridate::wday(date, label = TRUE, locale = "English_United States", abbr = FALSE),
         week = week(date))

hr_intraday <- hr_intraday %>%
  mutate(day = lubridate::wday(datetime, label = TRUE, locale = "English_United States", abbr = FALSE),
         week = week(datetime))

remove(hr_days, day)
##----- end:   HEART RATE -----------------------------------------------------------------------------------------------------------------------------------------------##

##----- start: WEIGHT ---------------------------------------------------------------------------------------------------------------------------------------------------##
if(length(weight_days) > 1) {
  for(day in weight_days) {
    if(day != as.character(Sys.Date())) {
      temp <- get_weight(date = day)
      if(is.data.frame(temp)) {
        weight <- rbind(weight, temp)
      }
      print(paste0("Gathered weight data for ", day))
    }
  }
  
  write_csv(weight, path = "data/weight.csv")
  
  remove(temp)
}

weight <- weight %>%
  mutate(day = lubridate::wday(date, label = TRUE, locale = "English_United States", abbr = FALSE),
         week = week(date))

remove(weight_days, day)
##----- end:   WEIGHT ---------------------------------------------------------------------------------------------------------------------------------------------------##
##### end:   COLLECT DATA #################################################################################################################################################

##### start:   TIDY DATA ##################################################################################################################################################
# Minute-level --> continuous: calories, distance, elevation
data <- list()
for(type in c("calories", "distance", "elevation", "floors", "steps")) {
  data[[type]] <- act_data[[type]] %>% 
    select_(.dots = list("datetime", type, "day", "week")) %>%
    mutate(type = type) %>%
    rename(value = !!type) %>%
    select(datetime, type, value, day, week)
  data[[type]]$value <- as.double(data[[type]]$value)
}

data$heartrate <- hr_intraday %>%
  group_by(datetime = cut(datetime, breaks = "1 min")) %>%
  summarize(value = round(mean(hr))) %>%
  ungroup() %>%
  mutate(datetime = as.POSIXct(datetime, tz = "Europe/Amsterdam"),
         type = "heartrate",
         day = lubridate::wday(datetime, label = TRUE, locale = "English_United States", abbr = FALSE),
         week = week(datetime))

ml_num <- rbind(data$calories, data$distance, data$elevation, data$floors, data$heartrate, data$steps)
ml_num$type <- factor(ml_num$type, levels=c("calories", "distance", "elevation", "floors", "heartrate", "steps"), ordered = FALSE)
ml_num$value <- ifelse(ml_num$type == "distance", 1000*ml_num$value, ml_num$value)  # From kilometers to meters
ml_num$day <- factor(ml_num$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
ml_num$week <- factor(ml_num$week, ordered = TRUE)

ml_num <- ml_num[order(ml_num$datetime, ml_num$type),]
remove(data)

# Minute-level --> categorical: activity intensity
ml_ord <- tibble(datetime = act_data$minutesSedentary$datetime,
                 type = "intensity",
                 sedentary = act_data$minutesSedentary$sedentary,
                 light = act_data$minutesLightlyActive$lightlyActive,
                 fair = act_data$minutesFairlyActive$fairlyActive,
                 high = act_data$minutesVeryActive$veryActive)

ml_ord$type <- factor(ml_ord$type, ordered = FALSE)                     
ml_ord$value <- names(ml_ord[-c(1, 2)])[apply(ml_ord[-c(1, 2)] == 1, 1, which)]
ml_ord <- ml_ord %>% 
  mutate(day = lubridate::wday(datetime, label = TRUE, locale = "English_United States", abbr = FALSE),
         week = week(datetime)) %>%
  select(datetime, type, value, day, week)
ml_ord$value <- factor(ml_ord$value, levels=c("sedentary", "light", "fair", "high"), ordered = TRUE)
ml_ord$day <- factor(ml_ord$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
ml_ord$week <- factor(ml_ord$week, ordered = TRUE)

ml_ord <- ml_ord[order(ml_ord$datetime, ml_ord$type),]

# Daily-level --> continuous: weight, BMI
dl_num <- weight %>%
  gather(key = "type", value = "value", weight, bmi) %>%
  select(date, type, value, day, week)

dl_num$type <- factor(dl_num$type, ordered = FALSE)
dl_num$day <- factor(dl_num$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
dl_num$week <- factor(dl_num$week, ordered = TRUE)
dl_num <- dl_num[order(dl_num$date, dl_num$type),]

# Daily-level --> integer: activity calories, bmr calories, rest heartrate
data <- list()
for(type in c("caloriesBMR", "activityCalories")) {
  data[[type]] <- act_data[[type]] %>% 
    select_(.dots = list("datetime", type, "day", "week")) %>%
    mutate(type = type) %>%
    rename(value = !!type) %>%
    select(datetime, type, value, day, week)
  data[[type]]$value <- as.double(data[[type]]$value)
}

data$activityCalories$date <- as.Date(data$activityCalories$datetime, tz = "Europe/Amsterdam")
data$caloriesBMR$date <- as.Date(data$caloriesBMR$datetime, tz = "Europe/Amsterdam")

data$activityCalories$datetime <- NULL
data$caloriesBMR$datetime <- NULL

data$restheartrate <- hr_summary %>%
  filter(zone == "Out of Range") %>%
  mutate(date = as.Date(date, tz = "Europe/Amsterdam"),
         type = "restHeartrate",
         value = rest_hr) %>%
  select(date, type, value, day, week)

dl_num <- rbind(dl_num, data$caloriesBMR, data$activityCalories, data$restheartrate)
dl_num$type <- factor(dl_num$type, ordered = FALSE)
dl_num$day <- factor(dl_num$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
dl_num <- dl_num[order(dl_num$date, dl_num$type),]

dl_num <- dl_num %>%
  select(date, type, value, day, week)

remove(data)

# Remove non-tidy data-frames
remove(act_data)
remove(hr_intraday)
remove(hr_summary)
remove(type)
remove(weight)

# Place all data in a list
fitbit_data <- list(dl_num = dl_num, ml_ord = ml_ord, ml_num = ml_num)
# Filter for NA values in minute-level data (this is caused by daylight saving time)
fitbit_data$ml_ord <- fitbit_data$ml_ord %>% 
  filter(!is.na(datetime))
fitbit_data$ml_num <- fitbit_data$ml_num %>% 
  filter(!is.na(datetime))
# Remove old data frames
remove(dl_num, ml_ord, ml_num)
##### end:     TIDY DATA ##################################################################################################################################################