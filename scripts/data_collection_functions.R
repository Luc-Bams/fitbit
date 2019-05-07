## --- Fitbit -----------------------------------------------------------------------------------------------------------------------------------------------------------##
##
## Script name: data_collection_functions
##
## Purpose of script: Importing data for my own Fitbit data project.
##
## Author: Luc Bams
##
## Date Created: 2019-04-04
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

##### start: LOAD PACKAGES ################################################################################################################################################
library(dotenv)

require(httr)
require(jsonlite)
require(magrittr)
require(purrr)
require(tidyverse)
##### end:   LOAD PACKAGES ################################################################################################################################################

##### start: DEFINE FUNCTIONS ############################################################################################################################################
load_dot_env(file = ".env")

##----- Checks the status of the token ----------------------------------------------------------------------------------------------------------------------------------##
check_state <- function(token = NULL) {
  POST(url = 'https://api.fitbit.com/1.1/oauth2/introspect',
       add_headers(Authorization = paste0('Bearer ', token)),
       body = paste0('token=', token),
       content_type('application/x-www-form-urlencoded'))
}

#state <- check_state(token = Sys.getenv('FITB_AUTH'))                    # Retrieves the state of the token
#content(state)$active                                                    # TRUE when token is active

##----- start: ACTIVITY --------------------------------------------------------------------------------------------------------------------------------------------------##
##....... Retrieves the raw "list-of-lists"-type activity data for the specified date ....................................................................................##
##......... Available values : calories, caloriesBMR, steps, distance, floors, elevation, minutesSedentary, minutesLightlyActive, minutesFairlyActive, minutesVeryActive,
##......... activityCalories
get_activity_raw <- function(date = NULL,
                             activity = NULL,
                             token = Sys.getenv("FITB_AUTH")) {
  GET(url = paste0("https://api.fitbit.com/1/user/-/activities/", activity,"/date/", date, "/1d.json"),
      add_headers(Authorization = paste0("Bearer ", token)))
}
##....... Performs the GET request for the activity data of the specified date and converts it to a data frame ...........................................................##
get_activity <- function(date = NULL, activity = NULL) {
  df <- content(get_activity_raw(date = date, activity = activity))         # Saves the content of the response of the request
  if((activity == "activityCalories") || (activity == "caloriesBMR")) {
    df <- df[[paste0("activities-", activity)]][[1]]
  }
  else {
    df <- df[[paste0("activities-", activity, "-intraday")]][["dataset"]]
  }
  
  if(activity == "calories") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame
    df$ID <- rep(x = 1:(nrow(df) / 4), each = 4)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", 
                      ifelse(grepl("level", rownames(df)), "level", 
                      ifelse(grepl("mets", rownames(df)), "mets", "value")))  # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 4 rows
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains variable names
    colnames(df) <- c("level", "mets", "datetime", "calories")                # Renames columns
    df <- df[, c("datetime", "calories", "level", "mets")]                    # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$calories <- as.double(as.character(df$calories))                       # Transforms calories to double type
    df$level <- as.integer(as.character(df$level))                            # Transforms level to integer type
    df$mets <- as.integer(as.character(df$mets))                              # Transforms mets to integer type
  }
  else if (activity == "distance") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "distance")        # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("distance", "datetime")                                 # Renames columns
    df <- df[, c("datetime", "distance")]                                     # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$distance <- as.double(as.character(df$distance))                       # Transforms steps to integer type
  }
  else if (activity == "elevation") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "elevation")       # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("elevation", "datetime")                                # Renames columns
    df <- df[, c("datetime", "elevation")]                                    # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$elevation <- as.double(as.character(df$elevation))                     # Transforms steps to integer type
  }
  else if (activity == "floors") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "floors")          # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("floors", "datetime")                                   # Renames columns
    df <- df[, c("datetime", "floors")]                                       # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$floors <- as.integer(as.character(df$floors))                          # Transforms steps to integer type
  }
  else if(activity == "steps") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "steps")           # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("steps", "datetime")                                    # Renames columns
    df <- df[, c("datetime", "steps")]                                        # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$steps <- as.integer(as.character(df$steps))                            # Transforms steps to integer type
  }
  else if(activity == "minutesVeryActive") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "veryActive")      # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    df <- df[, c(2, 1)]
    colnames(df) <- c("veryActive", "datetime")                               # Renames columns
    df <- df[, c("datetime", "veryActive")]                                   # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$veryActive <- as.integer(as.character(df$veryActive))                  # Transforms steps to integer type
  }
  else if(activity == "minutesFairlyActive") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "fairlyActive")    # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("fairlyActive", "datetime")                             # Renames columns
    df <- df[, c("datetime", "fairlyActive")]                                 # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$fairlyActive <- as.integer(as.character(df$fairlyActive))              # Transforms steps to integer type
  }
  else if(activity == "minutesLightlyActive") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "lightlyActive")   # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("lightlyActive", "datetime")                            # Renames columns
    df <- df[, c("datetime", "lightlyActive")]                                # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$lightlyActive <- as.integer(as.character(df$lightlyActive))            # Transforms steps to integer type
  }
  else if(activity == "minutesSedentary") {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame
    df <- as.data.frame(t(df))                                                # Transposes the data frame    
    df$ID <- rep(x = 1:(nrow(df) / 2), each = 2)                              # Creates an ID column to use as key in spreading later on
    df$type <- ifelse(grepl("time", rownames(df)), "time", "sedentary")       # Specifies the row of data is a time or a heart rate
    names(df)[names(df) == "V1"] <- "value"
    rownames(df) <- c()                                                       # Remove row names
    df <- df %>%
      spread(key = ID, value = value)                                         # Spreads the data over n columns and 2 rows ("datetime" and "hr")
    df <- as.data.frame(t(df))                                                # Transposes the data frame to n × 2                                   
    df <- df[2:nrow(df),]                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("sedentary", "datetime")                                # Renames columns
    df <- df[, c("datetime", "sedentary")]                                    # Removes the ID column
    df$datetime <- paste0(date, " ", df$datetime)                             # Appends the date to the times
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
    df$sedentary <- as.integer(as.character(df$sedentary))                    # Transforms steps to integer type
  }
  else if((activity == "activityCalories") || (activity == "caloriesBMR")) {
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                      # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)      # Transforms the list to a data frame                                                     # Removes the first row of the data since it contains ("time" and "value")
    colnames(df) <- c("datetime", activity)                                   # Renames columns
    df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d", tz = "CET")   # Transforms datetime to POSIXct type
    df$datetime <- df$datetime + 24*60*60-1                                 # Add 24*60*60-1 seconds to the time to indicate that this is the value at the end of the day
  }
  
  return(df)
}
##----- end:   ACTIVITY --------------------------------------------------------------------------------------------------------------------------------------------------##

##----- start: HEART RATE ------------------------------------------------------------------------------------------------------------------------------------------------##
##....... Retrieves the raw "list-of-lists"-type heart rate data for the specified date ..................................................................................##
get_hr_raw <- function(date = NULL,
                       detail_level = "1min",
                       token = Sys.getenv("FITB_AUTH")) {
  GET(url = paste0("https://api.fitbit.com/1/user/-/activities/heart/date/",
                   date, "/1d/", detail_level,".json"),
      add_headers(Authorization = paste0("Bearer ", token)))
}

##....... Performs the GET request for the heart rate data of the specified date and converts it to a data frame .........................................................##
get_hr <- function(date = NULL,
                   detail_level = "1min") {
  hr <- content(get_hr_raw(date = date, detail_level = detail_level))                           # Retrieves all hr data for the specified date and detail level
  
  ## Intraday heart rate data --> Every heart rate measurement in an entire day (detail level = "1sec") or aggregated to minute-level data points (detail level =  "1min")
  df_intraday_hr <- hr[["activities-heart-intraday"]][["dataset"]]                              # Extracts the list with intraday heart rate data
  df_intraday_hr <- unlist(df_intraday_hr, recursive = TRUE, use.names = TRUE)                  # Flattens the list structere data into a vector
  df_intraday_hr <- data.frame(lapply(df_intraday_hr, type.convert), stringsAsFactors = FALSE)  # Transforms the list to a data frame
  df_intraday_hr <- as.data.frame(t(df_intraday_hr))                                            # Transposes the data frame    
  df_intraday_hr$ID <- rep(x = 1:(nrow(df_intraday_hr) / 2), each = 2)                          # Creates an ID column to use as key in spreading later on
  df_intraday_hr$type <- ifelse(grepl("time", rownames(df_intraday_hr)), "time", "hr")          # Specifies the row of data is a time or a heart rate
  names(df_intraday_hr)[names(df_intraday_hr) == "V1"] <- "value"               
  rownames(df_intraday_hr) <- c()                                                               # Remove row names
  df_intraday_hr <- df_intraday_hr %>%
    spread(key = ID, value = value)                                                             # Spreads the data over n columns and 2 rows ("datetime" and "hr")
  df_intraday_hr <- as.data.frame(t(df_intraday_hr))                                            # Transposes the data frame to n × 2                                   
  df_intraday_hr <- df_intraday_hr[2:nrow(df_intraday_hr),]                                     # Removes the first row of the data since it contains ("time" and "value")
  colnames(df_intraday_hr) <- c("hr", "datetime")                                               # Renames columns
  df_intraday_hr <- df_intraday_hr[, c("datetime", "hr")]                                       # Removes the ID column
  df_intraday_hr$datetime <- paste0(date, " ", df_intraday_hr$datetime)                         # Appends the date to the times
  df_intraday_hr$datetime <- as.POSIXct(df_intraday_hr$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")  # Transforms datetime to POSIXct type
  df_intraday_hr$hr <- as.integer(as.character(df_intraday_hr$hr))                              # Transforms hr to integer type
  
  ## High-level heart rate data
  df_summary <- hr[["activities-heart"]][[1]][["value"]][["heartRateZones"]]                    # Extracts the list with data on heart rate zones
  rest_hr <- hr[["activities-heart"]][[1]][["value"]][["restingHeartRate"]]                     # Extracts the list with resting heart rate
  hr_temp <- unlist(df_summary, recursive = TRUE, use.names = TRUE)                             # Flattens the list structere data into a vector
  c_temp_1 <- c(date, rest_hr, hr_temp[5], hr_temp[2], hr_temp[3], hr_temp[4], hr_temp[1])      # Values in specific order in list can be arranged properly in this ...
  c_temp_2 <- c(date, rest_hr, hr_temp[10], hr_temp[7], hr_temp[8], hr_temp[9], hr_temp[6])     # straightforward manner.
  c_temp_3 <- c(date, rest_hr, hr_temp[15], hr_temp[12], hr_temp[13], hr_temp[14], hr_temp[11])
  c_temp_4 <- c(date, rest_hr, hr_temp[20], hr_temp[17], hr_temp[18], hr_temp[19], hr_temp[16])
  df_temp <- t(data.frame(c_temp_1, c_temp_2, c_temp_3, c_temp_4))                              # Combine per-heart-rate-zone vectors into a data frame
  names_temp <- c("date", "rest_hr", "zone", "max_hr", "min_hr", "minutes", "cal_out")          # Definition of vector with column names
  colnames(df_temp) <- names_temp                                                               # Assign names to df_temp
  rownames(df_temp) <- c()                                                                      # Remove row names
  df_temp <- as.data.frame(df_temp)                                                             # This is necessary because t() creates an atomic vector not a dataframe
  df_temp$date <- as.POSIXct(df_temp$date, format = "%Y-%m-%d", tz = "CET")
  df_temp$rest_hr <- as.integer(as.character(df_temp$rest_hr))
  df_temp$max_hr <- as.integer(as.character(df_temp$max_hr))
  df_temp$min_hr <- as.integer(as.character(df_temp$min_hr))
  df_temp$minutes <- as.integer(as.character(df_temp$minutes))
  df_temp$cal_out <- as.integer(as.character(df_temp$cal_out))
  df_summary <- as.data.frame(df_temp)
  remove(names_temp)
  remove(df_temp)
  hr_data <- list(summary = df_summary, intraday_hr = df_intraday_hr)
  return(hr_data)
}
##----- end:   HEART RATE ------------------------------------------------------------------------------------------------------------------------------------------------##

##----- start: WEIGHT ----------------------------------------------------------------------------------------------------------------------------------------------------##
##....... Retrieves the raw "list-of-lists"-type weight data for the specified date ......................................................................................##
get_weight_raw <- function(date = NULL,
                           token = Sys.getenv('FITB_AUTH')) {
  GET(url = paste0("https://api.fitbit.com/1/user/-/body/log/weight/date/",
                   date, ".json"),
      add_headers(Authorization = paste0("Bearer ", token)))
}

##....... Performs the GET request for the weight data of the specified date and converts it to a data frame .............................................................##
get_weight <- function(date = NULL) {
  df <- content(get_weight_raw(date = date))                              # Saves the content of the response of the request
  if(length(df$weight) > 0) {  
    df <- unlist(df, recursive = TRUE, use.names = TRUE)                    # Unlists all levels of the list of lists and preserves the names --> generates a named vector
    df <- data.frame(lapply(df, type.convert), stringsAsFactors = FALSE)    # Transforms the named vector to a df frame
    names(df) <- str_replace(names(df), "^weight\\.", "")                   # Removes the suffix "weight." - artefact of unlisting - from the variable names
    df <- subset(df, select = -c(source, time))                             # Source is always API and time is always "23:59:59". Therefore, both are useless
    df$date <- as.Date(df$date)
    df <- df[c(2, 4, 1, 3)]
  }
  return(df)
}
##----- end:   WEIGHT ----------------------------------------------------------------------------------------------------------------------------------------------------##
##### end:    COLLECT DATA #################################################################################################################################################