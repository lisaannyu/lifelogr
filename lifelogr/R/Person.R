#' @include global_var.R
Person <- R6::R6Class("Person",
  public = list(
  fitbit_daily = NULL, # dataframe of daily fitbit data
  fitbit_intraday = NULL, #dataframe of intraday fitbit data
  util = NULL, # util/admin data
  target_steps = NULL,
  start_date = NA, # optional start date of interest
  end_date = NA, # optional end date of interest
  user_info = NULL, # optional list of user info, such as "age", "gender", "name", etc.
  groupings = NULL, # named list of dataframes each with 2 columns: 1. a known 
  # variable (date, weekend, etc.) and 2. "group" with the group assignment for 
  # observations with that variable of that value
  apple = NULL,
  addl_data = NULL, # user's own df of other data
  addl_data2 = NULL, # another df
  initialize = function(user_email = NA, user_pw = NA, user_info = NA, 
                        apple_data_file = NA,
                        target_steps = 10000,
                        addl_data = NA, addl_data2 = NA, group_assignments = NA,
                        start_date = NA, end_date = NA) {
    self$addl_data <- addl_data
    self$addl_data2 <- addl_data2
    self$start_date <- as.Date(strptime(start_date, format="%Y-%m-%d"))
    self$end_date <- as.Date(strptime(end_date, format="%Y-%m-%d"))
    self$target_steps <- target_steps
    self$user_info <- user_info
    self$util <- private$create_util_data(self$start_date, self$end_date)

    if (!is.na(apple_data_file)){
      self$apple <- private$load_apple_data(apple_data_file) # user needs to pass in path from this
      # directory to file
    }
    
    self$fitbit_intraday <- private$get_fitbit_intraday(user_email, user_pw)
    self$fitbit_daily <- private$get_fitbit_daily(user_email, user_pw)
    
      # ^ NOTE: need to do the manipulations to get it into the same format as fitbit
    # or have the user pass in a matching dataframe
    #list of group assignments
    self$groupings <- group_assignments # convert these to tibbles
    }),
  
  private = list(
    load_apple_data = function(apple_data_file){
      raw_df <- read.csv(apple_data_file)
      cleaned <- tibble::data_frame(raw_df)
      
      # TODO: convert dates/datetimes, names of columns to match fitbit data
      
      return(cleaned)
    },
    
    create_util_data = function(start_date, end_date) {
      
      # date range between start and end date
      df <- tibble::data_frame(date = lubridate::ymd(as.Date(as.POSIXct(seq(from = start_date,
                                              to = end_date, by = 1)))))
      
      #      joined$date <- lubridate::ymd(as.Date(as.POSIXct(joined$datetime, Sys.timezone())))
      df$datetime <- df$date
      # MTWTFSS
      df$day_of_week <- lubridate::wday(df$date, label = TRUE)
      
      # need to change this if change wday to give full days
      # weekend/weekday
      weekend <- c('Sat', 'Sun') 
      df$day_type <- factor((df$day_of_week %in% weekend),
                            levels=c(FALSE, TRUE), 
                            labels=c('weekday', 'weekend'))
      # Month
      df$month <- lubridate::month(df$date, label=TRUE)
      return(tibble::as_data_frame(df))
    }, 
    
    get_fitbit_intraday = function(user_email, user_pw) {
      
      cookie <- fitbitScraper::login(email=user_email, password=user_pw)
      start <- self$start_date
      end <- self$end_date
      
      if (is.na(start)){
        # set start to beginning of time if there was none
      }
      if (is.na(end)){
        # set end to today if there was none
      }
      
      char_start <- as.character(start)
      char_end <- as.character(end)
      
      
      # Build each intraday dataset: create list of lists, then rbind all together at once
      intraday <- list() # list of lists; each of the below is a list of a day's data
      intraday$isteps <- NULL
      intraday$idist <- NULL
      intraday$ifloors <- NULL
      intraday$iactive_min <- NULL
      intraday$ical_burn <- NULL
      intraday$ihr <- NULL
      
      # could use self$util$date if got format right
      # could use something other than rbinding everything together one at a time
      for (indiv_date in format(seq.Date(from = as.Date(start), to = as.Date(end), by = "day"), format = "%Y-%m-%d")){
        char_date <- as.character(indiv_date)
        intraday$isteps <- rbind(intraday$isteps, fitbitScraper::get_intraday_data(cookie, what="steps", date=char_date))
        intraday$idist <- rbind(intraday$idist, fitbitScraper::get_intraday_data(cookie, what="distance", date=char_date))
        intraday$ifloors <- rbind(intraday$ifloors, fitbitScraper::get_intraday_data(cookie, what="floors", date=char_date))
        intraday$iactive_min <- rbind(intraday$iactive_min, fitbitScraper::get_intraday_data(cookie, what="active-minutes", date=char_date))
        intraday$ical_burn <- rbind(intraday$ical_burn, fitbitScraper::get_intraday_data(cookie, what="calories-burned", date=char_date))
        intraday$ihr <- rbind(intraday$ihr, fitbitScraper::get_intraday_data(cookie, what="heart-rate", date=char_date))
      }
      
      # Add weight (users can weigh many times/anytime in day)
      intraday$weight <- fitbitScraper::get_weight_data(cookie,
                                  start_date = char_start,
                                   end_date = char_end)
      
      # Join all, create date, time, datetime columns and drop dateTime
      joined <- tibble::as_data_frame(Reduce(function(x, y) merge(x, y, all=TRUE, by = "time"), intraday))
      joined <- dplyr::select(joined, -dateTime)
      joined$datetime <- lubridate::ymd_hms(joined$time, tz = Sys.timezone())
      joined$date <- lubridate::ymd(as.Date(as.POSIXct(joined$datetime, Sys.timezone())))
      joined$time <- lubridate::make_datetime(year = "1970", month = "01", day = "01",
                                              hour = lubridate::hour(joined$datetime),
                                              min = lubridate::minute(joined$datetime), 
                                              sec = lubridate::second(joined$datetime))
      joined <- plyr::rename(joined, replace = c("active-minutes" = "activeMin"))
      joined$distanceKm <- joined$distance * MI_TO_KM
      joined$weightKg <- joined$weight * LB_TO_KG
      return(joined)
      },
     
      # Returns a tibble of joined variables recorded daily
      # NOTE: should put variables of interest here - ex: minsAwakeRestless, 
      # pull out into dataframe instead of doing manipulations in other places
      get_fitbit_daily = function(user_email, user_pw) {
        # better way than duplicating this (save somewhere/use to call 
        # both from a higher level function?)
        cookie <- fitbitScraper::login(email=user_email, password=user_pw)
        start <- self$start_date
        end <- self$end_date
        
        if (is.na(start)){
          # set start to beginning of time if there was none
        }
        if (is.na(end)){
          # set end to today if there was none
        }
        
        char_start <- as.character(start)
        char_end <- as.character(end)
        
        
        daily <- list()
        
        daily$steps <- fitbitScraper::get_daily_data(cookie, what = "steps",
                                     start_date = char_start, end_date = char_end)
        daily$distance <- fitbitScraper::get_daily_data(cookie, what = "distance",
                                                    start_date = char_start,
                                                   end_date = char_end)
        daily$floors <- fitbitScraper::get_daily_data(cookie, what = "floors",
                                                    start_date = char_start,
                                                    end_date = char_end)
        daily$minsVery <- fitbitScraper::get_daily_data(cookie, what = "minutesVery",
                                                       start_date = char_start,
                                                       end_date = char_end)
        daily$cal_ratio <- fitbitScraper::get_daily_data(cookie,
                                                        what = "caloriesBurnedVsIntake",
                                                        start_date = char_start,
                                                        end_date = char_end)
        daily$rest_hr <- fitbitScraper::get_daily_data(cookie, what = "getRestingHeartRateData",
                                  start_date = as.character(start),
                                  end_date = as.character(end))

        daily$sleep <- fitbitScraper::get_sleep_data(cookie,
                                     start_date = char_start,
                                     end_date = char_end)[[2]]

        # temporarily get sleep's date to be "time"
        daily$sleep$time <- as.POSIXct(daily$sleep$date)
        
        #join all variables, create and keep only date column
        #Join all, create date, time, datetime columns and drop dateTime
        joined <- tibble::as_data_frame(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                                    by = "time"),
                                               daily))
        joined$date <- lubridate::ymd(as.Date(as.POSIXct(joined$time, tz = Sys.timezone())))
        joined <- dplyr::select(joined, -time)
        # doesn't work...figure out if desired (not actually a datetime)
        joined$datetime <- joined$date#lubridate::ymd_hms(joined$time, tz = Sys.timezone())
        joined$minsRestlessAwake <- joined$sleepDuration - joined$minAsleep
        
        # create sleepDurationHrs and minAsleepHrs and restlessProp
        joined <- dplyr::mutate(joined,
                                sleepDurationHrs = sleepDuration / 60,
                                minAsleepHrs = minAsleep / 60,
                                restlessProp = 
                                  (sleepDurationHrs - minAsleepHrs) / 
                                  sleepDurationHrs * 100)
        
        # create distanceKm
        joined <- dplyr::mutate(joined,
                                distanceKm = distance * MI_TO_KM)
        return(joined)
      }
    
  ))
