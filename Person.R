Person <- R6::R6Class("Person",
  public = list(
  fitbit_daily = NULL, # dataframe of daily fitbit data
  fitbit_intraday = NULL, #dataframe of intraday fitbit data
  util = NULL, # util/admin data
  target_steps = NULL,
  addl_data = NULL, # other data a user could provide (Apple, Fitbit, etc.)
  start_date = NA, # optional start date of interest
  end_date = NA, # optional end date of interest
  user_info = NULL, # optional list of user info, such as "age", "gender", "name", etc.
  groupings = NULL, # named list of dataframes each with 2 columns: 1. a known 
  # variable (date, weekend, etc.) and 2. "group" with the group assignment for 
  # observations with that variable of that value
  initialize = function(user_email = NA, user_pw = NA, user_info = NA, 
                        target_steps = 10000,
                        addl_data = NA, group_assignments = NA,
                        start_date = NA, end_date = NA) {
    self$addl_data <- addl_data
    self$start_date <- as.Date(strptime(start_date, format="%Y-%m-%d"))
    self$end_date <- as.Date(strptime(end_date, format="%Y-%m-%d"))
    self$target_steps <- target_steps
    self$user_info <- user_info
    self$util <- private$create_util_data(self$start_date, self$end_date)
    
    # NOTE: with this implementation need to make lookup table for source of variables
    # probably shouldn't expect users to know intraday or daily
    #self$fitbit_daily <- private$get_fitbit_daily(user_email, user_pw) 
    self$fitbit_intraday <- private$get_fitbit_intraday(user_email, user_pw)
    self$fitbit_daily <- private$get_fitbit_daily(user_email, user_pw)
    self$groupings <- group_assignments
    }),
  
  private = list(
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
        # ONLY TECHNICALLY NEED THIS OR ABOVE ONE - dist issues
        # data$distance <- fitbitScraper::get_daily_data(cookie, what = "distance",
        #                                           start_date = char_start,
        #                                           end_date = char_end)
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
        daily$hr_zones <- fitbitScraper::get_daily_data(cookie,
                                  what = "getTimeInHeartRateZonesPerDay",
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
        return(joined)
      }
    
  ))




########### deleted ##################

# not consistent
# if ("IN_DEFAULT_ZONE_3" %in% names(data$hr_zones)) {
#   data$hr_zones <- plyr::rename(data$hr_zones,
#                                  c("IN_DEFAULT_ZONE_3" = "peak",
#                                    "zone1" = "cardio",
#                                    "zone2" = "fat_burn"))
# } else if ("IN_DEFAULT_ZONE_2" %in% names(data$hr_zones)) {
#   data$hr_zones <- plyr::rename(data$hr_zones,
#                                 c("IN_DEFAULT_ZONE_2" = "cardio",
#                                   "zone1" = "fat_burn",
#                                   "zone2" = "peak"))
# }

# Save fields that contain date to date, as Date class, as needed
#data$steps$date <- as.Date(strptime(data$steps$time, format = "%Y-%m-%d"))
#data$floors$date <- as.Date(strptime(data$floors$time, format = "%Y-%m-%d"))
#data$dist$date <- as.Date(strptime(data$dist$time, format = "%Y-%m-%d"))

#duplicating (see above note)
#data$distance$date <- as.Date(strptime(data$dist$time, format = "%Y-%m-%d"))

#data$cal_ratio$date <- as.Date(strptime(data$cal_ratio$time, format = "%Y-%m-%d"))
#data$minsVery$date <- as.Date(strptime(data$minsVery$time, format = "%Y-%m-%d"))
#data$hr_zones$date <- as.Date(strptime(data$hr_zones$time, format = "%Y-%m-%d"))
#data$sleep$date <- as.Date(strptime(data$sleep$date, format = "%Y-%m-%d"))
#data$sleep$startDateTime <- as.POSIXct(strptime(data$sleep$startDateTime,
#    format = "%Y-%m-%d %H:%M:%S"))
#data$sleep$endDateTime <- as.POSIXct(strptime(data$sleep$endDateTime,
#    format = "%Y-%m-%d %H:%M:%S"))
# pulling out columns of interest...
#data$sleepDuration <- data$sleep[, c("date", "sleepDuration")]
#data$minsRestlessAwake <- data.frame("date" = data$sleep$date,
#                                     "minsRestlessAwake" = (data$sleep$sleepDuration - data$sleep$minAsleep))
#data$rest_hr$date <- as.Date(strptime(data$rest_hr$time, format="%Y-%m-%d"))
# duplicating column for naming...
#data$rest_hr$rest_hr <- data$rest_hr$restingHeartRate
#data$weight$date <- as.Date(strptime(data$weight$time, format = "%Y-%m-%d"))

# 
# if(FALSE){
#   data <- list()
#   cookie <- fitbitScraper::login(email=user_email, password=user_pw)
#   start <- self$start_date
#   end <- self$end_date
#   
#   if (is.na(start)){
#     # set start to beginning of time if there was none
#   }
#   if (is.na(end)){
#     # set end to today if there was none
#   }
#   
#   char_start <- as.character(start)
#   char_end <- as.character(end)
#   
#   # Pull each of the intraday datasets, rbind together
#   # data$df = get_intraday_data(cookie, what="steps", date="2017-02-16")
#   # Intra-day (multiple times per day):
#   # from get_intraday_data
#   # steps
#   # distance
#   # floors
#   # active-minutes
#   # calories-burned
#   # heart-rate
#   data$isteps <- NULL
#   data$idist <- NULL
#   data$ifloors <- NULL
#   data$iactive_min <- NULL
#   data$ical_burn <- NULL
#   data$ihr <- NULL
#   
#   for (indiv_date in format(seq.Date(from = as.Date(start), to = as.Date(end), by = "day"), format = "%Y-%m-%d")){
#     char_date <- as.character(indiv_date)
#     data$isteps <- rbind(data$isteps, fitbitScraper::get_intraday_data(cookie, what="steps", date=char_date))
#     data$idist <- rbind(data$idist, fitbitScraper::get_intraday_data(cookie, what="distance", date=char_date))
#     data$ifloors <- rbind(data$ifloors, fitbitScraper::get_intraday_data(cookie, what="floors", date=char_date))
#     data$iactive_min <- rbind(data$iactive_min, fitbitScraper::get_intraday_data(cookie, what="active-minutes", date=char_date))
#     data$ical_burn <- rbind(data$ical_burn, fitbitScraper::get_intraday_data(cookie, what="calories-burned", date=char_date))
#     data$ihr <- rbind(data$ihr, fitbitScraper::get_intraday_data(cookie, what="heart-rate", date=char_date))
#   }
#   
#   
#   
#   # Variables recorded once daily
#   data$steps <- fitbitScraper::get_daily_data(cookie, what = "steps", 
#                                start_date = char_start, end_date = char_end)
#   data$dist <- fitbitScraper::get_daily_data(cookie, what = "distance", 
#                                               start_date = char_start, 
#                                              end_date = char_end)
#   # ONLY TECHNICALLY NEED THIS OR ABOVE ONE - dist issues
#   data$distance <- fitbitScraper::get_daily_data(cookie, what = "distance", 
#                                              start_date = char_start, 
#                                              end_date = char_end)
#   data$floors <- fitbitScraper::get_daily_data(cookie, what = "floors", 
#                                               start_date = char_start, 
#                                               end_date = char_end)
#   data$minsVery <- fitbitScraper::get_daily_data(cookie, what = "minutesVery", 
#                                                  start_date = char_start,
#                                                  end_date = char_end)
#   data$cal_ratio <- fitbitScraper::get_daily_data(cookie,
#                                                   what = "caloriesBurnedVsIntake",
#                                                   start_date = char_start,
#                                                   end_date = char_end)
#   data$rest_hr <- fitbitScraper::get_daily_data(cookie, what = "getRestingHeartRateData",
#                             start_date = as.character(start),
#                             end_date = as.character(end))
#   data$hr_zones <- fitbitScraper::get_daily_data(cookie, 
#                             what = "getTimeInHeartRateZonesPerDay",
#                             start_date = as.character(start), 
#                             end_date = as.character(end))
#   # not consistent
#   # if ("IN_DEFAULT_ZONE_3" %in% names(data$hr_zones)) {
#   #   data$hr_zones <- plyr::rename(data$hr_zones,
#   #                                  c("IN_DEFAULT_ZONE_3" = "peak",
#   #                                    "zone1" = "cardio",
#   #                                    "zone2" = "fat_burn"))
#   # } else if ("IN_DEFAULT_ZONE_2" %in% names(data$hr_zones)) {
#   #   data$hr_zones <- plyr::rename(data$hr_zones,
#   #                                 c("IN_DEFAULT_ZONE_2" = "cardio",
#   #                                   "zone1" = "fat_burn",
#   #                                   "zone2" = "peak"))
#   # }
#   data$sleep <- fitbitScraper::get_sleep_data(cookie, 
#                                start_date = char_start, 
#                                end_date = char_end)[[2]]
#   data$weight <- fitbitScraper::get_weight_data(cookie, 
#                                start_date = char_start, 
#                                end_date = char_end)
#   
#   # Save fields that contain date to date, as Date class, as needed
#   data$steps$date <- as.Date(strptime(data$steps$time, format = "%Y-%m-%d"))
#   data$floors$date <- as.Date(strptime(data$floors$time, format = "%Y-%m-%d"))
#   data$dist$date <- as.Date(strptime(data$dist$time, format = "%Y-%m-%d"))
#   
#   #duplicating (see above note)
#   data$distance$date <- as.Date(strptime(data$dist$time, format = "%Y-%m-%d"))
#   
#   data$cal_ratio$date <- as.Date(strptime(data$cal_ratio$time, format = "%Y-%m-%d"))
#   data$minsVery$date <- as.Date(strptime(data$minsVery$time, format = "%Y-%m-%d"))
#   data$hr_zones$date <- as.Date(strptime(data$hr_zones$time, format = "%Y-%m-%d"))
#   data$sleep$date <- as.Date(strptime(data$sleep$date, format = "%Y-%m-%d"))
#   data$sleep$startDateTime <- as.POSIXct(strptime(data$sleep$startDateTime, 
#       format = "%Y-%m-%d %H:%M:%S"))
#   data$sleep$endDateTime <- as.POSIXct(strptime(data$sleep$endDateTime, 
#       format = "%Y-%m-%d %H:%M:%S"))
#   # pulling out columns of interest...
#   data$sleepDuration <- data$sleep[, c("date", "sleepDuration")]
#   data$minsRestlessAwake <- data.frame("date" = data$sleep$date,
#                                        "minsRestlessAwake" = (data$sleep$sleepDuration - data$sleep$minAsleep))
#   data$rest_hr$date <- as.Date(strptime(data$rest_hr$time, format="%Y-%m-%d"))
#   # duplicating column for naming...
#   data$rest_hr$rest_hr <- data$rest_hr$restingHeartRate
#   data$weight$date <- as.Date(strptime(data$weight$time, format = "%Y-%m-%d"))
#   
#   return(data)
#   }

                      

################  R6 EXAMPLES (DELETE LATER) ###############################
# 
# Person <- R6::R6Class("Person",
#                       public = list(
#                         name = NULL,
#                         hair = NULL,
#                     initialize = function(name = NA, hair = NA) {
#                       self$name <- name
#                       self$hair <- hair
#                       self$greet()
#                     },
#                     set_hair = function(val) {
#                       self$hair <- val
#                     },
#                     greet = function() {
#                       cat(paste0("Hello, my name is ", self$name, ".\n"))
#                     }
#                   )
# )
# 
# 
# EuroCurrencyTracker <- R6Class("EuroCurrencyTracker",
#                                public = list(
#                                  initialize = function(currencies) {
#                                    private$currencies <- currencies
#                                    private$rates <- vector("list", length=length(currencies))
#                                    names(private$rates) <- currencies
#                                    private$day <- 0L
#                                  },
#                                  startElement = function(name, attrs) {
#                                    if (name == "Cube")
#                                      if("time" %in% names(attrs)) {
#                                        private$day <- private$day + 1L
#                                        ##print(attrs["time"])
#                                        private$times[private$day] <- attrs["time"]
#                                      }
#                                    if ("currency" %in% names(attrs) &&
#                                        attrs["currency"] %in% private$currencies) {
#                                      private$rates[[attrs["currency"]]][private$day] <- as.numeric(attrs["rate"])
#                                    }
#                                  },
#                                  xmlHandlers = function() list(startElement = self$startElement),
#                                  rateData = function() list(times = private$times,
#                                                             rates = private$rates)
#                                ),
#                                private = list(
#                                  currencies = character(0),
#                                  rates = NA,
#                                  times = character(0),
#                                  day = NA
#                                ))