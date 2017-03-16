Person <- R6::R6Class("Person",
  public = list(
  fitbit = NULL, # list of fitbit data components of interest
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
    self$fitbit <- private$get_fitbit_data(user_email, user_pw)
    self$util <- private$create_util_data(self$start_date, self$end_date)
    self$groupings <- group_assignments
    }),
  
  private = list(
    create_util_data = function(start_date, end_date) {
      data <- list() # data is list of lists: date and each of day_of_week, weekend, etc.
      # (slightly inefficient)
      #date
      data$date <- seq(from = start_date, to = end_date, by = 1)
      
      #MTWTFSS
      data$day_of_week <- data.frame("date" = data$date,
                                     "day_of_week" = 
                                       lubridate::wday(data$date, label = TRUE))
      
      #1 if weekend, 0 otherwise
      # weekend <- c('Saturday', 'Sunday')
      # data$day_type <- data.frame("date" = data$date,
      #                             "day_type" = factor((data$day_of_week$day_of_week %in% weekend), 
      #                       levels=c(FALSE, TRUE),
      #                       labels=c('weekday', 'weekend')))
      data$day_type <- data.frame("date" = data$date,
                                  "day_type" = 
                                    factor(ifelse(lubridate::wday(data$date) %in% c(1, 7), "weekend", "weekday")))
      # Month
      data$month <- data.frame("date" = data$date, 
                                     "month" = lubridate::month(data$date, 
                                                                label=TRUE))
      return(data)
    }, 
    
    get_fitbit_data = function(user_email, user_pw) {
      data <- list()
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
      
      # Pull each of the intraday datasets, rbind together
      # data$df = get_intraday_data(cookie, what="steps", date="2017-02-16")
      # Intra-day (multiple times per day):
      # from get_intraday_data
      # steps
      # distance
      # floors
      # active-minutes
      # calories-burned
      # heart-rate
      data$isteps <- NULL
      data$idist <- NULL
      data$ifloors <- NULL
      data$iactive_min <- NULL
      data$ical_burn <- NULL
      data$ihr <- NULL
      
      for (indiv_date in format(seq.Date(from = as.Date(start), to = as.Date(end), by = "day"), format = "%Y-%m-%d")){
        char_date <- as.character(indiv_date)
        data$isteps <- rbind(data$isteps, fitbitScraper::get_intraday_data(cookie, what="steps", date=char_date))
        data$idist <- rbind(data$idist, fitbitScraper::get_intraday_data(cookie, what="distance", date=char_date))
        data$ifloors <- rbind(data$ifloors, fitbitScraper::get_intraday_data(cookie, what="floors", date=char_date))
        data$iactive_min <- rbind(data$iactive_min, fitbitScraper::get_intraday_data(cookie, what="active-minutes", date=char_date))
        data$ical_burn <- rbind(data$ical_burn, fitbitScraper::get_intraday_data(cookie, what="calories-burned", date=char_date))
        data$ihr <- rbind(data$ihr, fitbitScraper::get_intraday_data(cookie, what="heart-rate", date=char_date))
      }
      
      
      
      # Variables recorded once daily
      data$steps <- fitbitScraper::get_daily_data(cookie, what = "steps", 
                                   start_date = char_start, end_date = char_end)
      data$dist <- fitbitScraper::get_daily_data(cookie, what = "distance", 
                                                  start_date = char_start, 
                                                 end_date = char_end)
      # ONLY TECHNICALLY NEED THIS OR ABOVE ONE - dist issues
      data$distance <- fitbitScraper::get_daily_data(cookie, what = "distance", 
                                                 start_date = char_start, 
                                                 end_date = char_end)
      data$floors <- fitbitScraper::get_daily_data(cookie, what = "floors", 
                                                  start_date = char_start, 
                                                  end_date = char_end)
      data$minsVery <- fitbitScraper::get_daily_data(cookie, what = "minutesVery", 
                                                     start_date = char_start,
                                                     end_date = char_end)
      data$cal_ratio <- fitbitScraper::get_daily_data(cookie,
                                                      what = "caloriesBurnedVsIntake",
                                                      start_date = char_start,
                                                      end_date = char_end)
      data$rest_hr <- fitbitScraper::get_daily_data(cookie, what = "getRestingHeartRateData",
                                start_date = as.character(start),
                                end_date = as.character(end))
      data$hr_zones <- fitbitScraper::get_daily_data(cookie, 
                                what = "getTimeInHeartRateZonesPerDay",
                                start_date = as.character(start), 
                                end_date = as.character(end))
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
      data$sleep <- fitbitScraper::get_sleep_data(cookie, 
                                   start_date = char_start, 
                                   end_date = char_end)[[2]]
      data$weight <- fitbitScraper::get_weight_data(cookie, 
                                   start_date = char_start, 
                                   end_date = char_end)
      
      # Save fields that contain date to date, as Date class, as needed
      data$steps$date <- as.Date(strptime(data$steps$time, format = "%Y-%m-%d"))
      data$floors$date <- as.Date(strptime(data$floors$time, format = "%Y-%m-%d"))
      data$dist$date <- as.Date(strptime(data$dist$time, format = "%Y-%m-%d"))
      
      #duplicating (see above note)
      data$distance$date <- as.Date(strptime(data$dist$time, format = "%Y-%m-%d"))
      
      data$cal_ratio$date <- as.Date(strptime(data$cal_ratio$time, format = "%Y-%m-%d"))
      data$minsVery$date <- as.Date(strptime(data$minsVery$time, format = "%Y-%m-%d"))
      data$hr_zones$date <- as.Date(strptime(data$hr_zones$time, format = "%Y-%m-%d"))
      data$sleep$date <- as.Date(strptime(data$sleep$date, format = "%Y-%m-%d"))
      data$sleep$startDateTime <- as.POSIXct(strptime(data$sleep$startDateTime, 
          format = "%Y-%m-%d %H:%M:%S"))
      data$sleep$endDateTime <- as.POSIXct(strptime(data$sleep$endDateTime, 
          format = "%Y-%m-%d %H:%M:%S"))
      # pulling out columns of interest...
      data$sleepDuration <- data$sleep[, c("date", "sleepDuration")]
      data$minsRestlessAwake <- data.frame("date" = data$sleep$date,
                                           "minsRestlessAwake" = (data$sleep$sleepDuration - data$sleep$minAsleep))
      data$rest_hr$date <- as.Date(strptime(data$rest_hr$time, format="%Y-%m-%d"))
      # duplicating column for naming...
      data$rest_hr$rest_hr <- data$rest_hr$restingHeartRate
      data$weight$date <- as.Date(strptime(data$weight$time, format = "%Y-%m-%d"))
      
      return(data)
    }
    
  ))





                      

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