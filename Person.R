Person <- R6::R6Class("Person",
  public = list(
  fitbit = NULL, # list of fitbit data components of interest
  addl_data = NULL, # other data a user could provide (Apple, Fitbit, etc.)
  start_date = NA, # optional start date of interest
  end_date = NA, # optional end date of interest
  user_info = NULL, # optional list of user info, such as "age", "gender", "name", etc.
  initialize = function(user_email = NA, user_pw = NA, user_info = NA,
                        addl_data = NA, start_date = NA, end_date = NA) {
    self$addl_data <- addl_data
    self$start_date <- as.Date(strptime(start_date, format="%Y-%m-%d"))
    self$end_date <- as.Date(strptime(end_date, format="%Y-%m-%d"))
    self$user_info <- user_info
    self$fitbit <- private$get_fitbit_data(user_email, user_pw)
    }),
                      
  private = list(
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
      
      # pull each of the datasets can only get daily, rbind
      #data$df = get_intraday_data(cookie, what="steps", date="2017-02-16")
      
      # Variables recorded once daily
      data$steps <- fitbitScraper::get_daily_data(cookie, what = "steps", 
                                   start_date = char_start, end_date = char_end)
      data$dist <- fitbitScraper::get_daily_data(cookie, what = "distance", 
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
      data$sleep <- fitbitScraper::get_sleep_data(cookie, 
                                   start_date = char_start, 
                                   end_date = char_end)[[2]]
      data$weight <- fitbitScraper::get_weight_data(cookie, 
                                   start_date = char_start, 
                                   end_date = char_end)
      
      # Save fields that contain date to date, as Date class, as needed
      data$steps$date <- as.Date(strptime(data$steps$time, format = "%Y-%m-%d"))
      data$sleep$date <- as.Date(strptime(data$sleep$date, format = "%Y-%m-%d"))
      data$rest_hr$date <- as.Date(strptime(data$rest_hr$time, format="%Y-%m-%d"))
      
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