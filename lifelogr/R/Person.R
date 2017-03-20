#' @include global_var.R
NULL

#' A \code{Person} object is a complete view of an individual over a certain
#' time period, as seen through data from multiple sources
#'
#' @description \code{Person} is an object that encapsulates an individual's 
#' data over a specified date range (start and end date stored as \code{Date} 
#' objects.
#' An individual consists of basic information, such as name, age,
#' and gender (a \code{list} with named elements), data from their self-tracking 
#' devices such as Fitbit, Apple health, etc. (data from each source is a tibble
#' dataframe), individual goals such as target steps (\code{numeric}), 
#' additional data from self-tracking apps or one's own collection system 
#' (stored as a tibble dataframe), and ways of grouping the data a user may be 
#' interested in, such as grouping by seasons, or comparing weekend to weekday 
#' behavior and health (stored as a list of named dataframes, which each contain 
#' group assignments).
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom tibble data_frame as_data_frame
#' @importFrom lubridate hour minute second make_datetime ymd wday month
#' @importFrom fitbitScraper login get_intraday_data get_weight_data 
#'             get_daily_data get_sleep_data
#' @importFrom dplyr select mutate
#' @importFrom plyr rename
#' @field fitbit_daily tibble dataframe of fitbit variables (for user account
#'        info provided) observed daily
#' @field fitbit_intraday tibble dataframe of fitbit variables (for user account
#'        info provided) observed multiple times a day
#' @field util tibble dataframe that maps each date in the date range to utility
#'        information about that date
#' @field target_steps the person's target number of steps (numeric) for each
#'        day (default 10,000)
#' @field start_date start of user's date range of interest (Date object)
#' @field end_date end of user's date range of interest (Date object)
#' @field user_info provided user info, such as "age", "gender", "name" (list)
#' @field groupings named list of dataframes, each with two columns - a known 
#'        variable, and group, with the group assignment for observations where 
#'        that variable has appropriate value
#' @field apple tibble dataframe of user's provided Apple Health data
#' @field addl_data dataframe of data from another source provided by user
#' @field addl_data2 dataframe of data from another source provided by user
#' 
#' @section Methods:
#'
#' \describe{
#'   \item{\code{Person$new(fitbit_user_email, fitbit_user_pw, user_info = NA,
#'                           apple_data_file, target_steps, addl_data,
#'                           addl_data2, group_assignments, start_date, 
#'                           end_date)}}{Creates a new \code{Person}
#'         with specified data, and data from provided Fitbit account. If 
#'         provided, start_date and end_date must be characters with "%Y-%m-%d". 
#'         All defaults are \code{NA} - user can provide sources of data
#'         of interest.}}
#' 
#' @examples
#' library("lifelogr")
#'  
#' group_months <- data.frame("month" = c("Jan", "Feb", "Mar", "Apr", "May",
#'                                        "Jun", "Jul", "Aug",
#'                                        "Sep", "Oct", "Nov", "Dec"),
#'                                        "group" = c(0, 0, 0, 1, 1, 1, 1, 1, 
#'                                                    1, 0, 0, 0))
#' ash <- Person$new(user_info = list("name" = "Ash", "age" = 26,
#'                     "gender" = "female"), 
#'                     target_steps = 20000,
#'                     group_assignments = list("group_months" = group_months),
#'                     start_date = "2017-03-11",
#'                     end_date = "2017-03-12")
#'
#' \dontrun{                
#' bailey <- Person$new(fitbit_user_email = "bailey@gmail.com",
#'                  fitbit_user_pw = "baileypw",
#'                  #apple_data_file = "apple.csv",
#'                  start_date = "2017-03-11",
#'                  end_date = "2017-03-12")}
#'                  
#' @export
#' @format An \code{\link{R6Class}} generator object
#' 
Person <- R6::R6Class("Person",
  public = list(
    fitbit_daily = NULL, # dataframe of daily fitbit data
    fitbit_intraday = NULL, # dataframe of intraday fitbit data
    util = NULL, # util/admin data
    target_steps = NULL,
    start_date = NA, # optional start date of interest
    end_date = NA, # optional end date of interest
    user_info = NULL, # optional list of user info, such as "age", "gender", "name", etc.
    groupings = NULL, # named list of dataframes each with 2 columns: 1. a known 
    # variable (date, weekend, etc.) and 2. "group" with the group assignment for 
    # observations with that variable of that value
    apple = NULL, # apple data from csv
    addl_data = NULL, # user's own df of other data
    addl_data2 = NULL, # another df
    initialize = function(fitbit_user_email = NA, fitbit_user_pw = NA, 
                          user_info = NA, 
                          apple_data_file = NA,
                          target_steps = 10000,
                          addl_data = NA, addl_data2 = NA,
                          group_assignments = NA,
                          start_date = NA, end_date = NA) {
      self$addl_data <- addl_data
      self$addl_data2 <- addl_data2
      
      if (is.na(start_date)){
        start_date = "1990-01-01"
      }
      if (is.na(end_date)){
        end_date = Sys.Date()
      }
      
      self$start_date <- as.Date(strptime(start_date, format="%Y-%m-%d"))
      self$end_date <- as.Date(strptime(end_date, format="%Y-%m-%d"))
      self$target_steps <- target_steps
      self$user_info <- user_info
      self$util <- private$create_util_data(self$start_date, self$end_date)
  
      if (!is.na(apple_data_file)){
        self$apple <- private$load_apple_data(apple_data_file) 
        # user needs to pass in path from this directory to file
      }
      
      self$fitbit_intraday <- private$get_fitbit_intraday(fitbit_user_email,
                                                          fitbit_user_pw)
      self$fitbit_daily <- private$get_fitbit_daily(fitbit_user_email, 
                                                    fitbit_user_pw)
      self$groupings <- group_assignments
    }),
  
  private = list(
    load_apple_data = function(apple_data_file){
      raw_df <- readr::read_csv(apple_data_file)
      cleaned <- tibble::as_tibble(raw_df)
      
      # Remove Finish column because it's confusing
      cleaned$Finish <- NULL
      
      # Rename columns to match Person$data_intraday
      cleaned <- dplyr::rename(cleaned,
                               datetime = `Start`,
                               steps = `Steps (count)`,
                               floors = `Flights Climbed (count)`,
                               bpm = `Heart Rate (count/min)`,
                               distance = `Distance (mi)`,
                               resp_Rate = `Respiratory Rate (count/min)`,
                               active_cal = `Active Calories (kcal)`)

      # Convert datetime variable to dttm type
      cleaned$datetime <- lubridate::dmy_hm(cleaned$datetime)
      # fitbit steps data is in 15 min intervals, while Apple steps data is in
      # hour intervals
      cleaned$steps <- cleaned$steps / 4
      cleaned$distance <- cleaned$distance / 4
      return(cleaned)
    },
    
    create_util_data = function(start_date, end_date) {
      
      # date range between start and end date
      df <- tibble::data_frame(date = lubridate::ymd(as.Date(as.POSIXct(seq(from = start_date,
                                              to = end_date, by = 1)))))
      
      df$datetime <- df$date
      # MTWTFSS
      df$day_of_week <- lubridate::wday(df$date, label = TRUE)
      
      # weekend/weekday
      weekend <- c('Sat', 'Sun') 
      df$day_type <- factor((df$day_of_week %in% weekend),
                            levels=c(FALSE, TRUE), 
                            labels=c('weekday', 'weekend'))
      # Month
      df$month <- lubridate::month(df$date, label=TRUE)
      return(tibble::as_data_frame(df))
    }, 
    
    get_fitbit_intraday = function(fitbit_user_email, fitbit_user_pw) {
      
      cookie <- fitbitScraper::login(email=fitbit_user_email, password=fitbit_user_pw)
      
      start <- self$start_date
      end <- self$end_date
      
      char_start <- as.character(start)
      char_end <- as.character(end)
      
      # Build each intraday dataset: create list of lists, then rbind all 
      intraday <- list()
      intraday$isteps <- NULL
      intraday$idist <- NULL
      intraday$ifloors <- NULL
      intraday$iactive_min <- NULL
      intraday$ical_burn <- NULL
      intraday$ihr <- NULL
      
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
      joined <- tibble::as_data_frame(Reduce(function(x, y) merge(x, y, all=TRUE,
                                                                  by = "time"), 
                                             intraday))
      joined <- dplyr::select(joined, -dateTime)
      joined$datetime <- lubridate::ymd_hms(joined$time, tz = Sys.timezone())
      joined$date <- lubridate::ymd(as.Date(as.POSIXct(joined$datetime,
                                                       Sys.timezone())))
      joined$time <- lubridate::make_datetime(year = "1970", month = "01", 
                                              day = "01",
                                              hour = lubridate::hour(joined$datetime),
                                              min = lubridate::minute(joined$datetime), 
                                              sec = lubridate::second(joined$datetime))
      joined <- plyr::rename(joined, replace = c("active-minutes" = "activeMin"))
      joined$distanceKm <- joined$distance * MI_TO_KM
      joined$weightKg <- joined$weight * LB_TO_KG
      return(joined)
      },
     
      # Returns a tibble of joined variables recorded daily
      get_fitbit_daily = function(fitbit_user_email, fitbit_user_pw) {
        cookie <- fitbitScraper::login(email=fitbit_user_email, password=fitbit_user_pw)
        start <- self$start_date
        end <- self$end_date

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

        # manipulate date, time, and datetime
        daily$sleep$time <- as.POSIXct(daily$sleep$date)
        joined <- tibble::as_data_frame(Reduce(function(x, y) merge(x, y, all=TRUE, 
                                                                    by = "time"),
                                               daily))
        joined$date <- lubridate::ymd(as.Date(as.POSIXct(joined$time,
                                                         tz = Sys.timezone())))
        joined <- dplyr::select(joined, -time)
        joined$datetime <- joined$date
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
