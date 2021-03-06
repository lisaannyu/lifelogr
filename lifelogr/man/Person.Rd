% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Person.R
\docType{class}
\name{Person}
\alias{Person}
\title{A \code{Person} object is a complete view of an individual over a certain
time period, as seen through data from multiple sources}
\format{An \code{\link{R6Class}} generator object}
\usage{
Person
}
\description{
\code{Person} is an object that encapsulates an individual's 
data over a specified date range (start and end date stored as \code{Date} 
objects.
An individual consists of basic information, such as name, age,
and gender (a \code{list} with named elements), data from their self-tracking 
devices such as Fitbit, Apple health, etc. (data from each source is a tibble
dataframe), individual goals such as target steps (\code{numeric}), 
additional data from self-tracking apps or one's own collection system 
(stored as a tibble dataframe), and ways of grouping the data a user may be 
interested in, such as grouping by seasons, or comparing weekend to weekday 
behavior and health (stored as a list of named dataframes, which each contain 
group assignments).
}
\section{Fields}{

\describe{
\item{\code{fitbit_daily}}{tibble dataframe of fitbit variables (for user account
info provided) observed daily.
Columns include:
 \itemize{
     \item{date: unique for each row (date)}
     \item{datetime: includes date and time, time is an arbitrary 
     time, which is consistent for each day (date)}
     \item{dateInForJavascriptLocalFormatting: chr}
     \item{steps: total number of steps for that day (dbl)}
     \item{distance: total distance for that day, in miles (dbl)}
     \item{distanceKm: total distance for that day, in kilometers 
     (dbl)}
     \item{floors: total number of floors for that day (dbl)}
     \item{minutesVery: minutes 'very active' that day (dbl)}
     \item{caloriesBurned: calories (kcal) burned that day (dbl)}
     \item{caloriesIntake: calories (kcal) consumed that day, user
     must input this, either into this data frame or into the fitbit 
     (dbl)}
     \item{restingHeartRate: resting heart rate in beats per minute 
     (bpm) (dbl)}
     \item{startTime: sleeping start time for that day (chr)}
     \item{endTime: sleeping end time for that day (chr)}
     \item{startDateTime: sleeping start date and time for that day
     (chr)}
     \item{endDateTime: sleeping end date and time for that day (chr)}
     \item{sleepDuration: sleep duration for that day, in minutes 
     (int)}
     \item{sleepDurationHrs: sleep duration for that day, in hours 
     (dbl)}
     \item{minAsleep: time asleep that day, in minutes (int)}
     \item{minAsleepHrs: time asleep that day, in hours, derived from 
     minAsleep (dbl)}
     \item{minRestlessAwake: \eqn{sleepDuration - awakeCount} (int)}
     \item{awakeCount: int}
     \item{restlessCount: int}
     \item{awakeDuration: int}
     \item{restlessDuraton: int}
     \item{restlessProp: proportion of sleep spent restless, 
     calculated as \eqn{restlessProp = (sleepDurationHrs - 
     minAsleepHrs) / sleepDurationHrs * 100} (dbl)}
     \item{sleepQualityScoreB: dbl}
     \item{sleepQualityScoreA: int}
     \item{sleepQualityGraphicPercentA: dbl}
     \item{sleepQualityGraphicPercentB: dbl}
     \item{sleepBucketTextB: one of "ok", "good", "great" (chr)}
     \item{sleepBucketTextA: one of "ok", "good", "great" (chr)}
     \item{clusters: list of chr}
     \item{breaks: list of chr}
  }}

\item{\code{fitbit_intraday}}{tibble dataframe of fitbit variables (for user account
info provided) observed multiple times a day.
Columns include:
\itemize{
     \item{date: unique for each row (date)}
     \item{time: a combination of an arbitrary date ("1970-01-01") 
     and the time of the observation, generally in 5 minute intervals 
     (dttm)}
     \item{datetime: includes date from `date` and time from `time`
     (dttm)}
     \item{steps: number of steps in 15 minute interval (dbl)}
     \item{distance: distance traveled in 15 minute interval, in 
     miles (dbl)}
     \item{distanceKm: distance traveled in 15 minute interval, in
     kilometers (dbl)}
     \item{floors: number of floors went up and down in 15 minute
     interval (dbl)}
     \item{activeMin: number of active minutes in 15 minute 
     interval (dbl)}
     \item{activityLevel:  hypothesized activity level, one of: 
     "SEDENTARY", "LIGHTLY_ACTIVE", "MODERATELY_ACTIVE", or 
     "VERY_ACTIVE" (chr)}
     \item{bpm: average heart rate in 5 minute interval (int)}
     \item{confidence: one of -1, 1, 2, or 3 (int)}
     \item{caloriesBurned: calories (kcal) burned in 5 minute 
     interval (dbl)}
     \item{defaultZone: chr}
     \item{customZone: lgl}
     \item{weight: weight, in lbs (dbl)}
     \item{weightKg: weight, in kg (dbl)}
}}

\item{\code{util}}{tibble dataframe that maps each date in the date range to utility
information about that date
Columns include:
\itemize{
     \item{date: unique for each row (date)}
     \item{datetime: date from `date` and an arbitrary time (16:00:00)
     (dttm)}
     \item{day_of_week: day of the week, with Sun as first (ord)}
     \item{day_type: weekend or weekday (fctr)}
     \item{month: month, with Jan as first (ord)}
}}

\item{\code{target_steps}}{the person's target number of steps (numeric) for each
day (default 10,000)}

\item{\code{start_date}}{start of user's date range of interest (Date object)}

\item{\code{end_date}}{end of user's date range of interest (Date object)}

\item{\code{user_info}}{provided user info, such as "age", "gender", "name" (list)}

\item{\code{groupings}}{named list of dataframes, each with two columns - a known 
variable, and group, with the group assignment for observations where 
that variable has appropriate value}

\item{\code{apple}}{tibble dataframe of user's provided Apple Health data.  These
columns depend on which columns are passed in by the user.  However,
these columns match fitbit columns:
\itemize{
    \item{datetime: dttm}
    \item{steps: Original steps data for total number of steps in 60 
    minutes, but divided by 4 to match fitbit steps data, which is
    the total number of steps in 15 minutes (dbl)}
    \item{distance: Average distance in 15 minutes in miles.  
    Original distance data for total distance in 60 minutes, 
    but divided by 4 to match fitbit distance data, which is the total 
    distance in 15 minutes (dbl)}
    \item{distanceKm: Average distance in 15 minutes in km.  
    Original distance data for total distance in 60 minutes, 
    but divided by 4 to match fitbit distance data, which is the total 
    distance in 15 minutes (dbl)}
    \item{floors: Average number of floors in 15 minutes.  
    Original floors data for total number
     of floors in 60 minutes, but divided by 4 to match fitbit floors 
     data, which is the total number of floors in 15 minutes(dbl)}
    \item{bpm: average heart rate for the given hour, most users will
     not have this data (dbl)}
}}

\item{\code{addl_data}}{dataframe of data from another source provided by user}

\item{\code{addl_data2}}{dataframe of data from another source provided by user}
}}

\section{Methods}{


\describe{
  \item{\code{Person$new(fitbit_user_email, fitbit_user_pw, user_info = NA,
                          apple_data_file, target_steps, addl_data,
                          addl_data2, group_assignments, start_date, 
                          end_date)}}{Creates a new \code{Person}
        with specified data, and data from provided Fitbit account. If 
        provided, start_date and end_date must be characters with "%Y-%m-%d". 
        All defaults are \code{NA} - user can provide sources of data
        of interest.}}
}

\examples{
library("lifelogr")
 
group_months <- data.frame("month" = c("Jan", "Feb", "Mar", "Apr", "May",
                                       "Jun", "Jul", "Aug",
                                       "Sep", "Oct", "Nov", "Dec"),
                                       "group" = c(0, 0, 0, 1, 1, 1, 1, 1, 
                                                   1, 0, 0, 0))
ash <- Person$new(user_info = list("name" = "Ash", "age" = 26,
                    "gender" = "female"), 
                    target_steps = 20000,
                    group_assignments = list("group_months" = group_months),
                    start_date = "2017-03-11",
                    end_date = "2017-03-12")

\dontrun{                
bailey <- Person$new(fitbit_user_email = "bailey@gmail.com",
                 fitbit_user_pw = "baileypw",
                 #apple_data_file = "apple.csv",
                 start_date = "2017-03-11",
                 end_date = "2017-03-12")}
                 
}
\keyword{datasets}
