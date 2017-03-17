source("Person.R")

# need to make this faster
RA <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 user_info = list("name" = "RA", "age" = 23, "gender" = "female"),
                 target_steps = 10000,
                 group_assignments = list(data.frame(NA), data.frame(NA)),
                 start_date = "2017-02-09", end_date = "2017-03-12")

EX <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 user_info = list("name" = "EX", "age" = 29, "gender" = "male"),
                 target_steps = 10000,
                 group_assignments = list(data.frame(NA), data.frame(NA)),
                 start_date = "2017-01-19", end_date = "2017-02-17")


source("experiments.R")
# Can run an experiment all together with desired Person, variables/measures, 
#and analyses
# will get an error with non numeric variables and correlation/anova/regression
# (fix interaction/this for correlation?)
experiment(person = RA, variables = list("fitbit_intraday" = c("steps"), 
                                         "fitbit_daily" = c("sleepDuration"), 
                                      "util" = c("day_of_week", "day_type", "month")),
           measures = list("fitbit_daily" = c("distance", "restingHeartRate")),# , "minsRestlessAwake"), 
           analysis = c("plot"), #, "correlation", "anova", "regression"),
           time_var = c("date"))


# Or can create just the joined dataset

dataset <- create_dataset(person = RA,
                          all_variables = list("fitbit_intraday" = c("steps", 
                                                                     "bpm")), 
                          time_var = c("datetime"))

# going across dates and datetime variables is an unsolved issue...

dataset <- create_dataset(person = RA,
                          all_variables = list("util" = c("day_of_week",
                                                          "day_type"),
                                               "fitbit_daily" = c("steps", 
                                                            "sleepDuration", 
                                                            "distance",
                                                            "restingHeartRate")), 
                          time_var = c("date"))

# Then run each analysis on that dataset separately
pplot(dataset, person = RA, variables = list("fitbit_daily" = c("sleepDuration",
                                                                   "steps",
                                                                   "distance"), 
                                             "util" = c("day_of_week", 
                                                        "day_type")),
      measures = list("fitbit_daily" = c("restingHeartRate")), 
      time_var = c("date"))

correlation_df <- correlation(dataset, person = RA, 
                              variables = list("fitbit_daily" = c("sleepDuration",
                                                        "steps",
                                                        "distance")),
                              measures = list("fitbit_daily" = c("restingHeartRate")))

panova(dataset, person = RA, variables = list("fitbit_daily" = c("sleepDuration",
                                                                  "steps",
                                                                  "distance")),
                              measures = list("fitbit_daily" = c("restingHeartRate")))
pregression(dataset, person = RA, variables = list("fitbit_daily" = c("sleepDuration",
                                                                 "steps",
                                                                 "distance")),
       measures = list("fitbit_daily" = c("restingHeartRate")))


# Sleep visualizations
source("viz_sleep.R")
# Would like to create a suite of plots in R (like lm's 4 diagnostic plots)
# Will create a switch table too later
plot_sleep_over_time(RA)
plot_sleep_restless_prop(RA)
plot_sleep_restless_min(RA)
plot_sleep_quality(RA)
plot_sleep_weekday(RA)
plot_sleep_start_end(RA, "day_type")
plot_sleep_start_end(RA, "day_of_week")


# Building the package
pkgName <- "fitbitData"

# Save RA as EX for example purposes
save(EX, file = "../data/EX.rda") # might have to change the file directory

# Add imports to DESCRIPTION
devtools::use_package("ggplot2", type = "Imports", pkg = pkgName)
devtools::use_package("shiny", type = "Imports", pkg = pkgName)
############## TO DO/FOR REFERENCE ###################

# analyze sleep function for aggregating heart rate, etc. curves

# add capability for t testing based on two groups in additional data


# sleep experimentation - curves and when drops etc.
# visualize when awake/restless normed to start/not?

# figure out how to aggregate hr zone data, time intervals 


# 
# For the date-times, can you create POSIXct objects?
# 
# Like so: lubridate::ymd_hms(df$startDateTime, tz = Sys.timezone())
# 
# And for dates you can just use lubridate::ymd(df$date) to create Date objects
# 
# For the time data frame, could you also have another variable called time_only or something like that?  And then we can use this cheater way:
#   
# data$time <- lubridate::make_datetime(year = "1970", month = "01", day = "01",
#                                       hour = lubridate::hour(data$time),
#                                       min = lubridate::minute(data$time))
# #person$fitbit$daily gives dataframe with names "date", "rest_hr", "steps", etc.
#person$fitbit[, c("date", "steps")]

#person$fitbit$intraday

#person$fitbit$steps gives df with names date, steps
#lubridate

# names(RA$fitbit)
# [1] "isteps"           
# [2] "idist"            
# [3] "ifloors"          
# [4] "iactive_min" 
# [5] "ical_burn"        
# [6] "ihr"              
# [7] "steps"            
# [8] "dist"             
# [9] "distance"         
# [10] "floors"           
# [11] "minsVery"         
# [12] "cal_ratio"        
# [13] "rest_hr"          
# [14] "hr_zones"         
# [15] "sleep"            
# [16] "weight"           
# [17] "sleepDuration"    
# [18] "minsRestlessAwake"
# 
