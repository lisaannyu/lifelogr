#' @include global_var.R
#' @include experiments.R

group_months <- data.frame("month" = c("Jan", "Feb", "Mar", "Apr", "May",
                                        "Jun", "Jul", "Aug",
                                        "Sep", "Oct", "Nov", "Dec"),
                              "group" = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0))


# need to make this faster
RA <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 #apple_data_file = "apple.csv",
                 user_info = list("name" = "RA", "age" = 23, "gender" = "female"),
                 target_steps = 10000,
                 group_assignments = list("group_months" = group_months, data.frame(NA)),
                 start_date = "2017-03-11", end_date = "2017-03-12")

# should probably change input to fitbit_user_email
EX <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 # apple_data_file = "apple.csv",
                 user_info = list("name" = "EX", "age" = 29, "gender" = "male"),
                 target_steps = 10000,
                 group_assignments = list(data.frame(NA), data.frame(NA)),
                 start_date = "2017-01-19", end_date = "2017-02-17")

dataset <- create_dataset(person = RA,
                          all_variables = list("util" = c("month"),
                                               "fitbit_daily" = c("steps")), 
                          time_var = c("date"))


indiv_months <- data.frame("month"= c("Jan", "Feb", "Mar", "Apr", "May",
                                      "Jun", "Jul", "Aug",
                                      "Sep", "Oct", "Nov", "Dec"),
                           "group" = c(1:12))

td <- ttest(dataset, person = RA, 
            addl_grouping_assignments = list("indiv_months" = indiv_months), 
            names_of_groupings = c("group_months", "indiv_months"),
                  variables_to_compare = c("steps"))
  
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

# Building the package
pkgName <- "lifelogr"

# Save RA as EX for example purposes
# save(EX, file = "/Users/lisaannyu/GitHub/stats290-project/lifelogr/data/EX.rda") # might have to change the file directory

# Add documentation
devtools::document(pkg = pkgName)

# Add imports to DESCRIPTION
devtools::use_package("ggplot2", type = "Imports", pkg = pkgName)
devtools::use_package("shiny", type = "Imports", pkg = pkgName)
devtools::use_package("dplyr", type = "Imports", pkg = pkgName)
devtools::use_package("lubridate", type = "Imports", pkg = pkgName)
devtools::use_package("modelr", type = "Imports", pkg = pkgName)
devtools::use_package("stringr", type = "Imports", pkg = pkgName)
devtools::use_package("tidyr", type = "Imports", pkg = pkgName)
devtools::use_package("grDevices", type = "Imports", pkg = pkgName)
devtools::use_package("lazyeval", type = "Imports", pkg = pkgName)
devtools::use_package("stats", type = "Imports", pkg = pkgName)

# not sure if this should be imports or depends
devtools::use_package("tibble", type = "Imports", pkg = pkgName)

# use command line to do R CMD BUILD lifelogr
# use command line to do R CMD CHECK lifelogr._0...


############## TO DO/FOR REFERENCE ###################

# --- must do ------
# take in a dataframe
# figure out time, date, datetimes
# run t test if two groups in data
# documentation
# convert apple data
# error messages and other notes in code
# build package


# ----- lower priority --------------
# set up shiny tabs

# figure out how to aggregate hr zone data, time intervals 
# add 15 min bpm average column
# analyze sleep function for aggregating heart rate, etc. curves
# sleep experimentation - curves and when drops etc.
# visualize when awake/restless normed to start/not?



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
