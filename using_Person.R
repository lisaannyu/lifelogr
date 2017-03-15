source("Person.R")

# need to make this faster
RA <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 user_info = list("name" = "RA", "age" = 23, "gender" = "female"),
                 target_steps = 10000,
                 group_assignments = list(data.frame(NA), data.frame(NA)),
                 start_date = "2017-02-09", end_date = "2017-03-12")

source("experiments.R")

# Can run an experiment all together with desired Person, variables/measures, 
#and analyses
# will get an error with non numeric variables and correlation/anova/regression
# (fix interaction/this for correlation?)
experiment(person = RA, variables = c("steps", "sleepDuration", 
                                      "day_of_week", "day_type", "month"),
           measures = c("distance", "rest_hr", "minsRestlessAwake"), 
           analysis = c("plot"), #, "correlation", "anova", "regression"),
           vars.sources = c(rep("fitbit", 2), rep("util", 3)),
           meas.sources = c(rep("fitbit", 3)))

# Or can create just the joined dataset
dataset <- create_dataset(person = RA,
                          all_variables = c("steps", "sleepDuration", "distance",
                                            "rest_hr", "minsRestlessAwake"),
                          all_sources = rep("fitbit", 5))

# Then run each analysis on that dataset separately
correlation_df <- correlation(dataset, person = RA, 
                              variables = c("steps", "sleepDuration"),
                              measures = c("distance", "rest_hr",
                                           "minsRestlessAwake"),
                              vars.sources = rep("fitbit", 2), 
                              meas.sources = rep("fitbit", 3))

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
############## TO DO/FOR REFERENCE ###################


# add capability for t testing based on two groups in additional data

# need to add capability for experimenting with time axis variables

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
