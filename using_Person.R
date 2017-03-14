source("Person.R")


RA <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 user_info = list("name" = "RA", "age" = 23, "gender" = "female"),
                 target_steps = 10000,
                 start_date = "2017-02-09", end_date = "2017-03-10")

source("experiments.R")

# Can run an experiment all together with desired Person, variables/measures, 
#and analyses
experiment(person = RA, variables = c("steps", "sleepDuration"),
           measures = c("distance", "rest_hr", "minsRestlessAwake"), 
           analysis = c("plot", "correlation",
                        "anova", "regression")) #vars.sources = NA, meas.sources = NA)

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


############## TO DO/FOR REFERENCE ###################

# identifying weekend/weekdays? dealing with those columns before pulled out?
# visualize when awake/restless normed to start/not?

# add weekday day of week weekend etc as variables in person

# sleep experimentation - curves and when drops etc.



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
