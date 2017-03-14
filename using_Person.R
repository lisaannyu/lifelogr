source("Person.R")


RA <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 user_info = list("name" = "RA", "age" = 23, "gender" = "female"),
                 target_steps = 10000,
                 start_date = "2017-02-09", end_date = "2017-03-10")

#could make var names more friendly in the experiment fn (replace variables)
# should make analysis a list of analyses to do (experiment switch statement)


source("experiments.R")
experiment(person = RA, variables = c("steps", "sleepDuration"),
           measures = c("distance", "rest_hr", "minsRestlessAwake"), 
           analysis = "anova") #vars.sources = NA, meas.sources = NA)


# identifying weekend/weekdays? dealing with those columns before pulled out?
# visualize when awake/restless normed to start/not?
# convert dataframe to tidy?
# should probably have an experiment class so don't have to keep joining variables 
# in each function...(and running lm for regression and anova)

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
