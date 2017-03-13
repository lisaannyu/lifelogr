source("Person.R")


RA <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 user_info = list("name" = "RA", "age" = 23, "gender" = "female"),
                 start_date = "2017-02-09", end_date = "2017-03-10")

#could make var names more friendly in the experiment fn (replace variables)
# should make analysis a list of analyses to do (experiment switch statement)
experiment(person = RA, variables = c("steps", "sleepDuration"),
           measures = c("distance", "rest_hr", "minsRestlessAwake"), 
           analysis = "plot") #vars.sources = NA, meas.sources = NA)


source("experiments.R")
experiment(person = RA, variables = c("steps", "sleepDuration"),
           measures = c("distance", "rest_hr", "minsRestlessAwake"), 
           analysis = "anova") #vars.sources = NA, meas.sources = NA)


# identifying weekend/weekdays? dealing with those columns before pulled out?
# visualize when awake/restless normed to start/not?
# convert dataframe to tidy?
# should probably have an experiment class so don't have to keep joining variables 
# in each function...(and running lm for regression and anova)