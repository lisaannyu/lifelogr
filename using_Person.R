source("Person.R")


RA <- Person$new(user_email = "rohisha@gmail.com", user_pw = "datasamplepw",
                 user_info = list("name" = "RA", "age" = 23, "gender" = "female"),
                 start_date = "2017-03-09", end_date = "2017-03-10")

source("experiments.R")
experiment(person = RA, variables = c("steps"), measures = c("distance", "weight"), 
           analysis = c("plot")) #vars.sources = NA, meas.sources = NA)
