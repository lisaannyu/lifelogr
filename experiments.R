# WANT: visualize when restless each night, norm to time of sleep or not

# computing variances of variables

# 5. Experimentation framework. The package will provide the framework for a user 
# to easily perform A/B or other testing on him/herself - for example, decide 
# he/she wants to know if his/her heart rate is higher in a statistically 
# significant way when alcohol or caffeine is consumed, and so mark each day/time 
# of consumption, then display the results of hypothesis testing on the desired field.

# select factor to assess, impact to assess, way to assess
# ex: impact of step count on restlessness of sleep
# factors to assess impact of options: amount slept, step count, distance, amount
# of exercise, time in heart rate zones, variable you put in yourself (tag specific
# days/hours/nights/mornings), day(s) of the week, months, seasons

# factors to assess impact on:
# resting heart rate, time in heart rate zones, amount of time restless, weight

# ways to assess
# plot one vs other, do a linear regression and anova, histogram? etc.

# Do the specified analysis of the impact of the variables on the measure
# person: Person object to do analysis on
# variables options:
# measure options:
# analysis options:
# vars.sources: each variable is assumed to be of type fitbit unless told otherwise
# meas.sources: each measured variable is assumed to be of type fitbit unless 
# told otherwise
experiment <- function(person, variables, measures,
                       analysis = c("plot", "correlation", 
                                    "anova", "t_test", "regression"),
                       vars.sources = NA, meas.sources = NA) {
  # NOTE: change sleep to time slept, amount restless. add exercise, own var
  #c("steps", "dist", "rest_hr","hr_zones", "sleep", "weight", own variable)
  
  # NOTE: if give own additional variable, need to check and make sure have it
  # NOTE: rename functions to be something relevant to this package (p_)
  
  # set to be fitbit sources if no sources given
  if (is.na(vars.sources)){
    vars.sources <- rep("fitbit", times = length(variables))
  }
  if (is.na(meas.sources)){
    meas.sources <- rep("fitbit", times = length(measures))
  }
  
  # NOTE: what happens when you enter multiple analyses?
  # call the type of analysis requested
  switch(analysis,
         "plot" = pplot(person, variables, measures, vars.sources, meas.sources), 
         "correlation" = correlation(person, variables, measures, 
                                     vars.sources, meas.sources),
         "anova" = panova(person, variables, measures, 
                               vars.sources, meas.sources),
         "t_test" = pttest(person, variables, measures, 
                         vars.sources, meas.sources),
         "regression" = pregression(person, variables, measures, 
                          vars.sources, meas.sources)
         # print error: your analysis didn't match any options
         )
}

  
# Collect all the variables and measures into one df
# NOTE: can make this tidy
# NOTE: won't work if isn't date variable (if is a time variable)
create_joined <- function(person, variables, measures, vars.sources, meas.sources){
  # rbind each dataset of interest together
  all_dfs <- list()
  
  for (i in 1:length(variables)){
    print(data.frame(person[[vars.sources[[i]]]][[variables[[i]]]]))
    all_dfs[[variables[[i]]]] <- data.frame(person[[vars.sources[[i]]]][[variables[[i]]]])
    }
  for (i in 1:length(measures)){
    all_dfs[[measures[[i]]]] <- data.frame(person[[meas.sources[[i]]]][[measures[[i]]]])
  }
  
  print(all_dfs)
  joined <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "date"), all_dfs)
  return(joined)
}

# NOTE: doesn't work with ggplot2:: for some reason
# NOTE: maybe shouldn't be directly printing plots
# NOTE: test what happens when variables aren't on the same time scale
pplot <- function(person, variables, measures, vars.sources, meas.sources){
  # plot each variable against each measure
  joined <- create_joined(person, variables, measures, vars.sources, meas.sources)
  #print(joined)
  # NOTE: maybe not do individual plots if desired
  for (i in 1:length(measures)){
    for (j in 1:length(variables)){
      print(ggplot(joined) +
              aes_string(x = variables[[j]],
                                  y = measures[[i]]) +
              geom_point() + ggtitle(paste(variables[[j]], "vs", measures[[i]])))
    }
  }
}


correlation <- function(person, variables, measures, vars.sources, meas.sources){
  joined <- create_joined(person, variables, measures, vars.sources, meas.sources)
  # print(joined)
  
  # correlations <- data.frame(variable = character(0),
  #                            measure = character(0), correlation = numeric(0))
  # names(correlations) <- c("variable", "measure", "correlation")
  # 
  # # compute correlation between each variable and each measure pair (select one of
  # for (i in 1:length(measures)){
  #   for (j in 1:length(variables)){
  #   
  #     correlations <- rbind(correlations, c("variable" = variables[[j]],
  #                                           "measure" = measures[[i]],
  #                                           "correlation" = 0.5),
  #                           stringsAsFactors = FALSE)
  #   }
  # }
  # # unclear why this gets modified...
  # names(correlations) <- c("variable", "measure", "correlation")
  # print(correlations)
  
  pearson_corr <- cor(joined[, variables], joined[, measures], method = "pearson")
  print(pearson_corr)
}

  

panova <- function(person, variables, measures, vars.sources, meas.sources){
  joined <- create_joined(person, variables, measures, vars.sources, meas.sources)
  print(joined)
  # for each measure, fit linear model with interactions, run anova
  for (i in 1:length(measures)){
    f <- paste(measures[[i]], " ~ ", 
               "(", paste(variables, collapse=" + "), ")^2", sep="")
    print(f)
    lin_model <- do.call("lm", list(as.formula(f), data=as.name("joined")))
    #print(summary(lin_model))
    lin_anova <- anova(lin_model)
    print(lin_anova)
  }
  
}


ttest <- function(person, variables, measures, vars.sources, meas.sources){
  
}


pregression <- function(person, variables, measures, vars.sources, meas.sources){
  joined <- create_joined(person, variables, measures, vars.sources, meas.sources)
  print(joined)
  # for each measure, fit linear model with interactions, run anova
  for (i in 1:length(measures)){
    f <- paste(measures[[i]], " ~ ", 
               "(", paste(variables, collapse=" + "), ")^2", sep="")
    print(f)
    lin_model <- do.call("lm", list(as.formula(f), data=as.name("joined")))
    print(summary(lin_model))
  }
}
  




  
  
  
  