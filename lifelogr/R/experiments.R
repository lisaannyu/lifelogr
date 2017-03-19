# WANT: visualize when restless each night, norm to time of sleep or not

#could make var names more friendly in the experiment fn (replace variables)

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



# Returns lists, merged by first level
merge_lists <- function(list_of_lists){
  keys <- unique(unlist(lapply(list_of_lists, names)))
  merged_list <- setNames(do.call(mapply,
                                    c(FUN=c, 
                                      lapply(list_of_lists, `[`, keys))), keys)
  return(merged_list)
}
  
  
# Do the specified analysis of the impact of the variables on the measure
# person: Person object to do analysis on
# variables options: named list for each source, with variables desired from that source
# measure options:
# analysis options:
# told otherwise
# NOTE: verify how to require one of the options for time_vars


experiment <- function(person, variables, measures,
                       analysis = c("plot", "correlation", 
                                    "anova", "t_test", "regression"),
                       time_var = c("time", "date", "datetime")) {
  # NOTE: change sleep to time slept, amount restless. add exercise, own var
  #c("steps", "dist", "rest_hr","hr_zones", "sleep", "weight", own variable)
  
  # NOTE: if give own additional variable, need to check and make sure have it
  # NOTE: rename functions to be something relevant to this package (p_)
  
  # set to be fitbit sources if no sources given
  # NEED TO FIX SINCE COULD BE INTRADAY OR DAILY
  # if (is.na(vars.sources)){
  #   vars.sources <- rep("fitbit", times = length(variables))
  # }
  # if (is.na(meas.sources)){
  #   meas.sources <- rep("fitbit", times = length(measures))
  # }
  
  # create dataset
  dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                     measures)),
                            time_var = time_var)

  # print(dataset)
  # call the type of analysis requested
  for (type in analysis){
    switch(type,
           "plot" = pplot(dataset, person, variables, measures, time_var), 
           "correlation" = correlation(dataset, person, variables, measures),
           "anova" = panova(dataset, person, variables, measures),
           "t_test" = pttest(dataset, person, variables, measures),
           "regression" = pregression(dataset, person, variables, measures)
           # print error: your analysis didn't match any options
           )
  }
}

# example: all_variables = list("fitbit_daily" = c("x", "y", "z"), "util" = c("d", "e", "f"))
# Collect all the variables and measures into one df - across Person's types of data
# NOTE: time_var must exist in each one of all_sources
# Make sure should be specifying time_var options as such
create_dataset <- function(person, all_variables,
                           time_var = c("time", "date", "datetime")){
  all_dfs <- list()
  # for each source (name of a df), grab columns from that source + time_var
  for (source in names(all_variables)){
    all_dfs[[source]] <- person[[source]][, c(time_var, all_variables[[source]])]
    print(all_variables[[source]])
  }
  
  dataset <- Reduce(function(x, y) merge(x, y, all=TRUE, by = time_var), all_dfs)
  return(dataset)
}


# can pass in person and variables of interest by name, or pass in a dataset
# that has already been created by create_dataset, or call experiment and 
# it'll call this for you
# NOTE: maybe shouldn't be directly printing plots
# NOTE: test what happens when variables aren't on the same time scale
pplot <- function(dataset = NA, person, variables, measures, time_var){
  # plot each variable against each measure
  if (!is.data.frame(dataset)){
    # need to put the combining thing in
    dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                       measures)),
                              time_var = time_var)
  }

  for (meas.source in names(measures)){
    for (measure in measures[[meas.source]]){
      for (var.source in names(variables)){
        for (variable in variables[[var.source]]){

          print(ggplot2::ggplot(dataset) +
                  ggplot2::aes_string(x = variable, y = measure) +
                  ggplot2::geom_point() + ggplot2::ggtitle(paste(variable, 
                                                                 "vs", measure)))
          
        }
      }
    }
  }
}


correlation <- function(dataset = NA, person, variables, measures){
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                       measures)),
                              time_var = time_var)
    
    }
  
  pearson_corr <- cor(dataset[, unlist(variables)], dataset[, unlist(measures)], method = "pearson")
  print(pearson_corr)
  return(pearson_corr)
}

  
# should be printing output?
panova <- function(dataset = NA, person, variables, measures){
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                       measures)),
                              time_var = time_var)
  }
  measures_flat <- unlist(measures)
  variables_flat <- unlist(variables)
  # for each measure, fit linear model with interactions, run anova
  for (i in 1:length(measures_flat)){
    f <- paste(measures_flat[[i]], " ~ ", 
               "(", paste(variables_flat, collapse=" + "), ")^2", sep="")
    print(f)
    lin_model <- do.call("lm", list(as.formula(f), data=as.name("dataset")))
    lin_anova <- anova(lin_model)
    print(lin_anova)
  }
  # should return lists of anovas?
}

# Groupings is an optional list of group assignments to do the ttest on - 
# otherwise, does the test on each of the group_assignments person contains
# dataset, if passed in, is the dataset with everything in it except the group
# assignments variables
# compares (averages, t test if only two, variance, ?plot, ?histogram) 
#  each variable in variables_to_compare between the groups in groupings
# For now, variables is a simple list, 1D, no sources
# group assignments variable has to be named 'group'
# names_of_groupings are names of groupings want to analyze (must be in person
# named that way or passed in in addl)
# Maybe should be able to pass groupings_df directly to this function
ttest <- function(dataset = NA, person, names_of_groupings = NA, 
                  addl_grouping_assignments = NA, variables_to_compare){
  if (!is.data.frame(dataset)){
    # PRINT ERROR
    # don't want users to have to think at the level of time variables so requiring
    # them to pass in a dataset but debatable
  }
  if (all(is.na(names_of_groupings))){
    names_of_groupings <- names(person$groupings)
  }
  
  # append addl_grouping_assignments to person's grouping assignments
  all_group_maps <- c(person$groupings, addl_grouping_assignments)
  
  # From the dataset, keep the variables to compare, join on each groupings 
  # assignment with the column labeled by the groupings name
  # dataset <- dataset[, variables_to_compare]
  for (grouping in names_of_groupings){
    print(grouping)
    # join this grouping onto the dataset
    merge_var <- names(all_group_maps[[grouping]])[names(all_group_maps[[grouping]]) != "group"]
    g_dataset <- merge(dataset, all_group_maps[[grouping]], by = merge_var)

    # for each variable in variables to compare
    compare_groups <- function(variable){
      print(variable)
      print(dplyr::summarise_(dplyr::group_by(g_dataset, group),
                mean = lazyeval::interp(~mean(v), v=as.name(variable)),
                sd = lazyeval::interp(~sd(v), v=as.name(variable))))
    }
    
    lapply(variables_to_compare, compare_groups)
  }
  return(dataset)
  
}


pregression <- function(dataset = NA, person, variables, measures){
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                       measures)),
                              time_var = time_var)
  }
  
  measures_flat <- unlist(measures)
  variables_flat <- unlist(variables)
  # for each measure, fit linear model with interactions, run anova
  for (i in 1:length(measures_flat)){
    f <- paste(measures_flat[[i]], " ~ ", 
               "(", paste(variables_flat, collapse=" + "), ")^2", sep="")
    print(f)
    lin_model <- do.call("lm", list(as.formula(f), data=as.name("dataset")))
    print(summary(lin_model))
  }
}
  




  
  
  
  