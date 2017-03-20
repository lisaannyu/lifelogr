#' Merge a list of lists into one list
#' 
#' @description
#' 
#' @param list_of_lists list of lists, each with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @return one list, with structure list(source1 = c(var1, var2), 
#'                                       source2 = c(var3, var4)),
#'         where variables from the same source have been grouped in that 
#'         source's sublist
#' @export
#' @examples
#' variables = list("fitbit_intraday" = c("steps"), 
#'                 "fitbit_daily" = c("sleepDuration"),
#'                 "util" = c("day_of_week", "day_type", "month"))
#' measures = list("fitbit_daily" = c("distance", "restingHeartRate"))
#' all_variables <- merge_lists(list(variables, measures))
#' 
merge_lists <- function(list_of_lists){
  keys <- unique(unlist(lapply(list_of_lists, names)))
  merged_list <- setNames(do.call(mapply,
                                    c(FUN=c, 
                                      lapply(list_of_lists, `[`, keys))), keys)
  return(merged_list)
}
  
  
#' Do the specified analysis of the impact of the variables on the measure
#' 
#' @description Performs the analysis specified on the variables (X) and
#' measures (Y).
#' 
#' @param Person an instantiated Person object
#' @param variables
#' @param measures
#' @param analysis
#' @param time_var
#' @return NULL - results of analysis chosen are printed
#' @export
#' @examples
#' data(EX)
#' plot_daily_all(EX)
#'
experiment <- function(person, variables, measures,
                       analysis = c("plot", "correlation", 
                                    "anova", "compare_groups", "regression"),
                       time_var = c("time", "date", "datetime")) {
  
  # create dataset
  dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                     measures)),
                            time_var = time_var)

  # call the type of analysis requested
  for (type in analysis){
    switch(type,
           "plot" = l_plot(dataset, person, variables, measures, time_var), 
           "correlation" = correlation(dataset, person, variables, measures, 
                                       time_var),
           "anova" = l_anova(dataset, person, variables, measures, time_var),
           "compare_groups" = compare_groups(dataset, person, variables, measures),
           "regression" = l_regression(dataset, person, variables, measures, 
                                       time_var)
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
  }
  
  dataset <- Reduce(function(x, y) merge(x, y, all=TRUE, by = time_var), all_dfs)
  return(dataset)
}


# can pass in person and variables of interest by name, or pass in a dataset
# that has already been created by create_dataset, or call experiment and 
# it'll call this for you
# NOTE: maybe shouldn't be directly printing plots
# NOTE: test what happens when variables aren't on the same time scale
l_plot <- function(dataset = NA, person, variables, measures, time_var){
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

correlation <- function(dataset = NA, person, variables, measures, time_var){
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                       measures)),
                              time_var = time_var)
    }
  
  pearson_corr <- cor(dataset[, unlist(variables)], dataset[, unlist(measures)],
                      method = "pearson")
  print(pearson_corr)
  return(pearson_corr)
}

  
l_anova <- function(dataset = NA, person, variables, measures, time_var){
  anovas <- list()
  
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
    lin_model <- do.call("lm", list(as.formula(f), data=as.name("dataset")))
    lin_anova <- anova(lin_model)
    print(f)
    print(lin_anova)
    anovas <- c(anovas, lin_anova)
  }
  return(anovas)
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
compare_groups <- function(dataset, person, names_of_groupings = NA, 
                  addl_grouping_assignments = NA, variables_to_compare){

  if (all(is.na(names_of_groupings))){
    names_of_groupings <- names(person$groupings)
  }
  
  # append addl_grouping_assignments to person's grouping assignments
  all_group_maps <- c(person$groupings, addl_grouping_assignments)
  
  # From the dataset, keep the variables to compare, join on each groupings 
  # assignment with the column labeled by the groupings name
  for (grouping in names_of_groupings){
    
    # join this grouping onto the dataset
    merge_var <- names(all_group_maps[[grouping]])[names(all_group_maps[[grouping]]) != "group"]
    g_dataset <- merge(dataset, all_group_maps[[grouping]], by = merge_var)

    # for each variable in variables to compare
    group_stats <- function(variable){
      print(variable)
      print(dplyr::summarise_(dplyr::group_by(g_dataset, group),
                mean = lazyeval::interp(~mean(v), v=as.name(variable)),
                sd = lazyeval::interp(~sd(v), v=as.name(variable))))
    }
    
    lapply(variables_to_compare, group_stats)
  }
  # RETURN EACH stats
  return(dataset)
  
}


l_regression <- function(dataset = NA, person, variables, measures, time_var){
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