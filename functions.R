# function for Sub-exercise 3-a
# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # set a defaut value to return
  result <- NULL
  # access a column
  x <- d[[var]] 
  # tests whether the column exists in d
  if (!is.null(x)) {
    # if x contains numbers
    if (is.numeric(x)) {
      #, set the variable to be the sum of the values in x
      result <- sum(x)
    }
  }
  # return the result
  return(result)
}


# function for Sub-exercise 3-b
# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_sum <- function(x) {
  # set a default value to return
  result <- NULL
  # this tests whether the vector is null
  if (!is.null(x)) {
    # this tests whether the vector contains numbers
    if (is.numeric(x)) {
      # set the variable result to be 0
      result <- 0
      # then iterate the vector
      for (number in x) {
        # sum up every element in this vector
        result <- result + number
      }
    }
  }
  # return the final result
  return(result)
}


# function for Sub-exercise 3-c
# Sum values in a vector then divide by another number 
#
# ARGUMENTS:
# x: a vector
# k: a number
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values divided by a number k; otherwise, returns NULL
# we should note that k can not be 0 and its length is 1
#
# [YOUR FUNCTION HERE]
sum_divided_by <- function(x, k) {
  # do the calculate unless k is a number!=0 and the result of my_sum(x)!=NULL
  if (is.numeric(k) && !is.null(my_sum(x)) && k!=0) {
    # return the result of division
    return(my_sum(x)/k)
  # the other cases
  }else{
    # return NULL
    return(NULL)
  }
}


# function for Sub-exercise 3-d
# Give the mean of a vector
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the mean of
# all values of this vector; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_mean <- function(x) {
  # if the result of my_sum(x) is not NULL
  if (!is.null(my_sum(x))) {
    # return the result of sum_divided_by(x,len(x))
    return(sum_divided_by(x, length(x)))
  }
  # otherwise, return NULL
  return(NULL)
}


# function for Sub-exercise 3-d
# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
#     string
# grouping_var: the name of a column of d containing a grouping variable,
#               provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var, x=grouping_var, fill=grouping_var))
  # YOUR CODE HERE: Create a violin plot
  p <- p + ggplot2::geom_violin()
  return(p) 
}


# function for Sub-exercise 5-a
# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  # YOUR CODE HERE: assign the difference in the medians to to the variable 'result'
  result <- median(d_1[[var]])-median(d_2[[var]])
  return(result)
}


# function for Sub-exercise 5-b
# function to generate a shuffled version of a vector
randomize_rts <- function(d){
  # the coding doesn't work so I've changed it a little bit
  result <- sample(d, length(d))
  return(result)
}
# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  # YOUR CODE HERE: generate a shuffled version of d[[var]]
  # sample : take a number randomly from d[[var]] and doesn't put it back
  d[[var]] <- randomize_rts(d[[var]])
  return(d)  
}


# function for Sub-exercise 5-c
# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    #                 fill in the vector permutation_statistics with the
    #                 value of statistic(...) for this new permutation
    new_data <- randomize(d, var)
    permutation_statistics[i] <- statistic(new_data, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}


# function for Sub-exercise 5-d
# Perform a permutation test for two groups.

permutation_twogroups_2 <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # the only difference between the function 5-c and 5-d is that we randomize on grouping_var instead of var
    new_data <- randomize(d, grouping_var)
    permutation_statistics[i] <- statistic(new_data, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}