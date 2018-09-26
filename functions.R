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