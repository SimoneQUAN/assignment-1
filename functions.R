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
  # Set a default value to return
  result <- NULL
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    # YOUR CODE HERE: if x contains numbers, set the variable
    # result to be the sum of the values in x
    if (check_numeric(x)) {
      result <- sum(x)
    }
    # You will need to do a bit of research in order to figure out how
    # to test whether a vector contains numbers.
  }
  # YOUR CODE HERE: return the result
  return(result)
}

my_sum <- function(vecteur) {
  for ()
}