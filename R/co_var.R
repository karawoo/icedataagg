#####################################################
####  Function to find coefficient of variation  ####
#####################################################

co_var <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
