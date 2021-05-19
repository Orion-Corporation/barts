
rbern <- function(n, p) {
  stats::runif(n) < p
}

# Much faster creation than with data.frame()
# https://stackoverflow.com/a/18748069/4550695
new_data_frame <- function(x, nrow) {
  structure(x, row.names = c(NA, -nrow), class = "data.frame")
}
