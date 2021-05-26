#' Define a study phase
#'
#' @param n Number of subjects to accrue before the phase ends.
#' @param rule A [rule] for determining treatment allocation.
#'
#' @seealso [study] which are formed from phases.
#' @export
phase <- function(n, rule) {
  n <- vctrs::vec_cast(n, integer(), x_arg = "n")
  new_phase(n, rule)
}

new_phase <- function(n, rule) {
  stopifnot(rlang::is_scalar_integer(n), is_rule(rule))
  structure(list(n = n, rule = rule), class = "bat_phase")
}

is_phase <- function(x) inherits(x, "bat_phase")

all_are_phases <- function(x) {
  all(vapply(x, is_phase, logical(1)))
}
