#' Define a study
#' @seealso [simulate_study()] for running a simulation of a study.
#' @name study
NULL

#' @inheritDotParams phase
#' @rdname study
#' @export
single_phase_study <- function(...) {
  new_study(list(phase(...)), class = "bat_single_phase_study")
}

#' @param phases A list of [phase]s that form the study.
#' @rdname study
#' @export
multi_phase_study <- function(phases = list()) {
  stopifnot(all_are_phases(phases))
  new_study(phases, class = "bat_multi_phase_study")
}

new_study <- function(phases = list(), class = character()) {
  structure(list(phases = phases), class = c(class, "bat_study"))
}
