#' Simulate a study
#'
#' @param study A [study] whose phases will be simulated in sequence.
#' @param theta A vector of true event probabilities in each arm.
#' @param a,b Vectors of parameters to the beta prior with elements for each
#'   arm. Recycled to match the length of `theta` if possible.
#' @param n_draws The number of draws to sample from the posterior for
#'   evaluating posterior probabilities in allocation rules.
#'
#' @seealso [study] for options for study types. [phase()] for defining study
#'   phases. [allocation_rule] for available rules for treatment allocation.
#' @export
#'
#' @examples
#' # A single-phase study uses the same allocation rule throughout
#' simulate_study(
#'   single_phase_study(n = 240, rule = rule_1(epsilon = 0.1)),
#'   a = 1,
#'   b = 1,
#'   theta = 0.2 + 1:4 / 10
#' )
#'
#' # A multi-phase study can e.g. have a burn-in period with another rule
#' simulate_study(
#'   multi_phase_study(phases = list(
#'     phase(n = 40, rule = randomization_list()),
#'     phase(n = 200, rule = rule_1(epsilon = 0.1))
#'   )),
#'   a = 1,
#'   b = 1,
#'   theta = 0.2 + 1:4 / 10
#' )
simulate_study <- function(study, model, scenario) {
  UseMethod("simulate_study")
}

#' @export
simulate_study.barts_study <- function(study, model, scenario) {
  # Track results from each phase
  results <- list()

  # Simulate each phase
  for (phase in study$phases) {
    phase_result <- simulate_phase(phase, model, scenario)

    # Carry over posterior parameters to next phase
    model <- final_model(phase_result)

    results <- c(results, list(phase_result))
  }

  names(results) <- names(study$phases)
  new_study_result(results)
}

simulate_phase <- function(phase, model, scenario) {
  steps <- phase_steps(phase)
  trace <- vector("list", length(steps))

  rule <- phase_rule(phase, scenario)
  prior <- list(rule = rule, model = model)

  for (step in steps) {
    rule <- evaluate(rule, model)
    A <- allocate(rule, 1) # TODO: Adjustable cohort sizes
    data <- observe(scenario, A)
    model <- update(model, data)

    # Keep track of results
    trace[[step]] <- list(rule = rule, data = data, model = model)
  }

  new_phase_result(trace, prior = prior)
}

phase_steps <- function(phase) {
  seq_len(phase$n)
}

phase_rule <- function(phase, scenario) {

}


observe <- function(scenario, ...) {
  UseMethod("observe")
}

observe.barts_scenario_bernoulli <- function(scenario, allocations, ...) {
  thetas <- scenario$params$theta
  Map(function(n, theta) {
    new_data_frame(
      list(
        x = as.integer(rbern(n, theta)),
        n = rep_len(1L, n)
      ),
      nrow = n
    )
  }, tabulate(allocations, length(thetas)), thetas)
}

scenario_bernoulli <- function(theta) {
  new_scenario(params = list(theta = theta), class = "barts_scenario_bernoulli")
}

new_scenario <- function(params, class = character()) {
  structure(list(params = params), class = c(class, "barts_scenario"))
}

allocate <- function(rule, n) {
  rule_next_allocation(rule)
}

evaluate <- function(rule, model) {
  p <- predict_best_arm(model)
}
