
# Definition --------------------------------------------------------------

#' Create a new treatment allocation rule
#'
#' `new_rule()` is used as a base to create a new treatment allocation rule
#' subclass. Define a [rule_evaluate()] method to implement functionality.
#' The package's allocation rules are listed in the "See Also" section.
#'
#' @param parameters List of user-supplied parameters for the rule.
#' @param state List of values the rule keeps track of over evaluations.
#' @param class Name of the rule subclass to create.
#'
#' @export
#' @name allocation_rule
#' @aliases rule
#' @family allocation rules
new_rule <- function(parameters = list(), state = list(), class = character()) {
  structure(
    list(
      parameters = parameters,
      state = state,
      result = list()
    ),
    class = c(class, "barts_rule")
  )
}

#' Allocate by a pre-defined randomization list
#' @export
#' @family allocation rules
randomization_list <- function(randomization_list = NULL) {
  new_rule(
    parameters = list(r = randomization_list),
    state = list(n = 1L),
    class = "barts_randomization_list"
  )
}

#' Allocate randomly with a given probability
#' @export
#' @family allocation rules
random_allocation <- function(p = NULL) {
  new_rule(parameters = list(p = p), class = "barts_random_allocation")
}

#' Allocate according to Thompson's rule
#'
#' The randomization probability for each arm is determined by raising the
#' posterior probability of that arm being the best studied arm to the power
#' `kappa` given as a parameter.
#'
#' @export
#' @family allocation rules
thompson <- function(kappa) {
  new_rule(parameters = list(kappa = kappa), class = "barts_thompson")
}


# Initialization ----------------------------------------------------------

rule_initialize <- function(rule, ...) {
  UseMethod("rule_initialize")
}

#' @export
rule_initialize.barts_rule <- function(rule, ...) rule

#' @export
rule_initialize.barts_randomization_list <- function(rule, ..., n_max, n_arms) {
  if (is.null(rule$parameters$r)) {
    rule$parameters$r <- replicate(n_max, sample(n_arms))
  }
  rule
}

#' @export
#' @importFrom purrr %||%
rule_initialize.barts_random_allocation <- function(rule, ..., n_arms) {
  # Ensure randomization probabilities sum to one
  p <- rule$parameters$p %||% rep_len(1, n_arms)
  rule$parameters$p <- prop.table(p)

  rule
}


# Evaluation --------------------------------------------------------------

#' Evaluate a treatment allocation rule
#'
#' An implementation must populate the rule with the `$result` element,
#' including at least the next allocation in `$result$A`.
#'
#' @param rule A treatment allocation [rule].
#' @param samples A matrix of samples from the posterior distribution of the
#'   parameter of interest, with a column for each treatment arm.
#'
#' @export
rule_evaluate <- function(rule, samples) {
  UseMethod("rule_evaluate")
}

#' @export
rule_evaluate.barts_rule <- function(rule, samples) {
  # Find default statistics that should always be included
  rule_add_result(rule,
    p = pr_max_col(samples),
    I = rep_len(TRUE, ncol(samples))
  )
}

#' @export
rule_evaluate.barts_randomization_list <- function(rule, samples) {
  rule <- NextMethod()

  n <- rule$state$n
  A <- rule$parameters$r[n]
  rule$state$n <- n + 1L

  rule_add_result(rule, A = A)
}

#' @export
rule_evaluate.barts_random_allocation <- function(rule, samples) {
  rule <- NextMethod()

  p <- rule$parameters$p
  A <- sample(seq_along(p), 1, prob = p)

  rule_add_result(rule, A = A)
}

#' @export
rule_evaluate.barts_thompson <- function(rule, samples) {
  rule <- NextMethod()

  p <- rule$result$p
  k <- rule$parameters$kappa
  A <- sample(seq_along(p), 1, prob = p^k)

  rule_add_result(rule, A = A)
}


# Plotting ----------------------------------------------------------------

#' @export
plot.barts_rule <- function(x, ...) {
  # A rule can define a plot method to make annotations on the plot showing the
  # posterior probability of being the maximum. There is no default behavior.
}


# Helpers -----------------------------------------------------------------

rule_add_result <- function(rule, ...) {
  rule$result <- purrr::list_modify(rule$result, ...)
  rule
}

rule_next_allocation <- function(rule) {
  if (is.null(rule$result$A)) {
    stop(call. = FALSE, "Rule has not been evaluated yet.")
  } else {
    rule$result$A
  }
}

is_rule <- function(x) inherits(x, "barts_rule")
