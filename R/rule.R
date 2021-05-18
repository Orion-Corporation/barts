
# Definition --------------------------------------------------------------

#' @export
new_rule <- function(parameters = list(), state = list(), class = character()) {
  structure(
    list(
      parameters = parameters,
      state = state,
      result = list()
    ),
    class = c(class, "bat_rule")
  )
}

#' @export
thompson <- function(kappa) {
  new_rule(parameters = list(kappa = kappa), class = "bat_thompson")
}

#' @export
randomization_list <- function(randomization_list = NULL) {
  new_rule(
    parameters = list(r = randomization_list),
    state = list(n = 1),
    class = "bat_randomization_list"
  )
}

#' @export
random_allocation <- function(p = NULL) {
  new_rule(parameters = list(p = p), class = "bat_random_allocation")
}


# Initialization ----------------------------------------------------------

#' @export
rule_initialize <- function(rule, ...) {
  UseMethod("rule_initialize")
}

#' @export
rule_initialize.bat_rule <- function(rule, ...) rule

#' @export
rule_initialize.bat_randomization_list <- function(rule, ..., n_max, n_arms) {
  if (is.null(rule$parameters$r)) {
    rule$parameters$r <- replicate(n_max, sample(n_arms))
  }
  rule
}

#' @export
#' @importFrom purrr %||%
rule_initialize.bat_random_allocation <- function(rule, ..., n_arms) {
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
#' @param rule A treatment allocation rule.
#' @param samples A matrix of samples from the posterior distribution of the
#'   parameter of interest, with a column for each treatment arm.
#' @export
rule_evaluate <- function(rule, samples) {
  UseMethod("rule_evaluate")
}

#' @export
rule_evaluate.bat_randomization_list <- function(rule, samples) {
  rule <- NextMethod()

  n <- rule$state$n
  A <- rule$parameters$r[n]
  rule$state$n <- n + 1L

  rule_add_result(rule, A = A)
}

#' @export
rule_evaluate.bat_random_allocation <- function(rule, samples) {
  rule <- NextMethod()

  p <- rule$parameters$p
  A <- sample(seq_along(p), 1, prob = p)

  rule_add_result(rule, A = A)
}

#' @export
rule_evaluate.bat_thompson <- function(rule, samples) {
  rule <- NextMethod()

  p <- rule$result$p
  k <- rule$parameters$kappa
  A <- sample(seq_along(p), 1, prob = p^k)

  rule_add_result(rule, A = A)
}

#' @export
rule_evaluate.bat_rule <- function(rule, samples) {
  # Find default statistics that should always be included
  rule_add_result(rule,
    p = pr_max_col(samples),
    I = rep_len(TRUE, ncol(samples))
  )
}


# Helpers -----------------------------------------------------------------

rule_add_result <- function(rule, ...) {
  rule$result <- purrr::list_modify(rule$result, ...)
  rule
}

rule_next_allocation <- function(rule) {
  if (is.null(rule$result)) {
    stop(call. = FALSE, "Rule has not been evaluated yet.")
  } else {
    rule$result$A
  }
}
