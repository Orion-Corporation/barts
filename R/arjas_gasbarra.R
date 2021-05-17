
# Definition --------------------------------------------------------------

#' @export
rule_1 <- function(epsilon, delta = 0, randomization_list = NULL) {
  arjas_gasbarra(
    epsilon,
    epsilon_1 = 0,
    epsilon_2 = 0,
    delta = delta,
    randomization_list = randomization_list
  )
}

#' @export
rule_2 <- function(epsilon, epsilon_1, epsilon_2, delta = 0, randomization_list = NULL) {
  arjas_gasbarra(
    epsilon,
    epsilon_1 = epsilon_1,
    epsilon_2 = epsilon_2,
    delta = delta,
    randomization_list = randomization_list
  )
}

#' @export
arjas_gasbarra <- function(epsilon, epsilon_1, epsilon_2, delta, randomization_list = NULL) {
  new_rule(
    parameters = list(
      epsilon = epsilon,
      epsilon_1 = epsilon_1,
      epsilon_2 = epsilon_2,
      delta = delta,
      r = randomization_list
    ),
    state = list(n = 1, dropped = integer()),
    class = "bat_arjas_gasbarra"
  )
}


# Initialization ----------------------------------------------------------

#' @export
rule_initialize.bat_arjas_gasbarra <- function(rule, ..., n_max, n_arms) {
  if (is.null(rule$parameters$r)) {
    rule$parameters$r <- replicate(n_max, sample(n_arms))
  }
  rule
}


# Evaluation --------------------------------------------------------------

#' @export
rule_evaluate.bat_arjas_gasbarra <- function(rule, samples) {
  # For each arm, probability that it's best
  p <- pr_max_col(samples)

  # Update arm activity indicators
  I <- p > rule$parameters$epsilon

  # For reference arm, use protective delta
  if (rule$parameters$delta != 0) {
    samples[, 1L] <- samples[, 1L] + rule$parameters$delta
    I[1L] <- pr_max_col(samples)[1L] > rule$parameters$epsilon
  }

  # Ensure dropped arms are inactive
  I <- replace(I, rule$state$dropped, FALSE)

  # Find next active arm
  r <- rule$parameters$r
  n <- rule$state$n
  while (!I[A <- r[n]]) {
    n <- n + 1L
  }
  rule$state$n <- n + 1L

  rule$result <- list(p = p, I = I, A = A)
  rule
}
