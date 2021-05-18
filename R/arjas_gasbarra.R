
# Definition --------------------------------------------------------------

#' @export
rule_1 <- function(epsilon, delta = 0, randomization_list = NULL) {
  arjas_gasbarra(
    epsilon,
    epsilon_1 = 0,
    epsilon_2 = 0,
    theta_low = NULL,
    delta = delta,
    randomization_list = randomization_list
  )
}

#' @export
rule_2 <- function(epsilon, epsilon_1, epsilon_2, theta_low, delta = 0, randomization_list = NULL) {
  arjas_gasbarra(
    epsilon,
    epsilon_1 = epsilon_1,
    epsilon_2 = epsilon_2,
    theta_low = theta_low,
    delta = delta,
    randomization_list = randomization_list
  )
}

#' @export
arjas_gasbarra <- function(epsilon, epsilon_1, epsilon_2, theta_low, delta, randomization_list = NULL) {
  new_rule(
    parameters = list(
      epsilon = epsilon,
      epsilon_1 = epsilon_1,
      epsilon_2 = epsilon_2,
      theta_low = theta_low,
      delta = delta,
      r = randomization_list
    ),
    state = list(
      n = 1L,              # Current position in randomization list
      dropped = integer()  # Indices of permanently dropped arms so far
    ),
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
  # Determine arm state and statistics based on Arjas & Gasbarra (2021)
  state <- ag_arm_state(rule, samples)

  # If requested, use protective delta for reference arm
  if (rule$parameters$delta != 0) {
    samples[, 1L] <- samples[, 1L] + rule$parameters$delta
    state_with_delta <- ag_arm_state(rule, samples)
    state[1L, ] <- state_with_delta[1L, ]
  }

  # Keep track of potentially newly dropped arms
  rule$state$dropped <- which(state[, "dropped"])

  # Extract relevant statistics for further use
  p <- state[, "p_max"]
  I <- state[, "I"]

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

ag_arm_state <- function(rule, samples) {
  n_arms <- ncol(samples)

  # Indicator for arm being previously dropped
  dropped <- seq_len(n_arms) %in% rule$state$dropped

  # Probability of being best arm -- 0 for dropped arms
  p_max <- pr_max_col(samples[, !dropped, drop = FALSE])
  p_max <- replace(rep_len(0, n_arms), !dropped, p_max)

  # Probability of meeting minimum response theta_low
  if (!is.null(rule$parameters$theta_low) && rule$parameters$epsilon_1 > 0) {
    p_mrt <- colMeans(samples > rule$parameters$theta_low)
  } else {
    p_mrt <- rep_len(1, n_arms)
  }

  # Update dormant and dropped states
  dormant <- p_max < rule$parameters$epsilon
  dropped <- dropped |
    p_mrt < rule$parameters$epsilon_1 |
    p_max < rule$parameters$epsilon_2

  # Combined activity indicator
  I <- !dormant & !dropped

  data.frame(p_max, p_mrt, dormant, dropped, I)
}
