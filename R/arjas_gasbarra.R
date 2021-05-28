
# Definition --------------------------------------------------------------

#' Allocate according to Arjas & Gasbarra's rule
#'
#' `arjas_gasbarra()` allows defining the general form introduced in Arjas &
#' Gasbarra (2021). `rule_1()` and `rule_2()` provide convenient shortcuts to
#' match the rules presented in the paper.
#'
#' @export
#' @family allocation rules
arjas_gasbarra <- function(epsilon, epsilon_1, epsilon_2, theta_low, delta, randomization_list = NULL) {
  theta_low <- vctrs::vec_cast(theta_low, double(), x_arg = "theta_low")
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

#' @rdname arjas_gasbarra
#' @export
rule_1 <- function(epsilon, delta = 0, randomization_list = NULL) {
  arjas_gasbarra(
    epsilon,
    epsilon_1 = 0,
    epsilon_2 = 0,
    theta_low = NA,
    delta = delta,
    randomization_list = randomization_list
  )
}

#' @rdname arjas_gasbarra
#' @export
rule_2 <- function(epsilon, epsilon_1, epsilon_2, theta_low = NA, delta = 0, randomization_list = NULL) {
  arjas_gasbarra(
    epsilon,
    epsilon_1 = epsilon_1,
    epsilon_2 = epsilon_2,
    theta_low = theta_low,
    delta = delta,
    randomization_list = randomization_list
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
  arm_state <- ag_state(rule, samples)

  # If requested, use protective delta for reference arm
  if (rule$parameters$delta != 0) {
    samples[, 1L] <- samples[, 1L] + rule$parameters$delta
    arm_state_with_delta <- ag_state(rule, samples)
    arm_state[1L, ] <- arm_state_with_delta[1L, ]
  }

  # Keep track of potentially newly dropped arms
  rule$state$dropped <- which(arm_state[["dropped"]])

  # Extract relevant statistics for further use
  p <- arm_state[["p_max"]]
  I <- arm_state[["I"]]

  # Determine next treatment allocation
  if (!any(I)) {
    # Avoid infinite loop if all arms are inactive. This could occur if
    # dropping rules are configured badly, e.g. theta_low is too high.
    A <- NA_integer_
  } else {
    # Find next active arm
    r <- rule$parameters$r
    n <- rule$state$n
    while (!I[A <- r[n]]) {
      n <- n + 1L
    }
    rule$state$n <- n + 1L
  }

  rule_add_result(rule, p = p, I = I, A = A)
}

ag_state <- function(rule, samples) {
  n_arms <- ncol(samples)

  # Indicator for arm being previously dropped
  dropped <- seq_len(n_arms) %in% rule$state$dropped

  # Probability of being best arm -- 0 for dropped arms
  p_max <- pr_max_col(samples[, !dropped, drop = FALSE])
  p_max <- replace(rep_len(0, n_arms), !dropped, p_max)

  # Probability of meeting minimum response theta_low
  if (!is.na(rule$parameters$theta_low) && rule$parameters$epsilon_1 > 0) {
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

  new_data_frame(
    list(
      p_max = p_max,
      p_mrt = p_mrt,
      dormant = dormant,
      dropped = dropped,
      I = I
    ),
    nrow = n_arms
  )
}


# Plotting ----------------------------------------------------------------

#' @export
plot.bat_arjas_gasbarra <- function(x, ..., xlim = NULL) {
  if (!is.null(xlim)) {
    lines(xlim, rep(x$parameters$epsilon, 2), col = "grey", lty = 2)
    lines(xlim, rep(x$parameters$epsilon_2, 2), col = "grey", lty = 1)
  } else {
    abline(h = x$parameters$epsilon, col = "grey", lty = 2)
    abline(h = x$parameters$epsilon_2, col = "grey", lty = 1)
  }
}
