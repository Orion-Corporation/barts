#' Simulate a study
#' @param n_max The number of subjects to stop after.
#' @param theta A vector of true event probabilities in each arm.
#' @param a A vector of "a" parameters for the beta prior.
#' @param b A vector of "b" parameters for the beta prior.
#' @param rule Treatment allocation rule to use.
#' @param n_draws The number of draws to sample from the posterior for
#'   evaluating posterior probabilities.
#' @importFrom purrr %||%
#' @export
#' @examples
#' result <- simulate_study(
#'   n_max = 240,
#'   theta = 0.2 + 1:4 / 10,
#'   a = rep_len(1, 4),
#'   b = rep_len(1, 4),
#'   rule = rule_1(epsilon = 0.1)
#' )
simulate_study <- function(n_max, theta, a, b, rule, n_draws = 3000,
                           n_burn_in = 0, burn_in_rule = randomization_list()) {
  # Initialize list to hold tracked parameters at each step.
  trace <- vector("list", n_max + 1)

  rules <- list(burn_in_rule, rule)
  rules <- lapply(rules, rule_initialize, n_arms = length(theta), n_max = n_max)

  active_rule <- rules[[1L]]
  rule_updated <- FALSE

  for (i in seq_len(n_max + 1)) {
    # Swap allocation rule after burn-in period
    if (i > n_burn_in && !rule_updated) {
      active_rule <- rules[[2L]]
      rule_updated <- TRUE
    }

    # Draw samples from posterior
    posterior_samples <- mapply(stats::rbeta, n_draws, a, b)

    # Evaluate treatment allocation rule
    active_rule <- rule_evaluate(active_rule, posterior_samples)

    # Save tracked parameters
    trace[[i]] <- c(list(a = a, b = b), active_rule$result)

    if (i > n_max) {
      break # Stop before observing more data
    }

    A <- rule_next_allocation(active_rule)

    # Observe data
    d <- rbern(1, theta[A])

    # Update parameters
    a[A] <- a[A] + sum(d)
    b[A] <- b[A] + sum(1 - d)

    # Save observed data
    trace[[i]][["data"]] <- new_data_frame(list(A = A, d = d), nrow = 1L)
  }

  # Combine parameters into matrices across steps
  results <- lapply(purrr::transpose(trace), purrr::partial(do.call, "rbind"))
  results
}
