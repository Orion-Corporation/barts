#' Simulate a study
#'
#' @param phases A list of study [phase]s to simulate in sequence.
#' @inheritParams simulate_phase
#'
#' @seealso [simulate_phase()] for simulating a single phase.
#' @export
#'
#' @examples
#' simulate_study(
#'   phases = list(
#'     burn_in  = phase(n = 40, rule = randomization_list()),
#'     adaptive = phase(n = 240, rule = rule_1(epsilon = 0.1))
#'   ),
#'   a = 1,
#'   b = 1,
#'   theta = 0.2 + 1:4 / 10
#' )
simulate_study <- function(phases, theta, a, b, n_draws = 3000) {
  # Track results from each phase
  results <- list()

  # Simulate each phase
  for (phase in phases) {
    phase_result <- simulate_phase(phase, theta, a, b, n_draws)

    # Carry over posterior parameters to next phase
    a <- as.vector(tail(phase_result$a, 1L))
    b <- as.vector(tail(phase_result$b, 1L))

    results <- c(results, list(phase_result))
  }

  names(results) <- names(phases)
  results
}

#' Simulate a study phase
#'
#' @param phase A [phase] defining the number of subjects to accrue and the
#'   treatment allocation rule to use.
#' @param theta A vector of true event probabilities in each arm.
#' @param a,b Vectors of parameters to the beta prior with elements for each
#'   arm. Recycled to match the length of `theta` if possible.
#' @param n_draws The number of draws to sample from the posterior for
#'   evaluating posterior probabilities in allocation rules.
#'
#' @seealso [simulate_study()] for simulating a multi-phase study.
#' @importFrom zeallot %<-%
#' @export
#'
#' @examples
#' simulate_phase(
#'   phase(n = 240, rule = random_allocation(p = 1:2)),
#'   a = 1,
#'   b = 1,
#'   theta = c(0.3, 0.5)
#' )
simulate_phase <- function(phase, theta, a, b, n_draws = 3000) {
  # Validate input parameters
  c(theta, a, b) %<-% vctrs::vec_recycle_common(theta = theta, a = a, b = b)

  theta <- vctrs::vec_cast(theta, double(), x_arg = "theta")
  a <- vctrs::vec_cast(a, double(), x_arg = "a")
  b <- vctrs::vec_cast(b, double(), x_arg = "b")

  n_draws <- vctrs::vec_cast(n_draws, integer(), x_arg = "n_draws")

  # Initialize list to hold tracked parameters at each step -- one step before
  # each subject and one after the last to evaluate the posterior in the end.
  n_steps <- phase$n + 1L
  trace <- vector("list", n_steps)

  # Initialize allocation rule
  rule <- rule_initialize(phase$rule, n_max = n_steps, n_arms = length(theta))

  for (i in seq_len(n_steps)) {
    # Draw samples from posterior
    posterior_samples <- mapply(stats::rbeta, n_draws, a, b)

    # Evaluate treatment allocation rule
    rule <- rule_evaluate(rule, posterior_samples)

    # Save tracked parameters
    trace[[i]] <- c(list(a = a, b = b), rule$result)

    if (i > phase$n) {
      break # Stop before observing more data at last step
    }

    A <- rule_next_allocation(rule)

    # Observe data
    d <- rbern(1L, theta[A])

    # Update parameters
    a[A] <- a[A] + sum(d)
    b[A] <- b[A] + sum(1 - d)

    # Save observed data
    trace[[i]][["data"]] <- new_data_frame(list(A = A, d = d), nrow = 1L)
  }

  # Combine parameters into matrices across steps
  results <- lapply(purrr::transpose(trace), purrr::partial(do.call, "rbind"))

  new_phase_result(results, rule = rule)
}
