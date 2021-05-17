
simulate_study <- function(n_max, theta, a, b, r, epsilon, delta, n_draws) {
  # Initialize list to hold tracked parameters at each step.
  trace <- vector("list", n_max + 1)

  n_arms <- length(theta)
  n <- 0 # Current index in randomization list, after skipping dormant arms.

  for (i in seq_len(n_max + 1)) {
    # Draw samples from posterior
    samples <- mapply(stats::rbeta, n_draws, a, b)

    # For each arm, probability that it's best
    p <- pr_max_col(samples)

    # Update arm activity indicators
    I <- p > epsilon

    # For reference arm, use protective delta
    if (delta != 0) {
      samples[, 1] <- samples[, 1] + delta
      I[1] <- pr_max_col(samples)[1] > epsilon
    }

    # Save tracked parameters
    trace[[i]] <- list(a = a, b = b, p = p, I = I)

    if (i > n_max) {
      break # Don't observe any more data
    }

    # Find next active arm
    n <- n + 1
    while (!I[A <- r[n]]) {
      n <- n + 1
    }

    # Observe data
    d <- rbern(1, theta[A])

    # Update parameters
    a[A] <- a[A] + sum(d)
    b[A] <- b[A] + sum(1 - d)

    # Save observed data
    trace[[i]][["data"]] <- data.frame(A = A, d = d)
  }

  # Combine parameters into matrices across steps
  results <- lapply(purrr::transpose(trace), purrr::partial(do.call, "rbind"))
  results
}
