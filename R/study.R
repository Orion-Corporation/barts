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


# Result ------------------------------------------------------------------

new_study_result <- function(x) {
  stopifnot(is.list(x)) # List of phase results
  structure(x, class = "bat_study_result")
}

#' @export
plot.bat_study_result <- function(x, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(op)))

  # Find common plotting parameters across phases to start

  # Total number of subjects: this will be on the x-axis.
  ns <- purrr::map_int(x, list("p", nrow)) - 1L
  xlim <- c(0, sum(ns))

  # Subject count at the start of each phase
  n_start <- head(c(0L, cumsum(ns)), -1L)
  for (i in seq_along(x)) {
    x[[i]][["n_start"]] <- n_start[i]
  }

  # Expected value for theta at each phase
  for (i in seq_along(x)) {
    x[[i]][["mu"]] <- x[[i]][["a"]] / (x[[i]][["a"]] + x[[i]][["b"]])
  }

  # Find required y-axis ranges
  ylim_1 <- purrr::reduce(purrr::map(x, list("mu", range)), range)
  ylim_2 <- purrr::reduce(purrr::map(x, list("p", range)), range)


  # Start plotting

  # Plot with two panels, stacked
  par(mfcol = c(2, 1))

  # Top plot with means
  par(mar = c(3, 5, 2, 1))

  plot(0, type = "n",
    xlim = xlim,
    ylim = ylim_1,
    xlab = "",
    ylab = expression(
      hat(theta[i])
    )
  )

  # Mark out phase starts
  abline(v = n_start, col = "grey", lty = 2)

  for (result in x) {
    n <- result$n_start + seq_len(nrow(result$mu)) - 1L
    mu <- result$mu
    I <- result$I

    # Posterior means
    matlines(n, mu, lty = 1)

    # Grey out estimates for inactive arms, if any
    if (!all(I)) {
      matlines(n, replace(mu, I, NA), col = "grey", lwd = 2, lty = 1)
    }
  }

  # Bottom plot with probabilities
  par(mar = c(5, 5, 0, 1))

  plot(0, type = "n",
    xlim = xlim,
    ylim = ylim_2,
    xlab = "# subjects",
    ylab = expression(
     "Pr(" * theta[i] * "=" * theta[v] * ")"
    )
  )

  # Mark out phase starts
  abline(v = n_start, col = "grey", lty = 2)

  for (result in x) {
    n <- result$n_start + seq_len(nrow(result$mu)) - 1L

    # Posterior probability of being being best
    matlines(n, result$p, lty = 1)

    # Add annotations from rule
    plot(attr(result, "rule"), xlim = range(n))
  }

  box()
}
