#' Define a study phase
#'
#' @param n Number of subjects to accrue before the phase ends.
#' @param rule A [rule] for determining treatment allocation.
#'
#' @seealso [study] which are formed from phases.
#' @export
phase <- function(n, rule) {
  n <- vctrs::vec_cast(n, integer(), x_arg = "n")
  new_phase(n, rule)
}

new_phase <- function(n, rule) {
  stopifnot(rlang::is_scalar_integer(n), is_rule(rule))
  structure(list(n = n, rule = rule), class = "barts_phase")
}

is_phase <- function(x) inherits(x, "barts_phase")

all_are_phases <- function(x) {
  all(vapply(x, is_phase, logical(1)))
}


# Result ------------------------------------------------------------------

new_phase_result <- function(x, rule) {
  stopifnot(is.list(x), is_rule(rule))
  structure(x, rule = rule, class = "barts_phase_result")
}

#' @export
plot.barts_phase_result <- function(x, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(op)))

  with(x, {
    par(mfcol = c(2, 1))

    # Posterior means
    mu <- a / (a + b)
    n <- seq_len(nrow(mu)) - 1

    par(mar = c(3, 5, 2, 1))
    matplot(
      n, mu,
      type = "l", lty = 1,
      xlab = "",
      ylab = expression(
        hat(theta[i])
      )
    )

    # Grey out estimates for inactive arms, if any
    if (!all(I)) {
      matlines(n, replace(mu, I, NA), col = "grey", lwd = 2, lty = 1)
    }

    # Posterior probability of being being best
    par(mar = c(5, 5, 0, 1))
    matplot(
      n, p,
      type = "l", lty = 1,
      xlab = "# subjects",
      ylab = expression(
        "Pr(" * theta[i] * "=" * theta[v] * ")"
      )
    )

    # Add annotations from rule
    plot(attr(x, "rule"))

  })
}
