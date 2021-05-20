new_phase_result <- function(x, rule) {
  stopifnot(is.list(x), is_rule(rule))
  structure(x, rule = rule, class = "bat_phase_result")
}

#' @export
plot.bat_phase_result <- function(x, ...) {
  op <- par(mfcol = c(2, 1))
  on.exit(par(op))

  with(x, {

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
