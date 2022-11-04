model_beta_binomial <- function(a, b) {
  new_model(params = list(a = a, b = b), class = "barts_model_beta_binomial")
}

new_model <- function(params, class = character()) {
  structure(list(params = params), class = c(class, "barts_model"))
}

update.barts_model_beta_binomial <- function(object, data, ...) {
  n <- sapply(data, function(d) sum(d$n))
  a <- sapply(data, function(d) sum(d$x))
  b <- n - a
  model_beta_binomial(object$params$a + a, object$params$b + b)
}

predict_best_arm <- function(model, arms = NULL) {
  UseMethod("predict_best_arm")
}

predict_best_arm.barts_model_beta_binomial <- function(model, arms = NULL) {
  a <- model$params$a
  b <- model$params$b
  if (!is.null(arms)) {
    a <- a[arms]
    b <- b[arms]
  }
  # TODO: Allow passing control parameters like number of draws here.
  samples <- mapply(stats::rbeta, 4000, a, b)
  pr_max_col(samples)
}
