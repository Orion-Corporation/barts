new_rule <- function(..., .class = character()) {
  structure(list(...), class = c(.class, "bat_rule"))
}

rule_1 <- function(epsilon, delta = 0, randomization_list = NULL) {
  new_rule(epsilon = epsilon, delta = delta, .class = "bat_rule_1")
}

rule_2 <- function(epsilon, epsilon_1, epsilon_2, delta = 0, randomization_list = NULL) {
  new_rule(
    epsilon = epsilon,
    epsilon_1 = epsilon_1,
    epsilon_2 = epsilon_2,
    delta = delta,
    .class = "bat_rule_2"
  )
}

thompson <- function(kappa) {
  new_rule(kappa = kappa, .class = "bat_thompson")
}
