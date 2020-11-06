#' Create a data.frame of random receivables
#'
#' @param seed Random seed
#'
#' @importFrom  stats runif rpois rbinom
#' @importFrom boot inv.logit
#'
#' @export
create_receivables <- function(seed = 80402) {
  set.seed(seed)
  population_size <- 42000
  population_max <- 10000
  lambda <- c("t1" = 14, "t2" = 20)
  n_periods <- 2
  betas <- c("intercept" = .5, amount = 1, period = prod(lambda ^ c(1, -1)), -2)

  amount <- population_max * stats::runif(n = population_size * n_periods)
  period <- rep(seq(n_periods) - 1, each = population_size)
  days_past_due <- stats::rpois(
    n = population_size * n_periods,
    lambda = rep(lambda, each = population_size)
  )
  prob_paid <- boot::inv.logit(
    cbind(1, scale(cbind(amount, period, amount * period))) %*% betas
  )
  paid <- stats::rbinom(population_size * n_periods, size = 1, prob = prob_paid)

  data.frame(
    amount = amount,
    period = period,
    days_past_due = days_past_due,
    paid = paid
  )
}
