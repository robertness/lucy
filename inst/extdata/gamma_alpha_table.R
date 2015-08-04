gamma <- seq(from = 0, to = 3, by = .05)
get_b <- function(x){
  boot <- NULL
  for(i in 1:30){
    g <- ba.game(10000, power = x)
    d <- igraph::degree(g, mode = "in") 
    fit <- power.law.fit(d + 1, implementation="R.mle")
    boot <- c(boot, coef(fit))
  }
  mean(boot)
}
alpha <- sapply(gamma, get_b)
logistic <- function(x) 1 / (1 + exp(-x))
max_alpha <- get_b(10)
min_alpha <- get_b(0)
alpha2 <- (alpha - min_alpha) / (max_alpha - min_alpha)
fit <- nls(alpha2 ~ logistic(a + b * gamma), start = list(a = 1, b = 0))
alpha_pred <- logistic(coef(fit)[1] + coef(fit)[2] * gamma) * (max_alpha - min_alpha) + min_alpha
plot(gamma, alpha, main = "Graph generation and power law",
     xlab="gamma in generation algo", ylab ="mean estimate of power law param alpha")
lines(gamma, alpha_pred)
gamma_alpha_table <- data.frame(gamma, alpha, alpha_pred)
devtools::use_data(gamma_alpha_table, internal=TRUE, overwrite = TRUE)
