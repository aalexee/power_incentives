# the code reproduces the results from section 3.2 Discrete case

# packages ----------------------------------------------------------------

library(tidyverse)


# functions ---------------------------------------------------------------

# utility of money function
u <- function(x) {
  (1 - exp(-a*x^(1 - r)))/a
}

# expected values of lotteries
EV_A <- function(tau) 0.5*(2 + 1.6)*tau
EV_B <- function(tau) 0.5*(3.85 + 0.1)*tau

# expected utilities of lotteries
U_A <- function(tau) 0.5*(u(2*tau) + u(1.6*tau))
U_B <- function(tau) 0.5*(u(3.85*tau) + u(0.1*tau))

# outcome functions
mu_T <- function(tau) {
  U_A(tau)^(1/lambda)/(U_A(tau)^(1/lambda) + U_B(tau)^(1/lambda))
}

mu_C <- function(tau) {
  tau <- 1
  U_A(tau)^(1/lambda)/(U_A(tau)^(1/lambda) + U_B(tau)^(1/lambda))
}

# sample size
n <- function(tau) {
  
  p_t <- mu_T(tau)
  p_c <- mu_C(tau)
  
  (p_t*(1 - p_t) + p_c*(1 - p_c))*((z_alpha + z_beta) / (p_t - p_c))^2
}

# payoff functions
pi_T <- function(tau) {
  mu_T(tau)*EV_A(tau) + (1 - mu_T(tau))*EV_B(tau)
}

pi_C <- function(tau) {
  tau <- 1
  mu_C(tau)*EV_A(tau) + (1 - mu_C(tau))*EV_B(tau)
}

# budget
pi <- function(tau) {
  n(tau)*(2*w + pi_C(tau) + pi_T(tau))
}


# budget minimization -------------------------------------------------------------------

w <- 5

alpha <- 0.05
beta <- 0.2

z_alpha <- qnorm(1 - alpha/2)
z_beta <- qnorm(1 - beta)

a <- 0.029
r <- 0.269
lambda <- 0.134

bmp <- optimize(f = pi, interval = c(20, 100))

sim <- tibble(
  tau = seq(20, 100, 1)
) %>% 
  mutate(
    "Total Number of Subjects" = 2*n(tau)
    , "Payoff per Subject (\\$)" = (2*w + pi_C(tau) + pi_T(tau))/2
    , "Total Budget (\\$)" = pi(tau)
  )

(tau_star <- bmp$minimum)
(n_star <- 2*n(bmp$minimum))
(pi_star <- bmp$objective)

sim %>%
  pivot_longer(cols = -tau) %>% 
  mutate(across(where(is_character), as_factor)) %>% 
  {ggplot(data = ., aes(x = tau, y = value)) +
      geom_line() +
      facet_wrap(~ name, scales = "free_y") +
      geom_vline(
        xintercept = tau_star
        , color = "grey"
      ) +
    labs(y = NULL, x = "$\\tau$, log scale") +
      scale_y_continuous(limits = c(0, NA), n.breaks = 10 ) +
      scale_x_log10(
        breaks = seq(20, 100, 20)
      )
  } 

