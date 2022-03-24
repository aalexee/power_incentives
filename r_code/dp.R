# the code reproduces the results from section 3.1 Continuous case

# packages ----------------------------------------------------------------

library(tidyverse)


# functions ---------------------------------------------------------------

# outcome functions
mu_T <- function(tau) {
  z_T <- z_C + tau
  1/gamma*(log(s + z_T) - log(k))
}

mu_C <- function(tau) {
  1/gamma*(log(s + z_C) - log(k))
}

# sample size
n <- function(tau) {
  2*(sigma/(mu_T(tau) - mu_C(tau)))^2*(z_alpha + z_beta)^2
}

# payoff functions
pi_T <- function(tau) {
  z_T <- z_C + tau
  z_T*mu_T(tau)/100
}

pi_C <- function(tau) {
  z_C*mu_C(tau)/100
}

# budget
pi <- function(tau) {
  n(tau)*(2*w + pi_C(tau) + pi_T(tau))
}


# budget minimization --------------------------------------------------------------

alpha <- 0.05
beta <- 0.2

z_alpha <- qnorm(1 - alpha/2)
z_beta <- qnorm(1 - beta)

gamma <- 0.015641071
k <- 1.70926702e-16
s <- 3.72225938e-6
sigma <- 653.578104

w <- 1
z_C <- 0.0

bmp <- optimize(f = pi, interval = c(0.001, 0.5))

sim <- tibble(
  tau = seq(0.001, 0.5, 0.001)
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
    labs(y = NULL, x = "$\\tau$ (\\$), log scale") +
      scale_y_continuous(limits = c(0, NA), n.breaks = 10 ) +
      scale_x_log10(n.breaks = 4)
  } 

