library(tidyverse)
library(brms)
library(readr)
library(MASS)

## Define "true" values
true_or <- 0.9
true_beta <- log(true_or)
true_intercept <- -2
true_site_int_sd <- 0.03
true_clinician_int_sd <- 0.03
true_site_beta_sd <- 0.03
true_clinician_beta_sd <- 0.03
true_ppx <- 0
true_site_ppx_sd <- 0.1
true_clinician_ppx_sd <- 0.1
true_clinician_rho <- -0.2
true_site_rho <- -0.2

clinician_r <- matrix(c(1, true_clinician_rho, true_clinician_rho, 1), 2, 2)

site_r <- matrix(c(1, true_site_rho, true_site_rho, 1), 2, 2)

clinician_sd <- matrix(c(true_clinician_int_sd, 0, 0, true_clinician_beta_sd), 2, 2)

site_sd <- matrix(c(true_site_int_sd, 0, 0, true_site_beta_sd), 2, 2)

clinician_sigma <- clinician_sd %*% clinician_r %*% clinician_sd
site_sigma <- site_sd %*% site_r %*% site_sd

## Function to allow character seeds
## From: https://stackoverflow.com/questions/10910698/questions-about-set-seed-in-r

set.seed.alpha <- function(x) {
  require("digest")
  hexval <- paste0("0x",digest(x,"crc32"))
  intval <- type.convert(hexval, as.is = TRUE) %% .Machine$integer.max
  set.seed(intval)
}

## Simulated Data
## Define sample size parameters
site_prop <- c(0.032, 0.031, 0.065, 0.114, 0.014, 0.031, 0.065, 0.046, 0.038, 0.014,
               0.021, 0.025, 0.029, 0.033, 0.017, 0.014, 0.042, 0.043, 0.033, 0.033,
               0.022, 0.035, 0.051, 0.044, 0.041, 0.010, 0.009, 0.008, 0.031, 0.009)

n_sites <- length(site_prop)

total_n <- 100000

site_n <- total_n * site_prop %>% sort(decreasing = TRUE)

site_clinician_n <- pmax(rbinom(n_sites, size = 100, prob = 0.1), 2) %>% sort(decreasing = TRUE)
clinician_markers <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

set.seed(11)

make_df <- function(n_sites, site_n, site_clinician_n, clinician_markers) {
  
  df <- 
    tibble(site = rep(1:n_sites, times = site_n)) %>%
    mutate(clinician_n = site_clinician_n[site]) %>%
    rowwise() %>%
    mutate(clinician = paste0(sample(clinician_markers[1:clinician_n], size = 1, replace = TRUE), site)) %>%
    ungroup() %>%
    group_by(site) %>%
    mutate(site_ppx  = rnorm(1, mean = 0, sd = true_site_ppx_sd),
           site_int  = with(set.seed(site), mvrnorm(1, mu = rep(0, 2), Sigma = site_sigma)[1]),
           site_beta = with(set.seed(site), mvrnorm(1, mu = rep(0, 2), Sigma = site_sigma)[2])) %>%
    group_by(clinician) %>%
    mutate(clinician_ppx  = rnorm(1, mean = 0, sd = true_clinician_ppx_sd),
           clinician_int  = with(set.seed.alpha(clinician), mvrnorm(1, mu = rep(0, 2), Sigma = clinician_sigma)[1]),
           clinician_beta = with(set.seed.alpha(clinician), mvrnorm(1, mu = rep(0, 2), Sigma = clinician_sigma)[2])) %>%
    ungroup() %>%
    mutate(log_odds_ppx  = true_ppx + site_ppx + clinician_ppx,
           prob_ppx      = inv_logit_scaled(log_odds_ppx),
           ppx_yn        = rbinom(n(), 1, p = prob_ppx),
           log_odds      = true_intercept + site_int + clinician_int + ((true_beta + site_beta + clinician_beta) * ppx_yn),
           prob_ponv     = inv_logit_scaled(log_odds),
           ponv_yn       = rbinom(n(), size = 1, prob = prob_ponv))
  
  return(df)
}

df <- make_df(n_sites, site_n, site_clinician_n, clinician_markers)

## Fit Initial Model
fit <- 
  brm(data = df,
      family = bernoulli(link = "logit"),
      bf(ponv_yn ~ 1 + ppx_yn + (1 + ppx_yn | site / clinician),
         decomp = "QR"),
      prior = c(prior(normal(-1.5, 1), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(cauchy(0, 1), class = sd),
                prior(lkj_corr_cholesky(2), class = L)),
      backend = "cmdstanr",
      seed = 11,
      chains = 2,
      cores = 2,
      threads = threading(2))

## Function to iteratively simulate data and re-fit the model
sim_and_fit <- function(seed, n) {
  
  set.seed(seed)

  total_n <- n
  
  site_n <- total_n * site_prop %>% sort(decreasing = TRUE)
  
  site_clinician_n <- pmax(rbinom(n_sites, size = 100, prob = 0.1), 2) %>% sort(decreasing = TRUE)
  
  # Simulate new data
  df <- make_df(n_sites, site_n, site_clinician_n, clinician_markers)
  
  print(dim(df))
  
  # Update model fit
  update(fit,
         newdata = df,
         seed = seed,
         backend = "cmdstanr",
         chains = 2,
         cores = 2,
         threads = threading(2)) %>%
    fixef() %>%
    data.frame %>%
    rownames_to_column("parameter") %>%
    filter(parameter == "ppx_yn")
}

# Generate simulations and models
# Extract param
s1k <- tibble(seed = 1:50) %>%
  mutate(b1 = map(seed, sim_and_fit, n = 1000)) %>%
  unnest(b1) %>%
  mutate(width = Q97.5 - Q2.5)

s10k <- tibble(seed = 1:50) %>%
  mutate(b1 = map(seed, sim_and_fit, n = 10000)) %>%
  unnest(b1) %>%
  mutate(width = Q97.5 - Q2.5)

s100k <- tibble(seed = 1:50) %>%
  mutate(b1 = map(seed, sim_and_fit, n = 100000)) %>%
  unnest(b1) %>%
  mutate(width = Q97.5 - Q2.5)

s10k %>%
  ggplot(aes(x = reorder(seed, Q2.5), y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_hline(yintercept = c(0, true_beta), color = "white") +
  geom_pointrange(fatten = 1/2) +
  scale_x_discrete("reordered by the lower level of the 95% intervals", breaks = NULL) +
  ylab(expression(beta[1])) 

write_rds(s1k, "data/power_1k.rds")
write_rds(s10k, "data/power_10k.rds")
write_rds(s100k, "data/power_10k.rds")
