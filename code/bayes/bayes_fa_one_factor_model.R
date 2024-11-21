# Carica il file _common.R per impostazioni di pacchetti e opzioni
here::here("code", "_common.R") |> source()

# Carica pacchetti aggiuntivi
pacman::p_load(lavaan, tidyverse, here, corrplot, tidyr, kableExtra, 
               lavaanPlot, lavaanExtra, cmdstanr)

devtools::install_github('m-clark/noiris')

varnames <- c(
  "Classics", "French", "English", "Math", "Pitch", "Music"
)

ny <- length(varnames)

spearman_cor_mat <- matrix(
  c(
    1.00,  .83,  .78,  .70,  .66,  .63,
    .83, 1.00,  .67,  .67,  .65,  .57,
    .78,  .67, 1.00,  .64,  .54,  .51,
    .70,  .67,  .64, 1.00,  .45,  .51,
    .66,  .65,  .54,  .45, 1.00,  .40,
    .63,  .57,  .51,  .51,  .40, 1.00
  ),
  ny, ny,
  byrow = TRUE,
  dimnames = list(varnames, varnames)
)
spearman_cor_mat

# The sample size
n <- 33

spearman_mod <- "
  g =~ Classics + French + English + Math + Pitch + Music
"

fit <- lavaan::cfa(
  spearman_mod,
  sample.cov = spearman_cor_mat,
  sample.nobs = n,
  std.lv = TRUE
)

parameterEstimates(fit, standardized = TRUE) |>
  print()

# specify population model
population_model <- ' 
  f =~ 0.942*Classics + 
       0.857*French +
       0.795*English + 
       0.732*Math +
       0.678*Pitch +
       0.643*Music

  f ~~ 1*f
'

# generate data
set.seed(1234)
my_data <- simulateData(population_model, sample.nobs=500L)

d <- data.frame(
  Classics = as.numeric(scale(my_data$Classics)),
  French = as.numeric(scale(my_data$French)),
  English = as.numeric(scale(my_data$English)),
  Math = as.numeric(scale(my_data$Math)),
  Pitch = as.numeric(scale(my_data$Pitch)),
  Music = as.numeric(scale(my_data$Music))
)

cor(my_data)


fit_2 <- lavaan::cfa(
  spearman_mod,
  data = d,
  std.lv = TRUE
)

parameterEstimates(fit_2, standardized = TRUE) |>
  print()

# Bayesian Factor Model

stan_file =  file.path(
  here::here("code", "bayes", "one_factor_model.stan")
)
  
mod <- cmdstan_model(stan_file)

mod$print()

data_list <- list(
  n = 500, 
  y = as.matrix(d)
)

fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4
)

fit$summary(variables = c("lambda1", "lambda_other"), "mean", "sd")




# ------------------
# https://m-clark.github.io/models-by-example/bayesian-cfa.html

data('big_five', package = 'noiris')
# data('bfi', package = 'psych')

big_five_no_miss <- big_five |> 
  dplyr::select(matches('(^E|^O|^N)[1-3]')) %>% 
  drop_na() |> 
  slice_sample(n = 280)

stan_file =  file.path(
  here::here("code", "bayes", "one_factor_model.stan")
)

mod <- cmdstan_model(stan_file)

mod$print()

stan_data = 
  list(
    N = nrow(big_five_no_miss),
    P = ncol(big_five_no_miss),
    K = 3,
    X = big_five_no_miss
  )

fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4
)

fit$summary(variables = c("lambda", "phi"), "mean", "sd")



mod = "
  E =~ E1 + E2 + E3 
  N =~ N1 + N2 + N3 
  O =~ O1 + O2 + O3 
"

fit_lav = cfa(mod, data = big_five_no_miss)
summary(fit_lav)

lav_par  = parameterEstimates(fit_lav, standardized = TRUE)
lav_par

# Estrarre le stime posteriori
# Assumi che fit_cfa sia l'oggetto modello ottenuto da cmdstanr e stan_data sia l'input passato al modello Stan.

# Stime dei loadings
lambda_posterior <- fit$draws("lambda") # Estrai i draws per lambda
lambda_means <- apply(lambda_posterior, 2, mean) # Calcola la media di ogni loading
lambda <- c(1, lambda_means[1:2], 1, lambda_means[3:4], 1, lambda_means[5:6])

# Deviazioni standard dei fattori latenti e delle variabili osservate
sd_F_posterior <- fit$draws("sd_lv") # Estrai i draws per sd_lv
sd_F_means <- rep(apply(sd_F_posterior, 2, mean), each = 3) # Media e ripeti per il numero di fattori
x_sd <- apply(stan_data$X, 2, sd) # Calcola le deviazioni standard osservate

# Standardizzazione
lambda_std_F <- sd_F_means * lambda
lambda_std_all <- (sd_F_means / x_sd) * lambda

# Correlazioni tra i fattori
phi_posterior <- fit$draws("phi") # Estrai i draws per phi
phi_means <- apply(phi_posterior, c(2, 3), mean) # Calcola la media per ogni elemento della matrice di correlazione
fit_cors <- matrix(phi_means, nrow = 3, ncol = 3) # Crea la matrice di correlazione

# Confronto con i risultati lavaan
# `lav_par` sarà il dataframe dei parametri standardizzati ottenuto da lavaan.
lav_par <- parameterEstimates(fit_lav, standardized = TRUE)





