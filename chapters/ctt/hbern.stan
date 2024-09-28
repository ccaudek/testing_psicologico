data {
  int<lower=0> N;      // Number of subjects
  int<lower=0> N_items; // Number of timepoints
  array[N, N_items] int Y; // Binary responses for each subject and item
}

parameters {
  real<lower=0> sigma_theta; // SD of individual effects
  real mu_theta; // Mean of individual effects
  
  vector[N] theta_pr; // Non-centered individual-level parameters
}

transformed parameters {
  vector[N] theta = mu_theta + sigma_theta * theta_pr; // Individual-level effects
}

model {
  // Priors
  mu_theta ~ normal(0, 1);
  sigma_theta ~ normal(0, 1);
  theta_pr ~ normal(0, 1);
  
  // Likelihood
  for (i in 1:N) {
    for (j in 1:N_items) {
      Y[i, j] ~ bernoulli_logit(theta[i]);
    }
  }
}

generated quantities {
  array[N] real p; // Success probability estimate for each individual
  
  for (i in 1:N) {
    p[i] = inv_logit(theta[i]);
  }
}
