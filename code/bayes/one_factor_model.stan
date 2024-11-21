data {
  int<lower=1> N; // sample size
  int<lower=1> P; // number of variables
  int<lower=1> K; // number of factors
  matrix[N, P] X; // data matrix of order [N,P]
}
transformed data {
  int<lower=1> L;
  L = P - K; // Number of free loadings
}
parameters {
  vector[P] b; // intercepts
  vector[L] lambda01; // initial factor loadings
  matrix[N, K] FS; // factor scores, matrix of order [N,K]
  corr_matrix[K] phi; // factor correlations
  vector<lower=0, upper=2>[K] sd_lv; // std dev of the latent factors
  vector<lower=0, upper=2>[P] sd_x; // std dev of the disturbances
  vector<lower=0, upper=5>[L] sd_lambda; // hyper parameter for loading std dev
}
transformed parameters {
  vector[L] lambda; // factor loadings
  
  lambda = lambda01 .* sd_lambda; // lambda as normal(0, sd_lambda)
}
model {
  matrix[N, P] mu;
  matrix[K, K] Ld;
  vector[K] muFactors;
  
  muFactors = rep_vector(0, K); // Factor means, set to zero
  Ld = diag_matrix(sd_lv) * cholesky_decompose(phi);
  
  for (n in 1 : N) {
    mu[n, 1] = b[1] + FS[n, 1]; // Extraversion
    mu[n, 2] = b[2] + FS[n, 1] * lambda[1];
    mu[n, 3] = b[3] + FS[n, 1] * lambda[2];
    
    mu[n, 4] = b[4] + FS[n, 2]; // Neuroticism
    mu[n, 5] = b[5] + FS[n, 2] * lambda[3];
    mu[n, 6] = b[6] + FS[n, 2] * lambda[4];
    
    mu[n, 7] = b[7] + FS[n, 3]; // Openness
    mu[n, 8] = b[8] + FS[n, 3] * lambda[5];
    mu[n, 9] = b[9] + FS[n, 3] * lambda[6];
  }
  
  // priors
  
  phi ~ lkj_corr(2.0);
  
  sd_x ~ cauchy(0, 2.5);
  sd_lambda ~ cauchy(0, 2.5);
  sd_lv ~ cauchy(0, 2.5);
  
  b ~ normal(0, 10);
  lambda01 ~ normal(0, 1);
  
  // likelihood
  
  for (i in 1 : N) {
    FS[i] ~ multi_normal_cholesky(muFactors, Ld);
    
    X[i] ~ normal(mu[i], sd_x);
  }
}
