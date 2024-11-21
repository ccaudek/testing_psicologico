//stan code for illustrative example 4
// ------ data block ------;
data {
int<lower=0> nobs; // number of subjects
matrix[nobs,8] y7; // observed indicators
matrix[nobs,8] y8;
matrix[nobs,8] y9;
matrix[nobs,8] y10;
matrix[nobs,8] y11;
matrix[nobs,8] y12;
matrix[nobs,8] y21;
matrix[nobs,8] y22;
matrix[nobs,8] y23;
matrix[nobs,8] y24;
matrix[nobs,8] y25;
matrix[nobs,8] y26;
matrix[nobs,8] y27;
}
// ------ parameter block ------;
parameters {
vector[3] eta_means; // latent means
// indicator intercepts
real a8;
real a9;
real a10;
real a11;
real a12;
real a21;
real a22;
real a23;
real a24;
real a25;
real a26;
real a27;
// factor loadings
real g8;
real g9;
real g10;
real g11;
real g12;
real g21;
real g22;
real g23;
real g24;
real g25;

real g26;
real g27;
// covariance matrix for 2nd order latent variables
cov_matrix[3] sigma2_eta;
// covariance matrix for observed indicator residual variances
cov_matrix[8] sigma2_7;
cov_matrix[8] sigma2_8;
cov_matrix[8] sigma2_9;
cov_matrix[8] sigma2_10;
cov_matrix[8] sigma2_11;
cov_matrix[8] sigma2_12;
cov_matrix[8] sigma2_21;
cov_matrix[8] sigma2_22;
cov_matrix[8] sigma2_23;
cov_matrix[8] sigma2_24;
cov_matrix[8] sigma2_25;
cov_matrix[8] sigma2_26;
cov_matrix[8] sigma2_27;
// latent variables as parameters
matrix[nobs, 3] eta; matrix[nobs, 8] u; // latent growth constructs
// latent construct disturbances
}
// ------ transformed parameters block ------;
transformed parameters {
// declare the parameters
real mean_v; real var_v; // mean of the intra-individual variance
// variance of the intra-individual variance
matrix[nobs, 8] pa; // latent construct
// define transformation
mean_v = exp(eta_means[3] + sigma2_eta[3,3]/2);
var_v = (exp(sigma2_eta[3,3]) - 1) *exp(2* eta_means[3] +
sigma2_eta[3,3]);
for (i in 1:nobs){
for(t in 1:8){
pa[i, t] = eta[i,1] + (t-1)*eta[i, 2] + u[i, t];
}
}
}
// ------ model block ------;
model {
// priors for latent factor means
eta_means[1] ~ normal(0, 1e+07);
eta_means[2] ~ normal(0, 1e+07);
eta_means[3] ~ normal(0, 1e+07);
// priors for item intercepts
a8 ~ normal(0, 1e+07);
a9 ~ normal(0, 1e+07);
a10 ~ normal(0, 1e+07);
a11 ~ normal(0, 1e+07);
a12 ~ normal(0, 1e+07);
a21 ~ normal(0, 1e+07);
a22 ~ normal(0, 1e+07);
a23 ~ normal(0, 1e+07);
a24 ~ normal(0, 1e+07);
a25 ~ normal(0, 1e+07);
a26 ~ normal(0, 1e+07);
a27 ~ normal(0, 1e+07);
// priors for item loadings
g8 ~ normal(0, 1e+07);
g9 ~ normal(0, 1e+07);
g10 ~ normal(0, 1e+07);
g11 ~ normal(0, 1e+07);
g12 ~ normal(0, 1e+07);
g21 ~ normal(0, 1e+07);
g22 ~ normal(0, 1e+07);
g23 ~ normal(0, 1e+07);
g24 ~ normal(0, 1e+07);
g25 ~ normal(0, 1e+07);
g26 ~ normal(0, 1e+07);
g27 ~ normal(0, 1e+07);
// growth factors
for (i in 1:nobs){
    eta[i, 1:3] ~ multi_normal(eta_means[1:3], sigma2_eta);
    // level-1 latent factor disturbances
    for(t in 1:8){
        u[i, t] ~ normal(0, sqrt(exp(eta[i, 3]))); //factor for each person
    }
    // depends on log variance
    // observed indicators
    y7[i, 1:8] ~ multi_normal(0 + 1*pa[i, 1:8], sigma2_7);
    y8[i, 1:8] ~ multi_normal(a8 + g8*pa[i, 1:8], sigma2_8);
    y9[i, 1:8] ~ multi_normal(a9 + g9*pa[i, 1:8], sigma2_9);
    y10[i, 1:8] ~ multi_normal(a10 + g10*pa[i, 1:8], sigma2_10);
    y11[i, 1:8] ~ multi_normal(a11 + g11*pa[i, 1:8], sigma2_11);
    y12[i, 1:8] ~ multi_normal(a12 + g12*pa[i, 1:8], sigma2_12);
    y21[i, 1:8] ~ multi_normal(a21 + g21*pa[i, 1:8], sigma2_21);
    y22[i, 1:8] ~ multi_normal(a22 + g22*pa[i, 1:8], sigma2_22);
    y23[i, 1:8] ~ multi_normal(a23 + g23*pa[i, 1:8], sigma2_23);
    y24[i, 1:8] ~ multi_normal(a24 + g24*pa[i, 1:8], sigma2_24);
    y25[i, 1:8] ~ multi_normal(a25 + g25*pa[i, 1:8], sigma2_25);
    y26[i, 1:8] ~ multi_normal(a26 + g26*pa[i, 1:8], sigma2_26);
    y27[i, 1:8] ~ multi_normal(a27 + g27*pa[i, 1:8], sigma2_27);
    }
} // end of model block
