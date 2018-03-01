data {
  int<lower=0> n_repetitions;          // # schools
  int<lower=0> n_groups;
  real y[n_groups, n_repetitions];               // estimated treatment
  real<lower=0> sigma[n_groups, n_repetitions];  // std err of effect
}

parameters {
  real theta[n_groups, n_repetitions];           // school effect
  real mu[n_groups];                 // mean for schools
  real<lower=0> tau[n_groups];       // variance between schools
}

model {
  for(i in 1:n_groups) {
    theta[i] ~ normal(mu[i], tau[i]);
    y[i] ~ normal(theta[i], sigma[i]);
  }

}
