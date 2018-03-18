data {
  int<lower=0> n_times; // Number of time points
  real x[n_times]; //observed time points (not necessarily equally spaced)
  real<lower=0> period; // Period of the sinusoid component
  real y[n_times]; // observered values over time
  real<lower=0> sigma[n_times];  // observed standard dev. over time
  real prior_a; //prior for regression parameter
  real prior_b; //prior for regression parameter
}

transformed data  {
  real pi=3.141593;
}

parameters {
  real<lower=0> cnst;
  real<lower=0, upper=0.5> fi; //phase
  real a;
  real b; //weight of the periodic component. Sign is inferred from the phase
  real theta[n_times];   // const term independent from model for each measured value
  real<lower=0> tau;       // variance between schools
}


model {
  a ~ cauchy(0, prior_a);
  b ~ cauchy(0, prior_b);

  for(i in 1:(n_times)) {
    //\theta_i \sim \text{N}(C + a \cdot x_i + b * \sin\left(  (\frac{x_i}{\text{period}} + \phi ) \cdot 2 \pi \right), \tau)
    theta[i] ~ normal(cnst + a * x[i] + b * sin((x[i]/period + fi)*2*pi), tau); 
    y[i] ~ normal(theta[i], sigma[i]); //y_{i} \sim \text{N} \left( \theta_i, \sigma_i} \right)
  }
}


generated quantities{
  real pred_y[n_times];
  for(i in 1:n_times)
    pred_y[i] = cnst + a * x[i] + b * sin((x[i]/period + fi)*2*pi);
}


