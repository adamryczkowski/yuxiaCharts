


#library(pryr)
gen_mean_sd_model<-function() {
  model<-"
  data {
  int<lower=0> n_repetitions;          // # schools
  real y[n_repetitions];               // estimated treatment
  real<lower=0> sigma[n_repetitions];  // std err of effect
  }

  parameters {
  real theta[n_repetitions];           // school effect
  real mu;                 // mean for schools
  real<lower=0> tau;       // variance between schools
  }

  model {
  theta ~ normal(mu, tau);
  y ~ normal(theta, sigma);
  }"
  library(rstan)
  data<-list(
    n_repetitions=6,
    y<-rep(10,6),
    sigma<-rep(1,6)
  )

  fit1<-stan(model_code = model,
             data = data,
             chains = 4)

  return(fit1)
}
#delayedAssign('mean_sd_model', rstan::stan_model(model_code=model), assign.env = pack)


#Small tau means all elements share the same mean mu.
#Tau is a standard deviation of all the mu
#sd(mu) - standard error of mu.

get_mean_sd<-function(values, ses, model){
  if(length(values)!=length(ses)) {
    stop("Length of values and standard errors must be the same")
  }
  library(rstan)
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())


  data<-list(
    n_repetitions=length(values),
    y<-values,
    sigma<-ses
  )

  # if(length(mean_sd_model)) {
  #   mean_sd_model$m<-stan_model(model_code=model)
  # }

  fit1<-stan(fit=model,
             #       model_code = model,
             data = data,
             chains = 4)
  a<-extract(fit1)
  return(data.frame(m=mean(a$mu), sd=sd(a$mu), tau=mean(a$tau), tau_sd=sd(a$tau)))

}

get_mean_sd_multiple<-function(df, gr_var, m_var, sd_var) {
  fn<-function(df, gr_var, m_var, sd_var) {
    gr_vars <- setdiff(colnames(df), c(gr_var, m_var, sd_var))
    values<-df %>%
      select(-one_of(sd_var)) %>%
      group_by_(.dots=gr_vars) %>%
      tidyr::spread_(gr_var, m_var) %>%
      ungroup() %>%
      select(-one_of(c(gr_vars, 'factor_var'))) %>%
      data.matrix()

    ses<-df %>%
      select(-one_of(m_var)) %>%
      group_by_(.dots=gr_vars) %>%
      tidyr::spread_(gr_var, sd_var) %>%
      ungroup() %>%
      select(-one_of(c(gr_vars, 'factor_var'))) %>%
      data.matrix()

    if(sum(ses==0)>0) {
      suniq<-unique(ses)
      ses[ses==0]<- min(suniq[suniq>0])
    }
    library(rstan)
    rstan::rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores())

    data<-list(
      n_repetitions=dim(values)[2],
      n_groups=dim(values)[1],
      y=(values),
      sigma=(ses)
    )
    file <- system.file('stan/norm-h2.stan', package = 'yuxiaCharts')
    if(!file.exists(file)) {
      stop(paste0("Cannot find file with the model at ", file ))
    }

    init_f<-function(chain_id) {
      fn<-Vectorize(rnorm, vectorize.args = c('mean','sd'))
      ans<-list(
        theta=fn(1, data$y, data$sigma),
        mu=fn(1, apply(data$y,1,mean), apply(data$sigma, 1,mean)),
        tau=rnorm(dim(data$y)[[1]], 0.02, 0.005)
      )
      dim(ans$theta)<-dim(data$y)
      return(ans)
    }
    fit1<-stan(file = file,
               data = data,
               chains = 4,
               init = init_f
               #             control = list(adapt_delta = 0.999)
    )
    a<-extract(fit1)
    df <- df %>%
      select(-one_of(m_var)) %>%
      group_by_(.dots=gr_vars) %>%
      tidyr::spread_(gr_var, sd_var) %>% select_(.dots=gr_vars)
    df<-cbind(df,
              m=apply(a$mu, 2, mean),
              m_se=apply(a$mu, 2, sd),
              tau=apply(a$tau, 2, mean),
              tau_se=apply(a$tau, 2, sd)
    )
    return(df)

  }
  ans<-fn(df, gr_var, m_var, sd_var)
  gc()
  return(ans)
}
