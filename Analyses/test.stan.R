fwMod = "
  data {
    int<lower = 0> N;
    vector[N] x;
    vector[N] y;
    real beta_mu;
    real beta_sd;
    real alpha_mu;
    real alpha_sd;
  }

  parameters {
    real alpha;
    real beta;
    real sigma;
  }
  
  model {
    alpha ~ normal(alpha_mu, alpha_sd);
    beta ~ normal(beta_mu, beta_sd);
    sigma ~ cauchy(0,5);
    y ~ normal(alpha + beta * x, sigma);
  }
"

library(rstan)

nChains = 2
burnInSteps = 1000
thinSteps = 1
numSavedSteps = 3000
nIter = ceiling(burnInSteps + (numSavedSteps * thinSteps)/nChains)

stan_date <- list(N = nrow(calData), 
                  x = calData$Temperature,
                  y = calData$D47, 
                  beta_mu =  0.039,
                  beta_sd = 0.004,
                  alpha_mu = 0.231,
                  alpha_sd = 0.065 )

data.rstan.cal <- stan(data = stan_date, model_code = fwMod, 
                   chains = 2, iter = nIter, warmup = burnInSteps,
                   thin = thinSteps)
data.rstan.cal

vects.cal <- extract(data.rstan.cal)
##Reconstruction
predMod = "
 data {
    int<lower=0> N; // number of data items
    int<lower=0> M; // number of posterior samples
    vector[N] y; // predictor
    vector[M] beta; // beta
    vector[M] alpha; // alpha
    vector[M] sigma; // sigma
    vector[N] prior_mu;
    real prior_sig;
 }

parameters {
    matrix<lower=0>[N,M] x; // temperature to estimate
}

transformed parameters {
matrix[N, M] mu2 = sqrt(pow(10,6)*inv(x)) - 273.15;
}

model {
   vector[N] mu;
for (m in 1:M) {
    x[:,m] ~ normal(prior_mu, prior_sig);
    mu = alpha[m] + beta[m] * x[:,m];
    y ~ normal(mu, sigma[m]);
 }
}
"

library(rstan)

nChains = 2
burnInSteps = 1000
thinSteps = 1
numSavedSteps = 3000  #across all chains
nIter = ceiling(burnInSteps + (numSavedSteps * thinSteps)/nChains)

stan_date <- list(N = 3, 
                  M = length(vects.cal$alpha[1:10]),
                  y = c(0.6, 0.7, 0.8), 
                  beta = vects.cal$beta[1:10], 
                  alpha = vects.cal$alpha[1:10],
                  sigma = vects.cal$sigma[1:10],
                  prior_mu = rep(11, 3),
                  prior_sig = 5)

data.rstan <- stan(data = stan_date, model_code = predMod, 
                        chains = 2, iter = nIter, warmup = burnInSteps,
                        thin = thinSteps)


pe <- sqrt(10^6/(9.07)) - 273.15
sd <- (sqrt(10^6/(9.07)) - 273.15) - (sqrt(10^6/(9.07 + 0.1)) - 273.15)


vects <- extract(data.rstan, pars = "mu2")
matrix_of_draws <- as.data.frame(vects)



