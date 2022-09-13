##Model without errors

fwMod_NE = "
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
    sigma ~ cauchy(0, 5);
    y ~ normal(alpha + beta * x, sigma);
  }
"


##With errors

fwMod_Errors = "
data {
  int<lower=0> N;       // number of cases
  vector[N] y;          // outcome (variate)
  vector[N] x_meas;   // measurement of x
  real<lower=0> tau;     // measurement noise
  real beta_mu;
  real beta_sd;
  real alpha_mu;
  real alpha_sd;
  real mu_x;
  real sigma_x;

}
parameters {
  vector[N] x;    // unknown true value
  //real mu_x;          // prior location
  //real sigma_x;       // prior scale
  real alpha;           // intercept
  real beta;            // slope
  real<lower=0> sigma;  // outcome noise
}
model {
  beta ~ normal(beta_mu, beta_sd);
  alpha ~ normal(alpha_mu, alpha_sd);
  
  x ~ normal(mu_x, sigma_x);  // prior
  x_meas ~ normal(x, tau);    // measurement model
  y ~ normal(alpha + beta * x, sigma);

  sigma ~ cauchy(0, 5);
}
"

#Mixed (yi=αj[i]+βj[i]xi+ϵi)

fwMod_mixed = "
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  vector[N] x;
  int Material[N];
  real beta_mu;
  real beta_sd;
  real alpha_mu;
  real alpha_sd;
}
parameters {
  real<lower=0> sigma;
  vector[J] alpha;
  vector[J] beta;
}
model {
  alpha ~ normal(alpha_mu, alpha_sd);
  beta ~ normal(beta_mu, beta_sd);
  y ~ normal(alpha[Material] + beta[Material].*x, sigma);
  sigma ~ cauchy(0, 5);
}
"

matVector <- sample(c(1, 2), nrow(calData), replace = TRUE )

stan_data_mixed <- list(N = nrow(calData), 
                     x = calData$Temperature,
                     y = calData$D47, 
                     J = length(unique(matVector)),
                     Material = matVector,
                     beta_mu =  0.039,
                     beta_sd = 0.004,
                     alpha_mu = 0.231,
                     alpha_sd = 0.065)

BLM3 <- stan(data = stan_data_mixed, model_code = fwMod_mixed, 
                chains = 2, iter = nIter, warmup = burnInSteps,
                thin = thinSteps, pars = c('alpha', 'beta'))




##Fitting models

library(rstan)

nChains = 2
burnInSteps = 1000
thinSteps = 1
numSavedSteps = 3000
nIter = ceiling(burnInSteps + (numSavedSteps * thinSteps)/nChains)

stan_data_NE <- list(N = nrow(calData), 
                  x = calData$Temperature,
                  y = calData$D47, 
                  beta_mu =  0.039,
                  beta_sd = 0.004,
                  alpha_mu = 0.231,
                  alpha_sd = 0.065)

BLM1_NE <- stan(data = stan_data_NE, model_code = fwMod_NE, 
                       chains = 2, iter = nIter, warmup = burnInSteps,
                       thin = thinSteps)



stan_data_Err <- list(N = nrow(calData), 
                  x_meas = calData$Temperature,
                  y = calData$D47, 
                  tau = mean(calData$TempError),
                  beta_mu =  0.039,
                  beta_sd = 0.004,
                  alpha_mu = 0.231,
                  alpha_sd = 0.065 ,
                  mu_x = mean(calData$Temperature),
                  sigma_x = sd(calData$Temperature))

BLM1_E <- stan(data = stan_data_Err, model_code = fwMod_Errors, 
                       chains = 2, iter = nIter, warmup = burnInSteps,
                       thin = thinSteps, pars = c('alpha', 'beta'))


vects.NE <- extract(BLM1_NE)
vects.E <- extract(BLM1_E)


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

targetD47 <- c(0.6, 0.7, 0.8)

stan_date <- list(N = length(targetD47), 
                  M = length(vects.NE$alpha),
                  y = targetD47, 
                  beta = vects.NE$beta, 
                  alpha = vects.NE$alpha,
                  sigma = vects.NE$sigma,
                  prior_mu = rep(11, length(targetD47)),
                  prior_sig = 5)

data.rstan <- stan(data = stan_date, model_code = predMod, 
                        chains = 2, iter = 1000, warmup = 500,
                        thin = thinSteps)
library(coda)
s <- mcmc.list(lapply(1:ncol(data.rstan), function(x) mcmc(as.array(data.rstan)[,x,])))
s <- do.call(rbind,s)

tT <- s[,grep("mu2", colnames(s))]
index <- rep(1:length(targetD47), each=1, times = ncol(tT)/length(targetD47))

do.call(rbind, lapply(1:length(targetD47), function(x){
cbind.data.frame(targetD47 = targetD47[x],  
                 mean = mean(tT[,which(index == x)]), 
                 sd = sd(tT[,which(index == x)]))
}))







