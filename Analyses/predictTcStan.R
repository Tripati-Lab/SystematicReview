library(coda)

BayesianPredictions <- function(calModel,
                                calData,
                                recData,
                                iter = 500,
                                warmup = 100){
  
vects.params <- extract(calModel)

##Reconstruction
predMod = "
data {
  int<lower=0> n;
  vector[n] y;

  int<lower=0> posts;
  vector[posts] alpha;              
  vector[posts] beta;
  vector[posts] sigma;  
  real prior_mu;
  real prior_sig;
}

parameters {
  matrix[n, posts] x_new;
}

model {
  vector[posts] y_new_hat; 
  for(i in 1:n){
    x_new[i,] ~ normal(prior_mu, prior_sig);
    y_new_hat = alpha + beta .* x_new[i,]';
    y[i] ~ normal(y_new_hat, sigma);
}
}
"

stan_date <- list(n = nrow(recData), 
                  y = recData$D47, 
                  posts = length(vects.params$beta), 
                  alpha = vects.params$alpha,
                  beta = vects.params$beta,
                  sigma = vects.params$sigma,
                  prior_mu = 11,
                  prior_sig = 5)
iter = 3000
options(mc.cores = parallel::detectCores())
data.rstan <- stan(data = stan_date, model_code = predMod, 
                   chains = 4, iter = iter, warmup = floor(iter/2),
                   thin = 1, control = list(adapt_delta = 0.90, max_treedepth = 10))

params2 <- extract(data.rstan)
Xouts2 <- params2$x_new
Xdims2 <- dim(Xouts2)
xis <- list()
recs <- lapply(1:Xdims2[2], function(x){
  xis <- sqrt( 10^6/as.vector(Xouts2[,x,])) - 273.15
  quantile(xis, c(0.025, 0.975))
})

preds <- cbind.data.frame(recData, do.call(rbind, recs))


return(datPred)
}
