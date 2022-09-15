
##Reconstruction
predMod = "
data {
  int<lower=0> n_new;   // No 'new' y measurements to infer x_new from
  vector[n_new] y_new;

  int<lower=0> posts;
  vector[posts] B0;              
  vector[posts] B1;
  vector[posts] sig;   
}

parameters {
  matrix[n_new, posts] x_new;   # NB: flat priors
}

model {
  vector[posts] y_new_hat; 
  for(i in 1:n_new){
    y_new_hat = B0 + B1 .* x_new[i,]';
    y_new[i] ~ normal(y_new_hat, sig);
}
}
"

stan_date <- list(n_new = nrow(recData), 
                  y_new= recData$D47, 
                  posts = length(vects.params$beta), 
                  B0 = vects.params$alpha,
                  B1 = vects.params$beta,
                  sig = vects.params$sigma)
iter = 1000
data.rstan <- stan(data = stan_date, model_code = predMod, 
                   chains = 2, iter = iter, warmup = iter/2,
                   thin = 1)
print(data.rstan)

params2 <- extract(data.rstan)
Xouts2 <- params2$x_new
Xdims2 <-dim(Xouts2)
for(xi in 1: Xdims2[2]){
  xis <- as.vector(Xouts2[,xi,])
  Xe4[xi,] <- quantile(xis, Cqts)
}



