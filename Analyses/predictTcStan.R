library(coda)

BayesianPredictions <- function(calModel,
                                calData,
                                recData,
                                iter = 500,
                                warmup = 100){
  
vects.params <- extract(calModel)

sd(lmcals$alpha)
sd(lmcals$beta)

sd(vects.params$alpha)
sd(vects.params$beta)
mean(vects.params$alpha)
mean(vects.params$beta)

##Reconstruction
predMod = "
 data {
    int<lower=0> N;
    int<lower=0> M; 
    vector[N] y;
    vector[M] beta;
    vector[M] alpha;
    vector[M] sigma;
    vector[N] prior_mu;
    real prior_sig;
 }

parameters {
    matrix<lower=0>[N,M] x;
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

stan_date <- list(N = nrow(recData), 
                  M = length(vects.params$alpha),
                  y = recData$D47, 
                  beta = vects.params$beta, 
                  alpha = vects.params$alpha,
                  sigma = vects.params$sigma,
                  prior_mu = rep(11, nrow(recData)),
                  prior_sig = sd(calData$Temperature)
                  )

data.rstan <- stan(data = stan_date, model_code = predMod, 
                   chains = 2, iter = iter, warmup = warmup,
                   thin = 1)
s <- mcmc.list(lapply(1:ncol(data.rstan), function(x) mcmc(as.array(data.rstan)[,x,])))
s <- do.call(rbind,s)


tT <- s[,grep("mu2", colnames(s))]
index <- rep(1:nrow(recData), each=1, times = ncol(tT)/nrow(recData))
indexSample <- rep(as.numeric(as.factor(recData$Sample)), times = ncol(tT)/nrow(recData))

predTemp <- lapply(unique(indexSample), function(y){
  tTSub <- tT[,which(indexSample == y)]
    cbind.data.frame(meanTemp = mean(tTSub), 
                     sdTemp = sd(tTSub)
    )
})

predTemp <- do.call(rbind, predTemp)
datPred <- cbind.data.frame(meanD47 = aggregate(recData$D47, list(recData$Sample), mean)[,2]
                 , sdD47 = aggregate(recData$D47, list(recData$Sample), sd)[,2]
                 , predTemp)
attr(datPred, "priors") <-  s

return(datPred)
}
