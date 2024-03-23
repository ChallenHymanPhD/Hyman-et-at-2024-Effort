// Hyman et al 2024 Effort Model
data {
  int<lower=0> T;                                                               // Total samples : Months x  Years
  int<lower=0> OOS;                                                             // Total samples : Months x  Years (Out of sample)
  int<lower=0> S;                                                               // Total number of sigma_E predictors
  int<lower=0> E;                                                               // Total number of Effort model predictors
  vector[T] Effort;                                                             // Total effort (angler-trips) 
  matrix[T, E] Design_effort;                                                   // Effort design matrix
  matrix[OOS, E] Pred_effort;                                                   // Effort design matrix for out of sample predictions
  matrix[T, S] S_eff;                                                           // Sigma design matrix for Effort
  matrix[OOS, S] S_pred;                                                        // Sigma design matrix for out of sample predictions
}

parameters {
  vector[E] beta;                                                               // Coefficients for Effort model
  vector[S] tau_effort;                                                         // Noise terms for effort model (log-scale)
  vector[2] omega;                                                              // Region-specific effects of mean on noise term
}

transformed parameters {
  vector[T] mu_effort;                                                          // Effort mean
  vector[T] sigma_effort;                                                       // Effort model sigmas
  for (t in 1:T){
    mu_effort[t] = Design_effort[t,]*beta;
    sigma_effort[t] = exp(S_eff[t,]*tau_effort + S_eff[t,]*mu_effort[t]*omega);
  }
}

model {
  for (t in 1:T){
    Effort[t] ~ lognormal(mu_effort[t], sigma_effort[t]);
  }
  // Priors
  beta ~ normal(0,5);
  tau_effort ~ normal(0,1);
  omega ~ normal(0, 1);
}

generated quantities {
  vector[T] pred_Effort;
  vector[T] pred_mu;
  vector[T] pred_sigma;
  vector[OOS] oos_Effort;
  vector[OOS] oos_mu;
  vector[OOS] oos_sigma;
  for (t in 1:T){
    pred_mu[t] = Design_effort[t,]*beta;
    pred_sigma[t] = exp(S_eff[t,]*tau_effort + S_eff[t,]*pred_mu[t]*omega);
    pred_Effort[t] = lognormal_rng(pred_mu[t], pred_sigma[t]);
  }
  for (o in 1:OOS){
    oos_mu[o] = Pred_effort[o,]*beta;
    oos_sigma[o] = exp(S_pred[o,]*tau_effort + S_pred[o,]*oos_mu[o]*omega);
    oos_Effort[o] = lognormal_rng(oos_mu[o], oos_sigma[o]);
  }
}
