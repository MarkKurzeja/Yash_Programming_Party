// Fit the hyperparameters of a latent-variable Gaussian process with an
// exponentiated quadratic kernel and a Gaussian likelihood and predict
// out-of-sample observations

data {
  int<lower=1> n;
  real x[n];
  real y[n];
  int<lower=1> pred_length;
  real pred_x[pred_length];
}
transformed data {
  real delta = 1e-9;
}
parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  vector[n] eta;
}
transformed parameters {
  vector[n] f;
  {
    matrix[n, n] L_K;
    matrix[n, n] K = cov_exp_quad(x, alpha, rho);
  
    // diagonal elements
    for (nn in 1:n)
      K[nn, nn] = K[nn, nn] + delta;
    
    L_K = cholesky_decompose(K);
    f = L_K * eta;
  }
}
model {
  rho ~ inv_gamma(5, 5);
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);
  eta ~ normal(0, 1);

  y ~ normal(f, sigma);
}
generated quantities {
  vector[pred_length] f2;
  vector[pred_length] y2;
  {
    matrix[pred_length, pred_length] L_K;
    matrix[pred_length, pred_length] K = cov_exp_quad(pred_x, alpha, rho);
  
    // diagonal elements
    for (nn in 1:pred_length)
      K[nn, nn] = K[nn, nn] + delta;
    
	f2 = multi_normal_rng(rep_vector(0, pred_length), K);
    // f2 = L_K * eta;
  }
  for (n2 in 1:pred_length) {
	y2[n2] = normal_rng(f2[n2], sigma);
  }
}
