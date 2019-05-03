// Fit the hyperparameters of a latent-variable Gaussian process with an
// exponentiated quadratic kernel and a Gaussian likelihood and predict
// out-of-sample observations

data {
  int<lower=1> n;
  real x[n];
  real y[n];
  int<lower=1> pred_length;
  real pred_x[pred_length];
  int nruns;
}
transformed data {
  real delta = 1e-9;
  int<lower=1> N = n + pred_length;
  real joint_x[N];
  for (n1 in 1:n) joint_x[n1] = x[n1];
  for (n2 in 1:pred_length) joint_x[n + n2] = pred_x[n2];
}
parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  vector[N] eta;
}
transformed parameters {
  vector[N] f;
  {
    matrix[N, N] L_K;
    matrix[N, N] K = cov_exp_quad(joint_x, alpha, rho);
  
    // diagonal elements
    for (nn in 1:N)
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

  y ~ normal(f[1:n], sigma);
}
generated quantities {
  vector[pred_length] y2;
	  for (n2 in 1:pred_length) {
		y2[n2] = normal_rng(f[n + n2], sigma);
	  }
}
