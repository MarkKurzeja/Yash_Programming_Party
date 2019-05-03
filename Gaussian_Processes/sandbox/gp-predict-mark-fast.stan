data {
	int<lower=1> N;
	real x[N];
	vector[N] y;
	int<lower = 1> pred_length;
}
// transformed data {
// }
parameters {
	vector[N] mu;
	real<lower=0> rho;
	real<lower=0> alpha;
	real<lower=0> sigma;
}
// transformed parameters {
// }
model {
	matrix[N, N] L_K;
	matrix[N, N] K = cov_exp_quad(x, alpha, rho);
	real sq_sigma = square(sigma);
	// diagonal elements
	for (n in 1:N)
		K[n, n] = K[n, n] + sq_sigma;
	L_K = cholesky_decompose(K);
	
	mu ~ student_t(3, 0, 5);
	rho ~ inv_gamma(5, 5);
	alpha ~ normal(0, 1);
	sigma ~ normal(0, 1);
	y ~ multi_normal_cholesky(mu, L_K);
}
generated quantities {
	matrix[N, N] L_K;
	matrix[N, N] K = cov_exp_quad(x, alpha, rho);
	vector[N] y2;
	real sq_sigma = square(sigma);
	// diagonal elements
	for (n in 1:N)
		K[n, n] = K[n, n] + sq_sigma;
	L_K = cholesky_decompose(K);
	y2 = multi_normal_cholesky_rng(mu, L_K);
}
