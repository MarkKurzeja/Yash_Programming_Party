functions {
  // Link for the matern function: http://www.gaussianprocess.org/gpml/chapters/RW4.pdf p85
  
  matrix cov_exp_quad_cust(real[] x, real[] y, real alpha, real rho) {
    // return(cov_exp_quad(x, y, alpha, rho));
    
    // Work with the matrix operations to get the smoothed exp going
    int N1 = size(x);
    int N2 = size(y);
    matrix[N1, N2] result;                                                                                                matrix[N1, N1] D;                                                                                                
    // Compute the difference as a matrix operation
    result = to_vector(x) * rep_row_vector(1, N2) - rep_vector(1, N1) * to_row_vector(y);
    
    // Square the matrix and compute the result
    D = diag_matrix(rep_vector(1 / (2 * square(rho)), N1));
    result = square(alpha) * exp(D * result^2);
    
    
    // for (i in 1:N1) {
    //   for (j in 1:N2) {
    //     result[i,j] = alpha^2 * exp(-square(x[i] - y[j]) / (2 * rho^2));
    //   }
    //   // result[j, i] = result[i,j];
    // }
    return(result);
  }
  
  vector gp_pred_rng(real[] x2,
                     vector y1, real[] x1,
                     real alpha, real rho, real sigma, real delta) {
    int N1 = rows(y1);
    int N2 = size(x2);
    vector[N2] f2;
    {
      matrix[N1, N1] K =   cov_exp_quad_cust(x1, x1, alpha, rho)
                         + diag_matrix(rep_vector(square(sigma), N1));
      matrix[N1, N1] L_K = cholesky_decompose(K);

      vector[N1] L_K_div_y1 = mdivide_left_tri_low(L_K, y1);
      vector[N1] K_div_y1 = mdivide_right_tri_low(L_K_div_y1', L_K)';
      matrix[N1, N2] k_x1_x2 = cov_exp_quad_cust(x1, x2, alpha, rho);
      vector[N2] f2_mu = (k_x1_x2' * K_div_y1);
      matrix[N1, N2] v_pred = mdivide_left_tri_low(L_K, k_x1_x2);
      matrix[N2, N2] cov_f2 =   cov_exp_quad_cust(x2, x2, alpha, rho) - v_pred' * v_pred
                              + diag_matrix(rep_vector(delta, N2));
      f2 = multi_normal_rng(f2_mu, cov_f2);
    }
    return f2;
  }
}

data {
  int<lower=1> N;
  real x[N];
  vector[N] y;

  int<lower=1> N_predict;
  real x_predict[N_predict];
  
}

parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
}

model {
  matrix[N, N] cov =   cov_exp_quad_cust(x, x, alpha, rho)
                     + diag_matrix(rep_vector(square(sigma), N));
  matrix[N, N] L_cov = cholesky_decompose(cov);

  // P[rho < 2.0] = 0.01
  // P[rho > 10] = 0.01
  rho ~ inv_gamma(8.91924, 34.5805);
  alpha ~ normal(0, 2);
  sigma ~ normal(0, 1);

  y ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
}

generated quantities {
  vector[N_predict] f_predict = gp_pred_rng(x_predict, y, x, alpha, rho, sigma, 1e-10);
  vector[N_predict] y_predict;
  for (n in 1:N_predict)
    y_predict[n] = normal_rng(f_predict[n], sigma);
}
