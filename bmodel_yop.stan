// Define data
data {
  int F;           // Number of families
  int N;           // Number of rows in data
  int K;           // Number of predictors
  int id[N];       // Family identifiers
  vector[N] f;     // Outcome variable
  matrix[N, K] Xf; // Matrix of (factual) predictors
  matrix[N, K] Xc; // Matrix of (counterfactual) predictors
}

// Define parameters
parameters {
  vector[F] alpha;                // Random intercept
  real mu_a;                      // Intercept mean
  vector[K] beta;                 // Coefficients
  vector<lower = 0>[N] u;         // "Inefficiency" term
  real gamma1;                    // Coefficient in scale of asymmetric term
  real gamma2;                    // Coefficient in scale of asymmetric term
  real<lower = 0> sigma_a2;       // Intercept standard deviation
  real<lower = 0> sigma_v2;       // Standard deviation on noise term
  real<lower = 0, upper = 1> rho; // Probability of asymmetric inclusion
}

// Parameter transformations
transformed parameters{
  vector<lower = 0>[N] lambda;
  real<lower = 0> sigma_a_raw;
  real<lower = 0> sigma_v_raw;
  for (i in 1:N){
    lambda[i] = exp(gamma1 + gamma2*(mu_a + alpha[id[i]] + Xf[i]*beta));
  }
  sigma_a_raw = sqrt(sigma_a2/100);
  sigma_v_raw = sqrt(sigma_v2/100);
}

// Define model
model {
  alpha ~ normal(0, sigma_a_raw);
  mu_a ~ cauchy(0, 1);
  sigma_a2 ~ inv_gamma(0.5, 0.0005*100);
  sigma_v2 ~ inv_gamma(0.5, 0.0005*100);
  beta ~ cauchy(0, 1);
  gamma1 ~ cauchy(0, 1);
  gamma2 ~ cauchy(0, 1);
  rho ~ beta(2, 2);
  for (i in 1:N){
    u[i] ~ exponential(1/lambda[i]);
    target += log_mix(rho,
    normal_lpdf(f[i] | mu_a + alpha[id[i]] + Xf[i]*beta - u[i], sigma_v_raw),
    normal_lpdf(f[i] | mu_a + alpha[id[i]] + Xf[i]*beta, sigma_v_raw));
  }
}

// Generate predicted capabilities
generated quantities {
  vector[N] c_f;
  vector[N] f_pred_f;
  vector[N] c_c;
  vector[N] u_c;
  vector[N] f_pred_c;
  for (i in 1:N){
    // Factual posterior predictive distribution
    c_f[i] = mu_a + alpha[id[i]] + Xf[i]*beta; 
    f_pred_f[i] = normal_rng(c_f[i] - bernoulli_rng(rho)*u[i], sigma_v_raw); 
    // Counterfactual posterior predictive distribution
    c_c[i] = mu_a + alpha[id[i]] + Xc[i]*beta; 
    u_c[i] = exponential_rng(1/exp(gamma1 + gamma2*c_c[i]));
    f_pred_c[i] = normal_rng(c_c[i] - bernoulli_rng(rho)*u_c[i], sigma_v_raw); 
  }
}
  