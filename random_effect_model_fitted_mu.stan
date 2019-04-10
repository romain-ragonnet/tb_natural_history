// STAN file for the fixed-effect model
functions {
  
  // function to evaluate binomial probability based on mu_t and gamma
  real evaluate_p(real t_start,
                  real delta_t,
                  real e_mu,
                  real mu_t,
                  real gamma) {
    real p_t_start_1;
    real p_t_start_2;
    real A;
    real B;
    real p;
    
    // prob of being in state 1 (active TB) at time t_start
    p_t_start_1 = exp(-(gamma + e_mu + mu_t) * t_start);
    
    // prob of being in state 2 (recovered) at time t_start
    p_t_start_2 = (gamma / (gamma + mu_t)) *
      (exp(-e_mu * t_start) - exp(-(gamma + e_mu + mu_t) * t_start));
    
    // evaluate vector (1,0,0) times exponential of matrix Q (delta_t) times vector (0,0,1)
    A = 1 - (gamma / (gamma + mu_t)) * exp(-e_mu * delta_t) -
      (mu_t / (gamma + mu_t)) * exp(-(gamma + e_mu + mu_t) * delta_t);
    
    // evaluate vector (0,1,0) times exponential of matrix Q (delta_t) times vector (0,0,1)
    B = 1 - exp(-e_mu * delta_t);
    
    // combine the different components:
      p = (p_t_start_1 * A + p_t_start_2 * B) / (p_t_start_1 + p_t_start_2);
    
    return p;
  }
  
}

data {
  
  // dimensions
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of cohorts
  
  // observed data
  int<lower=0> n_at_risk[N];
  int<lower=0> n_new_deaths[N];
  vector<lower=0>[N] t_start;
  vector<lower=0>[N] delta_t;
  
  vector<lower=0>[K] mu_by_cohort;
  vector<lower=0>[K] mu_sd_by_cohort;
  
  
  // cohort indexing
  int<lower=1,upper=K> cohort_id[N];
  
}

parameters {
  
  // fcohort-specific parameters
  vector<lower=0, upper=1>[K] mu_t;  // TB mortality
  vector<lower=0, upper=1>[K] gamma; // intensity rate in Markov model (T -> R)
  
  vector<lower=0, upper=0.03>[K] e_mu; // estimated cohort-specific natural mortality
  
  // hyperparameters
  real<lower=0> lambda_mu_t;          // location for log normal dist for mu_t
  real<lower=0> lambda_gamma;         // location for log normal dist for gamma
  real<lower=0> sigma_mu_t;  // scale for log normal dist for mu_t
  real<lower=0> sigma_gamma; // scale for log normal dist for gamma
}

model {
  vector[N] p;
  real alpha;
  real beta;
  
  for (i in 1:N) {
    p[i] = evaluate_p(t_start[i],
                      delta_t[i],
                      e_mu[cohort_id[i]],
                      mu_t[cohort_id[i]],
                      gamma[cohort_id[i]]);
  }
  // binomial likelihood (vectorised)
  n_new_deaths ~ binomial(n_at_risk, p);
  
  // distribution for cohort specific parameters (vectorised)
  mu_t  ~ normal(lambda_mu_t,  sigma_mu_t);
  gamma ~ normal(lambda_gamma, sigma_gamma);
  
  // priors for estimated mu: gamma distributions
  for (j in 1:K) {
    alpha = pow(mu_by_cohort[j]/mu_sd_by_cohort[j], 2);
    beta = alpha/mu_by_cohort[j];
    e_mu[j] ~ gamma(alpha, beta);
  }
}

