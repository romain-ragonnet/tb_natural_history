// STAN file for the fixed-effect model
functions {

  // function to evaluate binomial probability based on mu_t and gamma
  real evaluate_p(real t_start,
                  real delta_t,
                  real mu,
                  real mu_t,
                  real gamma) {
    real p_t_start_1;
    real p_t_start_2;
    real A;
    real B;
    real p;

    // prob of being in state 1 (active TB) at time t_start
    p_t_start_1 = exp(-(gamma + mu + mu_t) * t_start);

    // prob of being in state 2 (recovered) at time t_start
    p_t_start_2 = (gamma / (gamma + mu_t)) *
      (exp(-mu * t_start) - exp(-(gamma + mu + mu_t) * t_start));

    // evaluate vector (1,0,0) times exponential of matrix Q (delta_t) times vector (0,0,1)
    A = 1 - (gamma / (gamma + mu_t)) * exp(-mu * delta_t) -
      (mu_t / (gamma + mu_t)) * exp(-(gamma + mu + mu_t) * delta_t);

    // evaluate vector (0,1,0) times exponential of matrix Q (delta_t) times vector (0,0,1)
    B = 1 - exp(-mu * delta_t);

    // combine the different components:
    p = (p_t_start_1 * A + p_t_start_2 * B) / (p_t_start_1 + p_t_start_2);

    return p;
  }

}

data {

  // dimensions
  int<lower=0> N; // number of observations

  // observed data
  int<lower=0> n_at_risk[N];
  int<lower=0> n_new_deaths[N];
  vector<lower=0>[N] t_start;
  vector<lower=0>[N] delta_t;

  // known constant for intensity rate in Markov model (R -> D) cohort-dependent
  real<lower=0> mu[N];

}

parameters {

  // fixed effect parameters
  real<lower=0> mu_t;  // intensity rate in Markov model (component of T -> D)
  real<lower=0> gamma; // intensity rate in Markov model (T -> R)

}

transformed parameters {

  vector[N] p;

  for (i in 1:N) {
    p[i] = evaluate_p(t_start[i],
                      delta_t[i],
                      mu[i],
                      mu_t,
                      gamma);
  }

}

model {

  // binomial likelihood (vectorised)
  n_new_deaths ~ binomial(n_at_risk, p);

  // priors (all improper uniform, so not defined here)

}
