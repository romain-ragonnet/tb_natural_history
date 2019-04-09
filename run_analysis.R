setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('review_classes.R')

# sampling options
model = 1
smear_status = c('positive')
n_chains = 1
n_iterations = 20000
n_burned = 10000
random_effects = TRUE
estimate_mu = FALSE

analysis = Analysis$new(smear_status, random_effects, estimate_mu)

analysis$run_mcmc_stan(model=model, n_chains=n_chains, n_iterations=n_iterations, n_burned=n_burned)

outputs = Outputs$new(analysis)
outputs$produce_stan_outputs()
outputs$produce_mcmc_random_effect_graphs_from_stan()

