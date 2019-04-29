setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('review_classes.R')

# sampling options
analysis_name = 'gamma_priors'
model = 1
smear_status = c('positive')
n_chains = 1
n_iterations = 11000
n_burned = 1000
random_effects = TRUE
estimate_mu = TRUE
restrict_to = NA
priors = 'gamma'

analysis = Analysis$new(smear_status, random_effects, estimate_mu, analysis_name, restrict_to=restrict_to, priors=priors)
analysis$run_mcmc_stan(model=model, n_chains=n_chains, n_iterations=n_iterations, n_burned=n_burned)

outputs = Outputs$new(analysis)
# outputs$produce_stan_outputs()
outputs$produce_mcmc_random_effect_graphs_from_stan(generate_cohort_profiles=FALSE)

