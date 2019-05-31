setwd('C:/Users/rrag0004/Models/tb_natural_history/')

source('review_classes.R')

# sampling options
analysis_name = 'main_analysis'
model = 1
smear_status = c('negative')
n_chains = 1
n_iterations = 60000
n_burned = 10000
thinning_period = 5
random_effects = TRUE
estimate_mu = TRUE
restrict_to = NA
priors = 'normal'

analysis = Analysis$new(smear_status, random_effects, estimate_mu, analysis_name, restrict_to=restrict_to, priors=priors)
analysis$run_mcmc_stan(model=model, n_chains=n_chains, n_iterations=n_iterations, n_burned=n_burned, thinning_period=thinning_period)

outputs = Outputs$new(analysis)
# outputs$produce_stan_outputs()
outputs$produce_mcmc_random_effect_graphs_from_stan(generate_cohort_profiles=FALSE)

outputs$write_disease_duration_and_cfr_to_file()
