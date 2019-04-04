setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('review_classes.R')

# sampling options
model = 1
random_effects = FALSE
update_type = 'block_wise' # 'block_wise'   # 'component_wise'
smear_status = c('negative')
n_chains=1
n_iterations = 2000
n_burned = 1000
parallel=TRUE

analysis$run_mcmc_stan(model = model,n_chains=n_chains,n_iterations = n_iterations,n_burned = n_burned,
                       smear_status = smear_status, random_effects = random_effects,parallel=parallel)

#analysis$run_metropolis(model = model, n_iterations = n_iterations, n_burned = n_burned,
#                        smear_status = smear_status,random_effects=random_effects, update_type=update_type)

outputs = Outputs$new(analysis)
outputs$produce_stan_outputs()

# outputs$produce_mcmc_outputs(model = model, smear_status = smear_status, random_effects=random_effects)

