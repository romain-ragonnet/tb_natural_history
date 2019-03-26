setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('load_data.R')

# sampling options
model = 1
random_effects = TRUE
update_type = 'component_wise' # 'block_wise'   # 'component_wise'
smear_status = c('positive')
n_iterations = 10
n_burned = 1


analysis$run_metropolis(model = model, n_iterations = n_iterations, n_burned = n_burned,
                        smear_status = smear_status,random_effects=random_effects, update_type=update_type)

outputs = Outputs$new(analysis)
outputs$produce_mcmc_outputs(model = model, smear_status = smear_status, random_effects=random_effects)