setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('load_data.R')

model=1
random_effects=TRUE
smear_status = c('negative')

analysis$run_metropolis(model = model, n_iterations = 100, n_burned = 10,smear_status = smear_status,random_effects=random_effects)
# View(analysis$metropolis_records)
#analysis$produce_mcmc_outputs(model = 1,smear_status = c('positive'))

outputs = Outputs$new(analysis)
outputs$produce_mcmc_outputs(model = model, smear_status = smear_status, random_effects=random_effects)