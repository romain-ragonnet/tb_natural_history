setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('load_data.R')

# pars=list('gamma'=0.11678 , 'mu'=1/55, 'mu_t'=0.21667)  # best LL for smear-positive
# LL= analysis$evaluate_pseudo_loglikelihood(model = 1, params=pars)
# print(LL)

# param_ranges=list('gamma'=c(0.011,0.13), 'mu_t'=c(0.21,0.24))
# n_per_axis=10
# z = analysis$plot_ll_surface(model=1, param_ranges=param_ranges, n_per_axis=n_per_axis,smear_status = c("positive"))


analysis$run_metropolis(model = 1,n_iterations = 100,smear_status = c('positive'))
# View(analysis$metropolis_records)
#analysis$produce_mcmc_outputs(model = 1,smear_status = c('positive'))

outputs = Outputs$new(analysis)
outputs$produce_mcmc_outputs(model = 1)
