setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('load_data.R')

#pars=list('gamma'=0.23935320 , 'mu'=1/55, 'mu_t'=0.08750983)
#LL= analysis$evaluate_pseudo_loglikelihood(model = 1, params=pars)
#print(LL)

z = analysis$plot_ll_surface(model=1, param_ranges=list('gamma'=c(0.1,0.4), 'mu_t'=c(0.01,0.15)), n_per_axis=10)