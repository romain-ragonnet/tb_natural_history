library(R6)
library(forecast)
source('G:/My Drive/R_toolkit/graph_tools.R')

Cohort <- R6Class(
  "Cohort",
  public = list(
    id = NULL,
    author = '',
    smear_status = '',
    cohort_name = '',
    year_range = NULL,
    cohort_size = 0,
    times = c(),
    perc_death = NULL,
    perc_alive = NULL,
    formatted_data = NULL,
  
    initialize = function(id, author, smear_status, cohort_name, year_range,
                          cohort_size, times, perc_death, perc_alive){
      self$id = id
      self$author = author
      self$smear_status = smear_status
      self$cohort_name = cohort_name
      self$year_range = year_range
      self$cohort_size = cohort_size
      self$times = times
      self$perc_death = perc_death
      self$perc_alive = perc_alive
      

      # work with death percentages only
      if (is.null(self$perc_death)){
        self$perc_death = 100 - self$perc_alive
        self$perc_alive = NULL
      }
      
      # check
      if (length(self$times) != length(self$perc_death)){
        print("Warning: times and survival data do not have same length")
      }
      
      # add the data for t=0
      if (!(0 %in% self$times)){
        self$times = c(0, self$times)
        if (is.null(self$perc_death)){
          self$perc_alive = c(100, self$perc_alive)
        }else{
          self$perc_death = c(0, self$perc_death)
        }
      }
      
      # format the data
      if (!is.null(self$cohort_size)){
        self$format_data()
      }
    },
    
    format_data = function(){
      n_rows = length(self$times) - 1
      u = rep(NA,n_rows)
      formatted_data = data.frame('t_start'=u, 'delta_t'=u, 'n_at_risk'=u, 'n_new_deaths'=u, 'smear_status'=u, 'cohort_id'=u)
      
      for (i in 1:n_rows){  # i represent the index of the starting time
        formatted_data$t_start[i] = self$times[i]
        formatted_data$delta_t[i] = self$times[i+1] - self$times[i]
        formatted_data$n_at_risk[i] = round((1 - self$perc_death[i]/100) * self$cohort_size)
        formatted_data$n_new_deaths[i] = round((self$perc_death[i+1] - self$perc_death[i])*self$cohort_size/100)
        formatted_data$smear_status[i] = self$smear_status
        formatted_data$cohort_id[i] = self$id
        
        if (formatted_data$n_at_risk[i] < formatted_data$n_new_deaths[i]){
          print("WARNING: more dead persons than at_risk persons")
        }
      }
      self$formatted_data = formatted_data
      
    },
    
    plot_cohort_data = function(){
      title = paste(self$author, self$smear_status, self$cohort_name)
      if (is.null(self$perc_death)){
        plot(self$times, self$perc_alive, main=title, 
             xlab='time (years)', ylab='percentage still alive')
      }else{
        plot(self$times, self$perc_death, main=title,
             xlab='time (years)', ylab='percentage dead')
      }
    },
    
    get_plot_color = function(){
      color=list(
        "Baart De La Faille" = my_blue,
        "Buhl" = my_red,
        "Griep" = my_purple,
        "Tattersall" = my_gold,
        "Thompson" = my_brown,
        "Hartley" = 'black',
        "Braeuning" = my_green,
        "Backer" = my_grey,
        "Trail" = 'blue',
        "Sinding-Larsen" = 'purple',
        "Furth" = 'darkgreen',
        "Berg" = 'yellow',
        "Munchbach" = "orange"
      )
      # determine an alpha between 0.5 and 1
      # 0.5 for the oldest times (1905), 1 for the most recent (1940) 
      alpha = (mean(self$year_range) - 1905)/35
      add.alpha(color[[self$author]], alpha)      
    }
  )
)

Analysis <- R6Class(
    "Analysis",
    public = list(
      cohorts = c(),
      n_cohorts = 0,
      all_data = NULL,
      proposal_sd = list(
        list('gamma'=0.01, 'mu_t'=0.01),  # model 1
        list('gamma'=0.01, 'mu_t'=0.01, 'kappa'=0.01, 'alpha'= 0.01)  # model 2
        ),
      mu = 1/55,
      metropolis_records = NULL,
      burned_iterations = 0,
     
      add_cohort = function(author, smear_status, cohort_name, year_range,
                            cohort_size, times, perc_death, perc_alive){
        cohort_id = self$n_cohorts + 1
        cohort = Cohort$new(cohort_id, author, smear_status, cohort_name, year_range,
                            cohort_size, times, perc_death, perc_alive)
        self$cohorts = c(self$cohorts, cohort)
        self$n_cohorts = cohort_id
      },
     
      produce_main_dataframe = function(){
        self$all_data = data.frame('t_start'=double(), 'delta_t'=double(), 'n_at_risk'=integer(), 'n_new_deaths'=integer(),'smear_status'=character(), 'cohort_id'=integer())
        
        for (c in self$cohorts){
          self$all_data = rbind(self$all_data, c$formatted_data)
        }
        
      },

      # ____________________________________
      #  MCMC algorithm for the fixed effects model
      get_individual_death_proba = function(data_item, model, params){
        # returns the probability of death according to the model given the information provided in data_item
        # That is, individual is still alive at time t_start and we want to know the probability of death at time
        # t_start + delta_t
        
        if (model == 1){
          # we use the three-state model
          attach(params)
          # proba of being in state 1 (active TB) at time t_start 
          P_t_start_1 = exp(-(gamma + mu + mu_t)*data_item$t_start)
          # proba of being in state 2 (recovered) at time t_start 
          P_t_start_2 = (gamma / (gamma + mu_t)) * (exp(-mu*data_item$t_start) - exp(-(gamma + mu + mu_t)*data_item$t_start))
          
          
          # evaluate vector (1, 0, 0) times exponetial of matrix Q (delta_t) times vector (0 0 1)
          A = 1 - (gamma / (gamma + mu_t)) * exp(-mu * data_item$delta_t) - (mu_t/(gamma + mu_t) )* exp(-(gamma + mu + mu_t) * data_item$delta_t)   
            
          # evaluate vector (0, 1, 0) times exponetial of matrix Q (delta_t) times vector (0 0 1)
          B = 1 - exp(-mu * data_item$delta_t)
          
          # combine the different components:
          p = (P_t_start_1 * A + P_t_start_2 * B) / (P_t_start_1 + P_t_start_2)
          
          detach(params)
        }else if(model == 2){
          attach(params)
          # proba of being in state 1 (active TB early) at time t_start 
          P_t_start_1 = exp(-(gamma + mu + mu_t + kappa)*data_item$t_start)
          
          # proba of being in state 2 (active TB late) at time t_start 
          P_t_start_2 = (kappa/(kappa+(1-alpha)*mu_t)) * (exp(-(gamma + mu + alpha*mu_t)*data_item$t_start) - exp(-(gamma+kappa+mu+mu_t)*data_item$t_start))
          
          # proba of being in state 3 (Recovered) at time t_start
          deno = (gamma+kappa+mu_t) * (kappa+(1-alpha)*mu_t) * (gamma+alpha*mu_t)
          cste = gamma/deno
          num_1 = -kappa*(gamma+kappa+mu_t)*exp(-(gamma+mu+alpha*mu_t)*data_item$t_start)
          num_2 = mu_t*(alpha-1)*(gamma+alpha*mu_t)*exp(-(gamma+kappa+mu+mu_t)*data_item$t_start)
          num_3 = (kappa+(1-alpha)*mu_t)*(gamma+kappa+alpha*mu_t)*exp(data_item$t_start)
          P_t_start_3 = cste * (num_1 + num_2 + num_3)
          
          #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          # evaluate vector (1, 0, 0, 0) times exponetial of matrix Q (delta_t) times vector (0 0 0 1)
          bloc1 = (1-alpha)*mu_t*(kappa+mu_t) / ((gamma+kappa+mu_t)*(kappa+(1-alpha)*mu_t))
          bloc2 = alpha*kappa*mu_t / ((kappa+(1-alpha)*mu_t)*(gamma+alpha*mu_t))
          bloc3 = gamma*(gamma+kappa+alpha*mu_t) / ((gamma+kappa+mu_t)*(gamma+alpha*mu_t))
          A = 1 - bloc1*exp(-(gamma+kappa+mu+mu_t)*data_item$delta_t) - bloc2*exp(-(gamma+mu+alpha*mu_t)*data_item$delta_t) - bloc3*exp(-mu_t*data_item$delta_t)
          
          # evaluate vector (0, 1, 0, 0) times exponetial of matrix Q (delta_t) times vector (0 0 0 1)
          B = (gamma*(1-exp(-mu*data_item$delta_t)) + alpha*mu_t*(1-exp(-(gamma+mu+alpha*mu_t)*data_item$delta_t))) / (gamma+alpha*mu_t)
          
          # evaluate vector (0, 0, 1, 0) times exponetial of matrix Q (delta_t) times vector (0 0 0 1)
          C = 1 - exp(-mu*data_item$delta_t)
          
          #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          # final calculation
          p = (P_t_start_1 * A + P_t_start_2 * B + P_t_start_3 * C) / (P_t_start_1 + P_t_start_2 + P_t_start_3)
          detach(params)
        }else{
          print('Model not supported')
        }
        return(p)
        
      },
      
      evaluate_pseudo_loglikelihood = function(model, params, smear_status=c('positive')){
        # returns the variable component of the log-likelihood function.
        # we do not need the true log-likelihood for the MCMC algorithm.
        # model is one of {1, 2}
        # params is a list of parameters keyed with the parameter names and valued with the parameter values
        pseudo_ll = 0
        for (j in 1:nrow(self$all_data)){
          if (self$all_data$smear_status[j] %in% smear_status){
            
            # evaluate the probability of death between t_k and t_k+1
            p = self$get_individual_death_proba(self$all_data[j,], model, params)
            
            if (p==0){
              print("Warning: death probability is 0")
            }
            y_k = self$all_data$n_new_deaths[j]
            n_k = self$all_data$n_at_risk[j]
            pseudo_ll = pseudo_ll + y_k * log(p) + (n_k - y_k) * log(1 - p)
          }
        }
        return(pseudo_ll)
      },

      evaluate_log_priors = function(model, params, smear_status=c('positive')){
        log_prior = 0
        return(log_prior)
      },

      proposal_function = function(model,current_param_vals){
        # current_param_vals is a list keyed with the names of the params and valued with their current values
        proposed_param_vals = current_param_vals
        for (par in names(current_param_vals)){
          if (par != 'mu'){
            proposed_param_vals[[par]] = rnorm(1,mean = current_param_vals[[par]],sd = self$proposal_sd[[model]][[par]])
          }
        }
        return(proposed_param_vals)
      },      
      
      store_mcmc_iteration = function(proposed_param_vals,pseudo_ll,accepted, index){
        for (par in names(proposed_param_vals)){
          if (par != 'mu'){
            self$metropolis_records[[par]][index] = proposed_param_vals[[par]]
          }
        }
        self$metropolis_records$pseudo_ll[index] = pseudo_ll
        self$metropolis_records$accepted[index] = accepted
      },
      
      run_metropolis = function(model,n_iterations, n_burned, smear_status=c('positive')){
        # Runs Metropolis algorithm for n_iterations
        self$burned_iterations = n_burned
        if (model == 1){
          self$metropolis_records = data.frame('gamma'=double(n_iterations), 'mu_t'=double(n_iterations),
                                               'pseudo_ll'=double(n_iterations),'accepted'=integer(n_iterations))
        
          current_param_vals = list('gamma'=0.1, 'mu_t'=0.1, 'mu'=self$mu)# initial guess
        }else if (model == 2){
          self$metropolis_records = data.frame('gamma'=double(n_iterations), 'mu_t'=double(n_iterations), 'kappa'=double(n_iterations), 'alpha'=double(n_iterations),
                                               'pseudo_ll'=double(n_iterations),'accepted'=integer(n_iterations))
          
          current_param_vals = list('gamma'=0.1, 'mu_t'=0.1, 'kappa'=1.0, 'alpha'=0.5, 'mu'=self$mu)  # initial guess
        }else{
          print("Model not supported in MCMC for the moment.")
        }
        
        current_pseudo_ll = self$evaluate_pseudo_loglikelihood(model=model, params=current_param_vals, smear_status=smear_status)
        
        self$store_mcmc_iteration(proposed_param_vals=current_param_vals, pseudo_ll=current_pseudo_ll, accepted=1, index=1)
        last_print_j = 0
        for (j in 2:n_iterations){
          if ((j-last_print_j) >= 10){
            str = paste('Completed iteration ', j, sep='')
            print(str)
            last_print_j = j
          }
          
          accepted = 1    #may change later on
          
          # new candidate parameter values
          candidate_param_vals = self$proposal_function(model,current_param_vals)
          
          # test for negative values
          for (par in names(candidate_param_vals)){
            if (candidate_param_vals[[par]] <= 0){
              accepted = 0
              break
            } 
            if (par == 'alpha' && candidate_param_vals[[par]] > 1){
              accepted = 0
              break
            }
          }
          
          # if proposed parameter values are acceptable
          if (accepted == 1){
            candidate_pseudo_ll = self$evaluate_pseudo_loglikelihood(model=model,params=candidate_param_vals, smear_status=smear_status)
            
            # decide acceptance
            if (candidate_pseudo_ll >= current_pseudo_ll){
              accepted = 1
            }else{
              log_accept_proba = candidate_pseudo_ll - current_pseudo_ll
              accepted = rbinom(1,1,prob = exp(log_accept_proba))
            }
          }else{
            candidate_pseudo_ll = NA
          }     
          
          # store information
          self$store_mcmc_iteration(proposed_param_vals=candidate_param_vals, pseudo_ll=candidate_pseudo_ll, accepted=accepted, index=j)
                
          # update variables
          if (accepted == 1){
            current_param_vals = candidate_param_vals
            current_pseudo_ll = candidate_pseudo_ll
          }
        }
      },
           
      plot_ll_surface = function(model, param_ranges, n_per_axis, smear_status=c('positive')){
        if (model==1){
          mu = self$mu
          
          x = seq(param_ranges$gamma[1], param_ranges$gamma[2], length.out = n_per_axis)
          y = seq(param_ranges$mu_t[1], param_ranges$mu_t[2], length.out = n_per_axis)
          z = matrix(rep(NA,n_per_axis*n_per_axis), nrow = n_per_axis, ncol = n_per_axis)
          
          best_params = c(0,0)
          best_z = -1e10
          for (i in 1:n_per_axis){
            for (j in 1:n_per_axis){
              pars=list('gamma'=x[i] , 'mu'=mu, 'mu_t'=y[j])
              ll = self$evaluate_pseudo_loglikelihood(model = 1, params=pars,smear_status = smear_status)
              z[i,j] = ll
              if (ll>best_z){
                best_z = ll
                best_params = c(x[i], y[j])
              }
              
            }
          }
        }
        x11()
        persp(x,y,z)
        
        print(best_params)
        return(z)
        
      },
             
      # ____________________________________
      #     Plotting methods below
            
      plot_cohort_dates = function(){
        n_coh = length(self$cohorts)
        line_height = 1.8
        filename = 'outputs/dates'
        open_figure(filename, 'png', w=14, h=7)
        
        par(mar=c(5.1,12,3,1))
        
        plot(0,0,type='n',xlab='years', ylab='',
             xlim=c(1900,1950), ylim=c(0,(n_coh+1)*line_height), axes = FALSE)
        axis(side=1)
        
        cpt=0
        for (cohort in self$cohorts){
          cpt = cpt+1
          col='black'
          if (cohort$smear_status == 'negative'){
            col='gray'
          }
          lines(cohort$year_range, c(cpt*line_height, cpt*line_height), lwd=4, col=col)
          name = paste(cohort$author, cohort$cohort_name, sep=' ')
          mtext(text = name,side = 2,line = 0,at = cpt*line_height, las=1)
          
          text(x = 1900, y=cpt*line_height,labels = cohort$cohort_size, pos=4)
        }
        text(x=1900, y=cpt*line_height+2, labels='N', pos=4)
        dev.off()
      },
      
      plot_multi_cohort = function(smear_status=c('positive', 'negative'), 
                                   plot_model=FALSE, model=1, mu=NA, mu_t=NA, gamma=NA,kappa=NA, alpha=NA, from_mcmc=FALSE){
        xmax = 0
        cohorts_to_plot = c()
            
        # populate cohort_to_plot and determine xmax
        for (coh in self$cohorts){
          if (coh$smear_status %in% smear_status){
            xmax = max(xmax, max(coh$times))
            cohorts_to_plot = c(cohorts_to_plot, coh)
          }
        }
        
        filename = 'outputs/all_cohorts'
        title = ''
        if (length(smear_status)<2){
          filename = paste(filename, smear_status[1], sep="_")
        }
        if (plot_model){
          title = paste('Model',model,sep=' ')
          filename = paste(filename, "with_model", model, sep="_")
        }
        if (from_mcmc){
          folder_name = paste('outputs/mcmc/Model',model,'/',sep='')
          filename= paste(folder_name, 'best_fit_to_data', sep='')
        }
        open_figure(filename, 'png', w=12, h=9)
        plot(0,0,xlim=c(0,xmax), ylim=c(0,100), xlab='time (years)',
             ylab='death %', main=title)
        count = 0
        for (coh in cohorts_to_plot){
          count = count+1
          color = coh$get_plot_color()
          points(coh$times, coh$perc_death, col=color, pch=18, cex=1.5)
          lines(coh$times, coh$perc_death, col=color, lwd=3)
        }
        if (plot_model){
          t = seq(0,30,by=0.01)
          if (model == 1){
            death = self$death_proportion_model_1(t=t, mu=mu, mu_t=mu_t, gamma=gamma)
          }else if (model == 2){
            death = self$death_proportion_model_2(t=t, mu=mu, mu_t=mu_t, gamma=gamma,kappa=kappa, alpha=alpha)
          }
          lines(t, 100*death, col='black', lwd=4)
        }
        dev.off()
      },
      
  
      #_____________________________________________________________________________________
      #                 Methods below are based on a deterministic verison of the model (ODE-based)
      death_proportion_model_1 = function(t, mu, mu_t, gamma){
        A = (mu+mu_t)/(mu+mu_t+gamma) - mu*gamma/((mu_t+gamma)*(mu+mu_t+gamma))
        B = 1 - exp(-(mu+mu_t+gamma)*t)
        C = mu*gamma/(mu*(mu_t+gamma))
        D = 1 - exp(-mu*t)
        death = A*B + C*D
        return(death)
      },
      death_proportion_model_2 = function(t, mu, mu_t, gamma, kappa, alpha){
        theta = gamma + kappa + mu + mu_t
        w = gamma + mu + alpha*mu_t
        Q = gamma*( (theta-w-kappa)/((theta-mu)*(theta-w)) + kappa/((w-mu)*(theta-w)) )
        
        A = 1-kappa/(theta-w) - gamma*(theta-w-kappa)/((theta-mu)*(theta-w))
        B = kappa/(theta-w) - gamma*kappa/((w-mu)*(theta-w))
        
        death = 1 - (A*exp(-theta*t) + B*exp(-w*t) + Q*exp(-mu*t))
        return(death)
      },
      square_dist_model_to_cohort = function(mu, mu_t, gamma, cohort, model=1, kappa=NA, alpha=NA){
        square_dist = 0
        for (i in 1:length(cohort$times)){
          if (model == 1){
            modelled_death = self$death_proportion_model_1(cohort$times[i], mu, mu_t, gamma)
          }else if(model==2){
            modelled_death = self$death_proportion_model_2(cohort$times[i], mu, mu_t, gamma, kappa, alpha)
          }
          square_dist = square_dist + (modelled_death - cohort$perc_death[i]/100)**2
        }
        return(square_dist)
      },
      square_dist_model_to_all_data = function(mu, mu_t, gamma, kappa=NA, alpha=NA, smear_status=c('positive'), model=1){
        square_dist = 0
        for (cohort in self$cohorts){
          if (cohort$smear_status %in% smear_status){
            square_dist = square_dist + self$square_dist_model_to_cohort(mu, mu_t, gamma, cohort, model=model,
                                                                       kappa=kappa, alpha=alpha)
          }
        }
        return(square_dist)
      },
      optimise_fit = function(mu=self$mu, smear_status=c('positive'), model=1){
        if (model == 1){
          fun_to_minimise = function(params){
            mu_t = params[1]
            gamma = params[2]
            dist = self$square_dist_model_to_all_data(mu,mu_t,gamma,smear_status=smear_status,model = 1)
            return(dist)
          }  
          par_0 = c(0.23, 0.1)  # initial guess for mu_t and gamma
          lower_bounds = c(0, 0)
        }else if(model == 2){
          fun_to_minimise = function(params){
            mu_t = params[1]
            gamma = params[2]
            kappa = params[3]
            alpha = params[4]
            dist = self$square_dist_model_to_all_data(mu, mu_t, gamma, kappa=kappa, alpha=alpha, smear_status=smear_status, model=2)
            return(dist)
          }  
          par_0 = c(0.23, 0.1, 1, 0.5)  # initial guess for mu_t and gamma
          lower_bounds = c(0, 0, 0, 0)
        }
        opt = optim(par = par_0, fn = fun_to_minimise, lower=lower_bounds)
        mu_t = opt$par[1]
        gamma = opt$par[2]
        kappa = NA
        alpha = NA
        if (model == 2){
          kappa = opt$par[3]
          alpha = opt$par[4]
        }
        self$plot_multi_cohort(smear_status=smear_status, plot_model=TRUE, model=model, mu=mu,
                               mu_t=mu_t, gamma=gamma, kappa=kappa, alpha=alpha)
        return(opt)
      }
    )
)


Outputs <- R6Class(
  "Outputs",
  public = list(
    analysis = NULL,
    true_mcmc_outputs = NULL,
    
    initialize = function(analysis){
      self$analysis = analysis 
    },
    
    produce_mcmc_outputs = function(model, smear_status=c('positive')){
      
      # parameter values and log likelihhod over iterations
      folder_name = paste('outputs/mcmc/Model',model,'/',sep='')
                          
      filename= paste(folder_name, 'parameter_progression', sep='')
      open_figure(filename, 'png', w=12, h=9)
      n_params = length(self$analysis$proposal_sd[[model]])
      par(mfrow=c(n_params+1,1))
      colour_list = c('black', 'red')  # black for rejected / red for accepted
      colours = colour_list[self$analysis$metropolis_records$accepted + 1]
      for (par in names(self$analysis$proposal_sd[[model]])){
        plot(self$analysis$metropolis_records[[par]], pch=19,xlab='iteration',ylab=par, col=colours)  
        lines(self$analysis$metropolis_records[[par]])
        if (self$analysis$burned_iterations > 0){
          abline(v=self$analysis$burned_iterations, lty=2)
        }
      }
      plot(self$analysis$metropolis_records$pseudo_ll, pch=19,xlab='iteration',ylab='pseudo log-likelihood', col=colours)  
      lines(self$analysis$metropolis_records$pseudo_ll)
      if (self$analysis$burned_iterations > 0){
        abline(v=self$analysis$burned_iterations, lty=2)
      }
      dev.off()
      
      # compute new table where rejected iterations are replaced with latest accepted ones
      # burned iterations are also removed
      self$true_mcmc_outputs = self$analysis$metropolis_records
      last_accepted_index = 1
      for (j in 2:nrow(self$analysis$metropolis_records)){
        if (self$analysis$metropolis_records$accepted[j] == 1){
          last_accepted_index = j
        }else{
          for (name in colnames(self$true_mcmc_outputs)){
            self$true_mcmc_outputs[[name]][j] = self$analysis$metropolis_records[[name]][last_accepted_index]
          }
        }
      }
       # burn-in
      self$true_mcmc_outputs = self$true_mcmc_outputs[(self$analysis$burned_iterations+1):nrow(self$true_mcmc_outputs),]
      
      
      # posterior distributions (marginal)
      for (par in names(self$analysis$proposal_sd[[model]])){
        filename= paste(folder_name, 'histogram_' ,par, sep='')
        open_figure(filename, 'png', w=12, h=9)
        hist(self$true_mcmc_outputs[[par]],breaks=20,xlab=par,main='')
        dev.off()
      }
      
      # joined posterior distribution
      # -> dotted
      params = names(self$analysis$proposal_sd[[model]])
      for (i in 1:(length(params)-1)){
        for (j in (i+1):length(params)){
          par1 = params[i]
          par2 = params[j]
          filename= paste(folder_name, 'joint_scatter_', par1, '_', par2, sep='')
          open_figure(filename, 'png', w=12, h=9)
          plot(self$true_mcmc_outputs[[par1]], self$true_mcmc_outputs[[par2]], pch=19, xlab=par1, ylab=par2)
          dev.off()
        }
      }
            
      # maximum-likelihood estimates
      j_max = which.max(self$analysis$metropolis_records$pseudo_ll)
      str = paste("Maximul likelihood parameter set found after ", j_max, " iterations:", sep='')
      print(str)
      print(self$analysis$metropolis_records[j_max,]) 
      
      # produce best_likelihood fitted graph
      if (model==1){
        self$analysis$plot_multi_cohort(smear_status=smear_status,plot_model = TRUE, model = model,
                                        gamma =self$analysis$metropolis_records$gamma[j_max], mu_t =self$analysis$metropolis_records$mu_t[j_max],
                                        mu = self$analysis$mu, from_mcmc=TRUE) 
      }else if(model==2){
        self$analysis$plot_multi_cohort(smear_status=smear_status,plot_model = TRUE, model = model,
                                        gamma =self$analysis$metropolis_records$gamma[j_max], mu_t =self$analysis$metropolis_records$mu_t[j_max],
                                        kappa =self$analysis$metropolis_records$kappa[j_max], alpha =self$analysis$metropolis_records$alpha[j_max],
                                        mu = self$analysis$mu, from_mcmc=TRUE)
      }
      
      # ACF graphs
      # parameter values and log likelihhod over iterations
      for (par in names(self$analysis$proposal_sd[[model]])){
        filename= paste(folder_name, 'correlogram_' ,par, sep='')
        open_figure(filename, 'png', w=12, h=9)
        # plot <- ggAcf(x = self$true_mcmc_outputs[[par]], lag.max = 20, type='correlation',title=par)
        # print(plot)
        dev.off()
      }
      
      # print some stats
      str = paste("MCMC acceptance ratio: ", round(100*sum(self$analysis$metropolis_records$accepted)/nrow(self$analysis$metropolis_records)) ,' %', sep='')
      print(str)
    }
  )
)
