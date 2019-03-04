library(R6)
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
      formatted_data = data.frame('t_start'=u, 'delta_t'=u, 'n_at_risk'=u, 'n_new_deaths'=u)
      
      for (i in 1:n_rows){  # i represent the index of the starting time
        formatted_data$t_start[i] = self$times[i]
        formatted_data$delta_t[i] = self$times[i+1] - self$times[i]
        formatted_data$n_at_risk[i] = round((1 - self$perc_death[i]/100) * self$cohort_size)
        formatted_data$n_new_deaths[i] = round((self$perc_death[i+1] - self$perc_death[i])*self$cohort_size/100)
        
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
        "Furth" = 'green',
        "Berg" = 'yellow',
        "Munchbach" = "orange"
      )
      # determine an alpha between 0.5 and 1
      # 0.5 for the oldest times (1905), 1 for the most recent (1940) 
      alpha = (mean(self$year_range) - 1905)/35
      print(alpha)
      add.alpha(color[[self$author]], alpha)      
    }
  )
)

Analysis <- R6Class(
    "Analysis",
    public = list(
      cohorts = c(),
      n_cohorts = 0,
     
      add_cohort = function(author, smear_status, cohort_name, year_range,
                            cohort_size, times, perc_death, perc_alive){
        cohort_id = self$n_cohorts + 1
        cohort = Cohort$new(cohort_id, author, smear_status, cohort_name, year_range,
                            cohort_size, times, perc_death, perc_alive)
        self$cohorts = c(self$cohorts, cohort)
        self$n_cohorts = cohort_id
      },
     
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
                                   plot_model=FALSE, model=1, mu=NA, mu_t=NA, gamma=NA,kappa=NA, alpha=NA){
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
      square_dist_model_to_all_data = function(mu, mu_t, gamma, kappa=NA, alpha=NA, smear_status='positive', model=1){
        square_dist = 0
        for (cohort in self$cohorts){
          if (cohort$smear_status %in% smear_status){
            square_dist = square_dist + self$square_dist_model_to_cohort(mu, mu_t, gamma, cohort, model=model,
                                                                       kappa=kappa, alpha=alpha)
          }
        }
        return(square_dist)
      },
      optimise_fit = function(mu=1/55, smear_status='positive', model=1){
        
        if (model == 1){
          fun_to_minimise = function(params){
            mu_t = params[1]
            gamma = params[2]
            dist = self$square_dist_model_to_all_data(mu,mu_t,gamma,smear_status,model = 1)
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
