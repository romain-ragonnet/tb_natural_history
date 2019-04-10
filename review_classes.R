library(R6)
library(truncnorm)
source('graph_tools.R')

get_param_base <- function(param){
  return(strsplit(param,'#')[[1]][1])
}

Cohort <- R6Class(
  "Cohort",
  public = list(
    id = NULL,
    author = '',
    country = '',
    smear_status = '',
    cohort_name = '',
    year_range = NULL,
    publi_year= NULL,
    cohort_size = 0,
    times = c(),
    perc_death = NULL,
    perc_alive = NULL,
    description = '',
    formatted_data = NULL,
    age_distribution = NULL,
    mu=NULL,
    mu_sd=NULL,
  
    initialize = function(id, author, smear_status, cohort_name, year_range,
                          cohort_size, times, perc_death, perc_alive, age_distribution=NULL){
      self$id = id
      self$author = author
      self$smear_status = smear_status
      self$cohort_name = cohort_name
      self$year_range = year_range
      self$cohort_size = cohort_size
      self$times = times
      self$perc_death = perc_death
      self$perc_alive = perc_alive
      self$age_distribution = age_distribution
      

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
      
      # country
      countries=list(
        "Baart De La Faille" = 'The Netherlands',
        "Buhl" = 'Denmark',
        "Griep" = 'The Netherlands',
        "Tattersall" = 'England',
        "Thompson" = 'England',
        "Hartley" = 'England',
        "Braeuning" = 'Poland',
        "Backer" = 'Norway',
        "Trail" = 'England',
        "Sinding-Larsen" = 'Denmark',
        "Furth" = 'Switzerland',
        "Berg" = 'Sweden',
        "Munchbach" = "Germany",
        "Lindhart" = 'Denmark',
        "Magnusson" = 'Iceland'
      )
      self$country = countries[[self$author]]
      
      # publi_year
      publi_years = list(
        "Baart De La Faille" = 1939,
        "Buhl" = 1967,
        "Griep" = 1939,
        "Tattersall" = 1947,
        "Thompson" = 1943,
        "Hartley" = 1935,
        "Braeuning" = 1936,
        "Backer" = 1937,
        "Trail" = 1931,
        "Sinding-Larsen" = 1937,
        "Furth" = 1930,
        "Berg" = 1939,
        "Munchbach" = "1939 (from Berg et al.)",
        "Lindhart" = 1939,
        "Magnusson" = 1938
      )
      self$publi_year = publi_years[[self$author]]
      
      # Description
      descriptions = list(
        "Baart De La Faille" = 'TB cases hospitalized in the Sanatorium "Berg en Bosch".',
        "Buhl" = 'Danish TB patients diagnosed between 1925 and 1954.',
        "Griep" = 'All notified cases of "open" pulmonary TB occurring in The Hague between 1920 and 1937.',
        "Tattersall" = 'Sputum-positive cases attending Reading (UK) dispensary between 1914 and 1940.',
        "Thompson" = 'All sputum-positive TB patients occurring in a compact industrial area in Middlesex County.',
        "Hartley" = 'Retrospective cohort study of cases treated for TB at Brompton Hospital.',
        "Braeuning" = 'TB dispensary patients from Szczecin, Poland (then known as Stettin, Germany).',
        "Backer" = 'Patients notified to the Board of Health in Oslo.',
        "Trail" = 'Cohort study among patients of the King Edward VII sanatorium in Midhurst.',
        "Sinding-Larsen" = 'Cohort study in Denmark among sanatorium patients.',
        "Furth" = 'Pulmonary TB patients from Barmelweid sanatorium.',
        "Berg" = 'All patients with "open" TB from Gothenburg diagnosed between 1928 and 1934.',
        "Munchbach" = 'Sanatorium patients with "open" bacillary TB.',
        "Lindhart" = 'Mortality of notified TB cases in Denmark between 1925 and 1934.',
        "Magnusson" = 'Cases admitted for treatment at the Vifillsstadir Sanatorium in Reykjavik.'
      )
      self$description = descriptions[[self$author]]
      
      # add gender details to descriptions
      if (grepl('men',self$cohort_name)){
        if (grepl('women',self$cohort_name)){
          str = paste(self$description, ' Female patients only.', sep='')
        }else{
          str = paste(self$description, ' Male patients only.', sep='')
        }
        self$description = str
      }
      
      
    },
    
    format_data = function(){
      n_rows = length(self$times) - 1
      u = rep(NA,n_rows)
      formatted_data = data.frame('t_start'=u, 'delta_t'=u, 'n_at_risk'=u, 'n_new_deaths'=u, 'smear_status'=u, 'cohort_id'=u, 'mu'=u)
      
      for (i in 1:n_rows){  # i represent the index of the starting time
        formatted_data$t_start[i] = self$times[i]
        formatted_data$delta_t[i] = self$times[i+1] - self$times[i]
        formatted_data$n_at_risk[i] = round((1 - self$perc_death[i]/100) * self$cohort_size)
        formatted_data$n_new_deaths[i] = round((self$perc_death[i+1] - self$perc_death[i])*self$cohort_size/100)
        formatted_data$smear_status[i] = self$smear_status
        formatted_data$cohort_id[i] = self$id
        formatted_data$mu[i] = self$mu
        formatted_data$mu_sd[i] = self$mu_sd
        
        
        if (formatted_data$n_at_risk[i] < formatted_data$n_new_deaths[i]){
          print("WARNING: more dead persons than at_risk persons")
        }
      }
      self$formatted_data = formatted_data
      
    },
    
    work_out_mortality = function(mortality_data){
      if (is.null(self$age_distribution)){
        self$mu = 0.01 # hard-code
        self$mu_sd = 0.002 #
      }else{
        self$mu_sd = 0.001
        recruitment_year = floor(mean(self$year_range))
        # find appropriate mortality table
        if (grepl('men',self$cohort_name)){
          if (grepl('women',self$cohort_name)){
            mortality_table = mortality_data$female
          }else{
            mortality_table = mortality_data$male
          }
        }else{  # all genders
          mortality_table = mortality_data$all
        }
        colname = paste('mu_',recruitment_year,sep='') 
        rates_by_age = mortality_table[[colname]]
        
        # calculate average rate
        pop = 0 # track the total population
        cumul_rates = 0 
        for (cat in self$age_distribution){
          pop = pop + cat[3]
          year_max = cat[2]
          if (year_max == 100){
            year_max = cat[1] + 10
          }
          for (age in cat[1]:year_max){
            cumul_rates = cumul_rates + rates_by_age[age+1] * cat[3]/(1+year_max-cat[1])
          }
        }
        self$mu = cumul_rates/pop
      }  
    },
    
    plot_cohort_data = function(){
      plot(self$times, self$perc_death, main='',
           xlab='time (years)', ylab='cumulative death (%)', xlim=c(0,30), ylim=c(0,100),
           pch=18, cex=1.5
           )
    }
  )
)

Inputs <- R6Class(
  "Inputs",
  public = list(
    cohorts = c(),
    mortality_data = list(),
    n_age_available = 0,
    n_cohorts = 0,
    
    initialize = function(){
      self$read_mortality_data()
    },
    
    read_mortality_data = function(){
      for (sex in c('male', 'female')){
        filename = paste('data/', sex, '_mortality.csv', sep='')
        str = paste("Reading ", filename,sep='')
        print(str)
        data = read.csv(filename,header=TRUE,colClasses = rep('numeric',37))
        colnames(data)[1]='age'
        self$mortality_data[[sex]] = data
      }
      self$mortality_data$all = 0.5*(self$mortality_data$male+self$mortality_data$female)
    },
    
    add_cohort = function(author, smear_status, cohort_name, year_range,
                          cohort_size, times, perc_death, perc_alive, age_distribution=NULL){
      cohort_id = self$n_cohorts + 1
      cohort = Cohort$new(cohort_id, author, smear_status, cohort_name, year_range,
                          cohort_size, times, perc_death, perc_alive, age_distribution)
      
      
      if(is.null(cohort_size)){
        return()
      }
      
      # work out mortality rate
      cohort$work_out_mortality(self$mortality_data)
      
      # format the data
      if (!is.null(cohort$cohort_size)){
        cohort$format_data()
      }
      
      if (!is.null(cohort$age_distribution)){
        self$n_age_available = self$n_age_available+1
      }
      
      self$n_cohorts = self$n_cohorts + 1
      self$cohorts = c(self$cohorts, cohort)
      str=paste("Cohort ", cohort_id, ' / size: ', cohort_size, ' / author: ', author, sep='')
      # print(str)
    },
    plot_multi_cohort = function(){
      xmax = 0
      cohorts_to_plot = c()
      colours_by_smear = list('positive'='black', 'negative'='gray50')
      windowsFonts(A = windowsFont("Times New Roman"))
      
      # populate cohort_to_plot and determine xmax
      for (coh in self$cohorts){
        xmax = max(xmax, max(coh$times))
        cohorts_to_plot = c(cohorts_to_plot, coh)
      }
      
      filename = 'outputs/all_cohorts'
      title = ''
      open_figure(filename, 'png', w=12, h=9)
      plot(0,0,xlim=c(0,xmax), ylim=c(0,100), xlab='time after recruitment (years)',
           ylab='cumulative death %', main=title,bty='n',cex.lab=1.3)
      count = 0
      count_sp = 0
      count_sn = 0
      for (coh in cohorts_to_plot){
        count = count+1
        color = colours_by_smear[[coh$smear_status]]
        points(coh$times, coh$perc_death, col=color, pch=20, cex=1)
        lines(coh$times, coh$perc_death, col=color, lwd=2)
        if (coh$smear_status == 'positive'){
          count_sp = count_sp + 1
        }else if (coh$smear_status == 'negative'){
          count_sn = count_sn + 1
        }
      }
      
      txt1 = paste('smear-positive (n=',count_sp,')',sep='')
      txt2 = paste('smear-negative (n=',count_sn,')',sep='')
      
      legend(x = 20, y=50,legend = c(txt1, txt2),lwd=c(3,3),cex=1.3,col = c(colours_by_smear$positive,colours_by_smear$negative),bty = 'n')
      
      dev.off()
    },
    
    plot_cohort_dates = function(){
      n_coh = length(self$cohorts)
      line_height = 1.8
      filename = 'outputs/dates'
      open_figure(filename, 'png', w=14, h=12)
      
      par(mar=c(5.1,12,3,1))
      
      plot(0,0,type='n',xlab='years', ylab='',
           xlim=c(1900,1950), ylim=c(0,(n_coh+1)*line_height), axes = FALSE)
      axis(side=1)
      
      cpt=0
      sizes = c()
      for (cohort in self$cohorts){
        
        sizes = c(sizes, cohort$cohort_size)
        
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
      return(sizes)
    },
    
    generate_cohort_profile = function(cohort, fitted_params,likelihoods){
      
      filename = paste('outputs/cohort_profile_',cohort$id,sep='')
      open_figure(filename, 'png', w=8.27, h=11.69)
      str_years_diagnosis = paste(cohort$year_range[1],'-',cohort$year_range[2], sep='')
      
      text_fields = list(
        'Author'= cohort$author,
        'Publication year'= cohort$publi_year,
        'Patient recruitment years'=str_years_diagnosis,
        'Cohort size'=cohort$cohort_size,
        'Location'=cohort$country,
        'Type of TB (smear-status)'=cohort$smear_status,
        'Details'=cohort$description
      )
      title=paste('Cohort ', cohort$id, sep='')
      
      layout(matrix(c(1,1,2,3,4,4,5,6),ncol = 2,byrow = TRUE),heights = c(0.2,0.4,1,1))
      par(mar=c(1, 1, 1, 1), mgp=c(2, 1, 0))
      
      # Cohort id
      plot(0,0,type='n',axes=FALSE,main='',xlim=c(0,10),ylim=c(0,10))
      text(5,5, title,cex = 2.5)
      
      # cohort details
      count = 0
      plot(0,0,type='n',axes=FALSE,main='',xlim=c(0,10),ylim=c(0,10))
      for (field in names(text_fields)[1:4]){
        count=count+1
        str = paste(field, ": ", text_fields[[field]], sep='')
        text(0,10-2*count,str, cex=1.5, adj=c(0,1))
      }
      
      # cohort details continued
      count = 0
      plot(0,0,type='n',axes=FALSE,main='',xlim=c(0,10),ylim=c(0,10))
      for (field in names(text_fields)[5:length(names(text_fields))]){
        count=count+1
        str = paste(field, ": ", text_fields[[field]], sep='')
        if (field == 'Details'){
          str = strBreakInLines(str,breakAt = 30)
        }
        text(0,10-2*count,str, cex=1.5, adj=c(0,1))
      }
      
      # data
      par(mar=c(3.5,6,1,6))
      cohort$plot_cohort_data()
      
      par(mar=c(4,4,4,1))
      param = paste('mu_t#',cohort$id,sep='')
      hist(c(0))
      # hist(fitted_params[[param]],breaks = 50,main='',xlab='mu_t')
      
      param = paste('gamma#',cohort$id,sep='')
      hist(c(0))
      # hist(fitted_params[[param]],breaks = 50,main='',xlab='gamma')
      
      dev.off()
      
    },
    
    generate_age_distributions = function(){
      filename = 'outputs/age_distributions'
      open_figure(filename, 'png', w=8.27, h=11.69)
      bot=0.8
      lef=0.5
      top=0.1
      rig=0.
      par(mfrow=c(5,4),
          mai=c(bot,lef,top,rig))
      
      count = 0
      for (coh in self$analysis$cohorts){
        if (!is.null(coh$age_distribution)){
          count = count + 1
          x_lab='age (years)'
          if (count>16){ # last row
            par(mai=c(bot,lef,top,rig))
            x_lab='age (years)'
          }
          ymax=0
          for (cat in coh$age_distribution){
            if (cat[3]>ymax){
              ymax = cat[3]
            }
          }
          
          # transform age distributions into list of 'representative' ages so we can plot a histogram
          ages = c()
          breaks_ = c()
          for (cat in coh$age_distribution){
            h=cat[3]
            low=cat[1]
            high=1+cat[2]
            ages = c(ages,rep(low+1,h))
            breaks_ = c(breaks_, low)
          }
          breaks_ = c(breaks_, 80)
          
          title = paste('Cohort ', coh$id, sep='')
          hist(ages,breaks=breaks_,xlab =x_lab , ylab = 'density', main=title, xlim=c(0,80) )
        }
      }
      
      dev.off()
    }
  )
)
    
Analysis <- R6Class(
    "Analysis",
    public = list(
      inputs = NULL,
      all_data = NULL,
      param_bases = c(), #  e.g. c('gamma', 'mu_t') for Model 1
      stan_fit = NULL,
      cohort_ids = c(),  # only those considered in the model fitting
      smear_status = NULL,
      random_effects = FALSE,
      estimate_mu = FALSE,
      analysis_name = '',
      base_path = '',
      
      initialize = function(smear_status,random_effects,estimate_mu, analysis_name){
        self$inputs = inputs
        self$smear_status = smear_status
        self$random_effects = random_effects
        self$estimate_mu = estimate_mu
        self$analysis_name = analysis_name
        self$produce_main_dataframe()
        
        # create directories
        effect_string = 'outputs/stan/fixed_effect'
        if (self$random_effects){effect_string = 'outputs/stan/random_effect'}
        smear_string = paste(effect_string,'/',self$smear_status[1], sep='')
        with_analysis_name = paste(smear_string,'/',self$analysis_name, sep='')
        paths = c('outputs', 'outputs/stan',effect_string, smear_string, with_analysis_name)
        self$base_path = paste(with_analysis_name,'/',sep='') 
        for (path in paths){
          if (!file.exists(path)){
            dir.create(path)
          }
        }
      },
      
      produce_main_dataframe = function(){
        self$all_data = data.frame('t_start'=double(), 'delta_t'=double(), 'n_at_risk'=integer(), 'n_new_deaths'=integer(),'smear_status'=character(), 'cohort_id'=integer(), 'mu'=numeric(),'mu_sd'=numeric())
        
        for (c in self$inputs$cohorts){
          if (c$smear_status %in% self$smear_status){
            self$all_data = rbind(self$all_data, c$formatted_data)
            self$cohort_ids = c(self$cohort_ids, c$id)
          }
        }
        
      },

      run_mcmc_stan =function(model,n_chains, n_iterations, n_burned){
        print("Loading rstan...")
        library(rstan)
        print("... done")
      
        self$param_bases = c('gamma', 'mu_t')

        dat    <- self$all_data
        dat$id <- as.integer(factor(dat$cohort_id))  # create new cohort indices

        if (n_chains>1){
          options(mc.cores = parallel::detectCores())
        }else{
          options(mc.cores = 1)
        }
        rstan_options(auto_write = TRUE)

        if (estimate_mu){
          # get mu and sd by cohort
          mu_by_cohort = c()
          mu_sd_by_cohort = c()
          coh_id=-1
          for (j in 1:nrow(dat)){
            if (dat$cohort_id[j] > coh_id){
              mu_by_cohort = c(mu_by_cohort, dat$mu[j])
              mu_sd_by_cohort = c(mu_sd_by_cohort, dat$mu_sd[j])
              coh_id = dat$cohort_id[j]
            }
          }
          
          standat <- list(
            N            = nrow(dat),
            K            = max(dat$id),
            n_at_risk    = dat$n_at_risk,
            n_new_deaths = dat$n_new_deaths,
            t_start      = dat$t_start,
            delta_t      = dat$delta_t,
            cohort_id    = dat$id,
            mu_by_cohort = mu_by_cohort,
            mu_sd_by_cohort = mu_sd_by_cohort
          )
        }else{
          standat <- list(
            N            = nrow(dat),
            K            = max(dat$id),
            n_at_risk    = dat$n_at_risk,
            n_new_deaths = dat$n_new_deaths,
            t_start      = dat$t_start,
            delta_t      = dat$delta_t,
            cohort_id    = dat$id,
            mu           = dat$mu
          )
        }
        
        if (!random_effects && !estimate_mu){
          stan_file = "fixed_effect_model.stan" 
          tracked_pars = c("mu_t","gamma") 
        }else if(!random_effects && estimate_mu){
          stan_file = "fixed_effect_model_fitted_mu.stan"
          tracked_pars = c("mu_t","gamma","e_mu")
        }else if(random_effects && estimate_mu){
          stan_file = "random_effect_model_fitted_mu.stan"
          tracked_pars = c("lambda_mu_t","sigma_mu_t","lambda_gamma","sigma_gamma")
        }else{ # random effect with fixed mu
          stan_file = "random_effect_model.stan"
          tracked_pars = c("mu_t","gamma","lambda_mu_t","sigma_mu_t","lambda_gamma","sigma_gamma")
        }
        
        # fit the model
        print("Start Stan fit")
        self$stan_fit <- stan(file = stan_file,pars = tracked_pars,
                     data = standat, chains=n_chains,
                     iter = n_iterations, warmup = n_burned, thin = 1, 
                     init = "random")

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

    produce_mcmc_random_effect_graphs_from_stan = function(fixed_estimates=NA){
      # fixed_estimates is a list of type list(mu_T=c(lower,median,upper), ...)
      fit = self$analysis$stan_fit
      outputs = as.data.frame(fit)
      
      if(is.na(fixed_estimates)){
        if (self$analysis$smear_status[1] == 'positive'){
          fixed_estimates=list('mu_t'=c(0.301, 0.307, 0.313), 
                               'gamma'=c(0.133, 0.139, 0.145))
        }else{
          fixed_estimates=list('mu_t'=c(0.018, 0.024, 0.031), 
                               'gamma'=c(0.090, 0.140, 0.199))
        }
      }
      
      base_path = self$analysis$base_path 
      # figure height
      height_plot = length(self$analysis$cohort_ids) + 2 
      
      filename= paste(base_path, 'params_by_cohort',sep='')
      open_figure(filename, 'pdf', w=25, h=height_plot + 2)
      
      layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), 
             widths=c(1,2,2), heights=c(1,height_plot))
      
      par(mar=c(0.2, 4.1, 0.2, 2.1))

      # Plot graph titlte
      plot(0,0,type='n',bty='n',axes=FALSE,xlab='',ylab='')
      title = paste("Smear-",self$analysis$smear_status[1]," TB",sep='')
      text(0,0,title, cex=6)
      
      abline_h = 1.5
      
      # Write cohort indices
      par(mar=c(7.1, 4.1, 0.2, 2.1))
      cex=5
      x_write = 5
      plot(0,0,type='n',bty='n',axes=FALSE,xlim=c(0,10),ylim=c(0,height_plot), xlab='',ylab='')
      h = 1
      for (i in self$analysis$cohort_ids){
        h = h+1
        text(x = x_write,y=h,labels = i, cex=cex*0.75,adj=0.5)
      }
      text(x=x_write,y=h+1,labels='Cohort #', cex=cex,adj=0.5)
      text(x=x_write,y=1,labels='Average*', cex=cex,adj=0.5)
      text(x=x_write,y=0,labels='Fixed-effect', cex=cex,adj=0.5)
      abline(h = abline_h)
      
      # Plot cohort-specific results for each parameter
      if (self$analysis$smear_status[1] == 'positive'){
        xmax = list('gamma'=1,'mu_t'=1.0)
      }else{
        xmax = list('gamma'=1,'mu_t'=0.15)
      }
      par_names = list('gamma'='Self-recovery rate','mu_t'='TB mortality rate')
      lwd = 4
      cex=4
      par(mgp=c(5, 3, 0))
      for (par_base in self$analysis$param_bases){
        plot(0,0,type='n',main='',bty='n',axes=FALSE, cex.lab = 4,
             xlim=c(0,xmax[[par_base]]),ylim=c(0,height_plot),xlab='/year',ylab='')
        axis(side=1, lwd = 3, lwd.ticks = 3, cex.axis=4)
        h = 1
        for (i in self$analysis$cohort_ids){
          h = h+1
          par = paste(par_base,"[",h-1,"]",sep='')
          x = outputs[[par]]
          qt = quantile(x,c(0.025,0.5,0.975),names = FALSE)
          lines(x = c(qt[1], qt[3]), y=c(h,h), lwd=lwd)
          points(x=qt[2], y=h, cex=cex, pch=18)
        }
        # plot mean estimates based on random-effect model
        hyper_par_mean = paste('lambda_',par_base,sep='')
        hyper_par_sd = paste('sigma_',par_base,sep='')
        
        #ln_means = exp(outputs[[hyper_par_mean]] + 0.5*(outputs[[hyper_par_sd]])**2)
        ln_means = outputs[[hyper_par_mean]]
        qt = quantile(ln_means,c(0.025,0.5,0.975),names = FALSE)
        lines(x = c(qt[1], qt[3]), y=c(1,1), col='red', lwd=lwd)
        points(x=qt[2], y=1, col='red', cex=cex, pch=18)
        
        # plot estimates based on fixed-effect model
        qt = fixed_estimates[[par_base]] 
        lines(x = c(qt[1], qt[3]), y=c(0,0), col='blue', lwd=lwd)
        points(x=qt[2], y=0, col='blue', cex=cex, pch=18)
        
        abline(h=abline_h)
        
        text(x=xmax[[par_base]]/2,y=h+1,labels=par_names[[par_base]], cex=5,adj=0.5)
      }
      dev.off()
      
    },
    
    produce_stan_outputs = function(){
      fit = self$analysis$stan_fit
      outputs = as.data.frame(fit)
      
      # work-out output file
      if (self$analysis$random_effects){
        str_effect = 'random_effect'
      }else{
        str_effect = 'fixed_effect'
      }

      base_path = self$analysis$base_path
      
      # summary stats
      print(summary(fit)$summary)
      
      # plots of posterior density
      filename = paste(base_path,'posterior_density',sep='')
      open_figure(filename, 'png')
        p=plot(fit, plotfun = "dens")
        print(p)
      dev.off()
      
      # plots of trace with warmup
      filename = paste(base_path,'trace_with_warmup',sep='')
      open_figure(filename, 'png')
        p =plot(fit, plotfun = "trace", inc_warmup = TRUE)
        print(p)
      dev.off()
      
      # plots of trace without warmup
      filename = paste(base_path,'trace_without_warmup',sep='')
      open_figure(filename, 'png')
        p=plot(fit, plotfun = "trace", inc_warmup = FALSE)
        print(p)
      dev.off()
      
      # save workspace
      filename = paste(base_path,'workspace.RData',sep='')
      save.image(filename)
    }
  )
)

source('load_data.R')
