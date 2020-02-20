library(R6)
library(truncnorm)

setwd('C:/Users/rrag0004/Models/tb_natural_history/')
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
    sex = 'not_given',
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
      if (grepl('men',self$cohort_name,ignore.case = TRUE)){
        if (grepl('women',self$cohort_name,ignore.case = TRUE)){
          str = paste(self$description, ' Female patients only.', sep='')
          self$sex = 'women'
        }else{
          str = paste(self$description, ' Male patients only.', sep='')
          self$sex = 'men'
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
        formatted_data$start_year[i] = self$year_range[1]
        formatted_data$end_year[i] = self$year_range[2]
        
        
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
        if (grepl('men',self$cohort_name,ignore.case = TRUE)){
          if (grepl('women',self$cohort_name, ignore.case = TRUE)){
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
    
    plot_cohort_data = function(fitted_params=NA,n_spaghettis=100){
      plot(self$times, self$perc_death, main='',
           xlab='time (years)', ylab='cumulative death (%)', xlim=c(0,30), ylim=c(0,100),
           pch=18, cex=1.5
           )
      if (!is.na(fitted_params)){
        t = seq(0,30,by=0.1)
        n_plotted = min(n_spaghettis, length(fitted_params$mu_t))
        for (i in 1:n_plotted){
          gamma=fitted_params$gamma[i]
          mu_t=fitted_params$mu_t[i]
          mu = fitted_params$mu[i]
          y=1 - (gamma / (gamma + mu_t)) * exp(-mu * t) -
            (mu_t / (gamma + mu_t)) * exp(-(gamma + mu + mu_t) * t)
          lines(t,100*y,col=my_blue, lwd=1)
        }
        points(self$times, self$perc_death,pch=18, cex=2.5, col=my_red)
      }
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
    tb_mortality_data = list('Sweden'=list('values' = 0.001*c(1.92,1.78,1.77,1.93,1.89,1.79,1.73,1.69,1.58,1.56,
                                                 1.53,1.57,1.49,1.57,1.63,1.65,1.57,1.46,1.4,1.29,1.23,
                                                 1.2,1.12,1.19,1.16,1.1,1.11,1.04,1.04,1.01,1.05,0.94,
                                                 0.84,0.84,0.79),
                                    'years' = seq(1901,1935,by=1)),
                             'Denmark'=list('values' = 0.001*c(0.71,0.63,0.63,0.57,0.61,0.57,0.54,0.54,0.48,0.44),
                                            'years'=seq(1925,1934,by=1))
                             ),
    tb_mortality_data_as_df = NA,
    
    initialize = function(){
      self$read_mortality_data()
      self$process_tb_mortality_data()
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
    
    process_tb_mortality_data = function(){
      n_rows = length(self$tb_mortality_data$Sweden$years) + length(self$tb_mortality_data$Denmark$years)
      self$tb_mortality_data_as_df = data.frame('year'=rep(0,n_rows),'mortality'=rep(0.,n_rows))
      i_row=0
      for (country in c('Sweden', 'Denmark')){
        for (i in 1:length(self$tb_mortality_data[[country]]$years)){
            i_row = i_row + 1
            self$tb_mortality_data_as_df$year[i_row] = self$tb_mortality_data[[country]]$years[i]
            self$tb_mortality_data_as_df$mortality[i_row] = self$tb_mortality_data[[country]]$values[i]
        }
      }
      self$tb_mortality_data_as_df = self$tb_mortality_data_as_df[order(self$tb_mortality_data_as_df$year),]
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
      colours_by_smear = list('positive'=my_blue, 'negative'=my_orange)
      windowsFonts(A = windowsFont("Times New Roman"))
      
      # populate cohort_to_plot and determine xmax
      for (coh in self$cohorts){
        xmax = max(xmax, max(coh$times))
        cohorts_to_plot = c(cohorts_to_plot, coh)
      }
      
      filename = 'outputs/all_cohorts_test'
      title = ''
      open_figure(filename, 'tiff', w=12, h=9)
      plot(0,0,xlim=c(0,xmax), ylim=c(0,100), xlab='time after recruitment (years)',
           ylab='cumulative death %', main=title,bty='n',cex.lab=1.3)
      count = 0
      count_sp = 0
      count_sn = 0
      for (coh in cohorts_to_plot){
        count = count+1
        color = colours_by_smear[[coh$smear_status]]
        points(coh$times, coh$perc_death, col=color, pch=20, cex=1.2)
        lines(coh$times, coh$perc_death, col=color, lwd=1, lty=1.2)
        if (coh$smear_status == 'positive'){
          count_sp = count_sp + 1
        }else if (coh$smear_status == 'negative'){
          count_sn = count_sn + 1
        }
      }
      
      txt1 = paste('smear-positive (n=',count_sp,')',sep='')
      txt2 = paste('smear-negative (n=',count_sn,')',sep='')
      
      legend(x = 20, y=50,legend = c(txt1, txt2),lwd=c(2,2),lty=c(1,1),pch=c(20,20),cex=1.2,col = c(colours_by_smear$positive,colours_by_smear$negative),bty = 'n')
      
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
      for (cohort in rev(self$cohorts)){
        
        sizes = c(sizes, cohort$cohort_size)
        
        cpt = cpt+1
        col='black'
        if (cohort$smear_status == 'negative'){
          col='gray'
        }
        lines(cohort$year_range, c(cpt*line_height, cpt*line_height), lwd=4, col=col)
        #name = paste(cohort$author, cohort$cohort_name, sep=' ')
        name = cohort$id 
        mtext(text = name,side = 2,line = 0,at = cpt*line_height, las=1)
        
        text(x = 1900, y=cpt*line_height,labels = cohort$cohort_size, pos=4)
      }
      text(x=1900, y=cpt*line_height+2, labels='Cohort size', pos=4)
      
      mtext(at=cpt*line_height+2, text='Cohort #', las=1, line=0, side=2)
      
      dev.off()
      return(sizes)
    },
    
    generate_cohort_profile = function(cohort, fitted_params){
      smear_str = 'sp_'
      if (cohort$smear_status == 'negative'){
        smear_str = 'sn_'
      }
      filename = paste('outputs/cohort_profiles/',smear_str,'cohort_',cohort$id,sep='')
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
      cohort$plot_cohort_data(fitted_params)
      
      
      xmax_mu_t = list('positive'=1, 'negative'=0.1)
      xmax_gamma = list('positive'=1, 'negative'=0.5)
      h_color = my_purple
      
      # fitted mu_t
      par(mar=c(4,4,4,1))
      hist(fitted_params$mu_t,breaks = 50,main='',xlab='mu_t',xlim=c(0,xmax_mu_t[[cohort$smear_status]]),col = h_color,border=h_color)
      
      # fitted gamma
      hist(fitted_params$gamma,breaks = 50,main='',xlab='gamma',xlim=c(0,xmax_gamma[[cohort$smear_status]]),col = h_color,border=h_color)
      
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
      for (coh in self$cohorts){
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
    },
    
    get_average_diagnosis_year = function(){
      years = c()
      for (coh in self$cohorts){
        mean_y = mean(coh$year_range)
        years= c(years, mean_y)
      }
      return(years)
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
      restrict_to = NA,
      priors = 'normal',
      base_path = '',
      tracked_pars=c(),
      substract_tb_mortality = TRUE,
      
      initialize = function(smear_status,random_effects,estimate_mu, analysis_name, restrict_to=NA, priors='normal', substract_tb_mortality=TRUE){
        self$inputs = inputs
        self$smear_status = smear_status
        self$random_effects = random_effects
        self$estimate_mu = estimate_mu
        self$analysis_name = analysis_name
        self$produce_main_dataframe()
        self$restrict_to = restrict_to
        self$priors = priors
        self$substract_tb_mortality = substract_tb_mortality
        
        # create directories
        effect_string = 'outputs/stan/fixed_effect'
        if (self$random_effects){effect_string = 'outputs/stan/random_effect'}
        mu_string = paste(effect_string,'/fixed_mu',sep='')
        if (self$estimate_mu){mu_string = paste(effect_string,'/estimated_mu',sep='')}
        smear_string = paste(mu_string,'/',self$smear_status[1], sep='')
        with_analysis_name = paste(smear_string,'/',self$analysis_name, sep='')
        paths = c('outputs', 'outputs/stan',effect_string, mu_string, smear_string, with_analysis_name, 'outputs/cohort_profiles')
        self$base_path = paste(with_analysis_name,'/',sep='') 
        for (path in paths){
          if (!file.exists(path)){
            dir.create(path)
          }
        }
      },
      
      produce_main_dataframe = function(){
        self$all_data = data.frame('t_start'=double(), 'delta_t'=double(), 'n_at_risk'=integer(), 'n_new_deaths'=integer(),'smear_status'=character(), 
                                   'cohort_id'=integer(), 'mu'=numeric(),'mu_sd'=numeric(), 'start_year'=numeric(), 'end_year'=numeric())
        
        for (c in self$inputs$cohorts){
          if (c$smear_status %in% self$smear_status){
            if (!is.na(restrict_to)){
              if (restrict_to == 'men'){
                if(c$sex == 'men'){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                  print(c$id)
                }              
              }
              if (restrict_to == 'women'){
                if(c$sex == 'women'){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }              
              }
              if (restrict_to == 'no_gender'){
                if(c$sex == 'not_given'){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }              
              }
              if (restrict_to == 'before_1924'){
                if(mean(c$year_range) < 1924){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }
              }  
              if (restrict_to == 'after_1924'){
                if(mean(c$year_range) > 1924){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }
              }
              if (restrict_to == 'sanatorium'){
                if(grepl('anatori',c$description)){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }
              }
              if (restrict_to == 'non-sanatorium'){
                if(!grepl('anatori',c$description)){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }
              }
              if (restrict_to == 'hospital'){
                if(grepl('ospital',c$description) | grepl('anatori',c$description) | grepl('ispensar',c$description)){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }
              }
              if (restrict_to == 'non-hospital'){
                if(!grepl('ospital',c$description) & !grepl('anatori',c$description) & !grepl('ispensar',c$description)){
                  self$all_data = rbind(self$all_data, c$formatted_data)
                  self$cohort_ids = c(self$cohort_ids, c$id)
                }
              }
                
            }else{
              self$all_data = rbind(self$all_data, c$formatted_data)
              self$cohort_ids = c(self$cohort_ids, c$id)
            }

          }
        }
        
      },

      fit_tb_mortality_exponential = function(n_chains, n_iterations, n_burned, thinning_period){
        library(rstan)
        if (n_chains>1){
          options(mc.cores = parallel::detectCores())
        }else{
          options(mc.cores = 1)
        }
        rstan_options(auto_write = TRUE)
        
        standat = {}
        standat$nrow_tb_mortality_data = nrow(self$inputs$tb_mortality_data_as_df)
        standat$tb_mortality_data_years = array(self$inputs$tb_mortality_data_as_df$year, dim=c(length(self$inputs$tb_mortality_data_as_df$year),1))
        standat$tb_mortality_data_values = self$inputs$tb_mortality_data_as_df$mortality

        self$tracked_pars = c(self$tracked_pars, 'a', 'b')
        
        self$stan_fit <- stan(file = 'tb_mortality_exponential_fit.stan',pars = self$tracked_pars,
                              data = standat, chains=n_chains,
                              iter = n_iterations, warmup = n_burned, thin = thinning_period, 
                              init = "random")
      },
      
      run_mcmc_stan =function(model,n_chains, n_iterations, n_burned, thinning_period){
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

        if (self$estimate_mu){
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
        
        if (!self$random_effects && !self$estimate_mu){
          stan_file = "fixed_effect_model.stan" 
          self$tracked_pars = c("mu_t","gamma") 
        }else if(!self$random_effects && self$estimate_mu){
          stan_file = "fixed_effect_model_fitted_mu.stan"
          self$tracked_pars = c("mu_t","gamma","e_mu")
        }else if(self$random_effects && self$estimate_mu){
          stan_file = "random_effect_model_fitted_mu.stan"
          if (self$substract_tb_mortality){
            stan_file = "random_effect_model_fitted_mu_substracted_tb_mortality.stan"
          }
          if (self$priors == 'gamma'){
            stan_file = "random_effect_model_fitted_mu_gamma_priors.stan"
            if (self$substract_tb_mortality){
              stan_file = "random_effect_model_fitted_mu_gamma_priors_substracted_tb_mortality.stan"
            }
          }
          self$tracked_pars = c("lambda_mu_t","sigma_mu_t","lambda_gamma","sigma_gamma","e_mu","mu_t","gamma")
        }else{ # random effect with fixed mu
          stan_file = "random_effect_model.stan"
          self$tracked_pars = c("mu_t","gamma","lambda_mu_t","sigma_mu_t","lambda_gamma","sigma_gamma")
        }
        
        
        # if we substract population tb mortality rate from mu, we load more data and track more parameters:
        if (self$substract_tb_mortality){
          standat$nrow_tb_mortality_data = nrow(self$inputs$tb_mortality_data_as_df)
          standat$tb_mortality_data_years = array(self$inputs$tb_mortality_data_as_df$year, dim=c(length(self$inputs$tb_mortality_data_as_df$year),1))
          standat$tb_mortality_data_values = self$inputs$tb_mortality_data_as_df$mortality
          standat$start_year = dat$start_year
          standat$end_year = dat$end_year
          
          self$tracked_pars = c(self$tracked_pars, 'a', 'b')
        }
        
        # fit the model
        print("Start Stan fit using code from:")
        print(stan_file)
        self$stan_fit <- stan(file = stan_file,pars = self$tracked_pars,
                     data = standat, chains=n_chains,
                     iter = n_iterations, warmup = n_burned, thin = thinning_period, 
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

    produce_mcmc_random_effect_graphs_from_stan = function(generate_cohort_profiles=FALSE){
      # fixed_estimates is a list of type list(mu_T=c(lower,median,upper), ...)
      fit = self$analysis$stan_fit
      outputs = as.data.frame(fit)
      
      base_path = self$analysis$base_path 
      # figure height
      height_plot = length(self$analysis$cohort_ids) + 2 
      
      filename= paste(base_path, 'params_by_cohort',sep='')
      open_figure(filename, 'tiff', w=25, h=height_plot + 2)
      
      
      layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), 
             widths=c(1,2,2), heights=c(1,height_plot))
      
      par(mar=c(0.2, 4.1, 0.2, 2.1))

      filename_txt = paste(base_path,'param_estimates.txt',sep='')
      to_write = c()
      fileConn<-file(filename_txt)
      
      # Plot graph titlte
      plot(0,0,type='n',bty='n',axes=FALSE,xlab='',ylab='')
      title = paste("Smear-",self$analysis$smear_status[1]," TB",sep='')
      text(0,0,title, cex=6)
      
      abline_h = .5
      
      # Write cohort indices
      par(mar=c(7.1, 4.1, 0.2, 2.1))
      cex=5
      x_write = 5
      plot(0,0,type='n',bty='n',axes=FALSE,xlim=c(0,10),ylim=c(-2,height_plot), xlab='',ylab='')
      h = 0
      for (i in rev(self$analysis$cohort_ids)){
        h = h+1
        text(x = x_write,y=h,labels = i, cex=cex*0.75,adj=0.5)
      }
      text(x=x_write,y=h+1,labels='Cohort #', cex=cex,adj=0.5)
      text(x=x_write,y=-0.5,labels='mean', cex=cex,adj=0.5)
      text(x=x_write,y=-1.5,labels='combined', cex=cex,adj=0.5)
      
      abline(h = abline_h)
      
      # Plot cohort-specific results for each parameter
      if (self$analysis$smear_status[1] == 'positive'){
        xmax = list('gamma'=1,'mu_t'=1.0)
      }else{
        xmax = list('gamma'=1,'mu_t'=0.15)
      }
      par_names = list('gamma'=expression(paste('Self-recovery rate (', gamma, ')')),
                       'mu_t'=expression(paste('TB mortality rate (', mu[T],')'))
                       )
      lwd = 4
      cex=4
      par(mgp=c(5, 3, 0))
      for (par_base in self$analysis$param_bases){
        plot(0,0,type='n',main='',bty='n',axes=FALSE, cex.lab = 4,
             xlim=c(0,xmax[[par_base]]),ylim=c(-2,height_plot),xlab='/year',ylab='')
        axis(side=1, lwd = 3, lwd.ticks = 3, cex.axis=4)
        h = 0
        combined_sample = c()  # will merge all cohort-specific samples together, using cohort sizes as weights
        for (i in rev(self$analysis$cohort_ids)){
          h = h+1
          index = length(self$analysis$cohort_ids) - (h-1)
          par = paste(par_base,"[",index,"]",sep='')
          x = outputs[[par]]
          qt = quantile(x,c(0.025,0.5,0.975),names = FALSE)
          
          coh = NA
          for (coho in self$analysis$inputs$cohorts){
            if (coho$id == i){
              coh = coho
              break
            }
          }
          
          this_color = my_red
          if(grepl('ospital',coh$description) | grepl('anatori',coh$description) | grepl('ispensar',coh$description)){
            this_color = my_purple
          }
          this_size = coh$cohort_size 
          
          # work out cex value
          cex_max = 10
          coh_size_max = 2382
          if ('negative' %in% self$analysis$smear_status){
            coh_size_max = 280
          }
          alpha = cex_max * sqrt(pi/coh_size_max)
          this_cex = alpha * sqrt(this_size/pi)
          
          lines(x = c(qt[1], qt[3]), y=c(h,h), lwd=lwd, col=this_color)
          points(x=qt[2], y=h, cex=this_cex, pch=16, col=this_color)
          str = paste(par_base, ' for cohort ', i, ': ', round(qt[2],3), " (",round(qt[1],3),"-",round(qt[3],3),")",sep='')
          to_write = c(to_write, str)
          
          # update combined_sample
          repeat_sample = rep(x,this_size)
          combined_sample = c(combined_sample, repeat_sample)
        }
        # plot mean estimates based on random-effect model
        hyper_par_mean = paste('lambda_',par_base,sep='')
        hyper_par_sd = paste('sigma_',par_base,sep='')
        
        #ln_means = exp(outputs[[hyper_par_mean]] + 0.5*(outputs[[hyper_par_sd]])**2)  # lognormal priors
        ln_means = outputs[[hyper_par_mean]]
        if (self$analysis$priors == 'gamma'){  # lambda is actually the shape param  and sigma the rate
          ln_means = outputs[[hyper_par_mean]]/outputs[[hyper_par_sd]]
        }  
        qt = quantile(ln_means,c(0.025,0.5,0.975),names = FALSE)
        lines(x = c(qt[1], qt[3]), y=c(-0.5,-0.5),  lwd=lwd)
        points(x=qt[2], y=-0.5, cex=5, pch=18)
        str = paste("Estimates for ",par_base," (mean hyperparameter): ", round(qt[2],3), " (95CI ",round(qt[1],3),"-",round(qt[3],3),")",sep='')
        to_write = c(to_write, str)
        to_write = c(to_write, "")
        
        # estimates for combined sample
        qt_combined = quantile(combined_sample,c(0.025,0.5,0.975),names = FALSE)
        lines(x = c(qt_combined[1], qt_combined[3]), y=c(-1.5,-1.5),  lwd=lwd)
        points(x=qt_combined[2], y=-1.5, cex=5, pch=18)
        str = paste("Estimates for ",par_base," (combined sample): ", round(qt_combined[2],3), " (95CI ",round(qt_combined[1],3),"-",round(qt_combined[3],3),")",sep='')
        to_write = c(to_write, str)
        to_write = c(to_write, "")
      
        abline(h=abline_h)
        
        text(x=xmax[[par_base]]/2,y=h+1,labels=par_names[[par_base]], cex=5,adj=0.5)
      }
      
      writeLines(to_write, fileConn)
      close(fileConn)
      
      dev.off()
      
      x11();
      hist(combined_sample, main = par_base)
      # generate cohort profiles with fitted parameters
      if (generate_cohort_profiles){
        h=1
        for (i in self$analysis$cohort_ids){
          h = h+1
          mu_t_str = paste("mu_t[",h-1,"]",sep='')
          gamma_str = paste("gamma[",h-1,"]",sep='')
          mu_str = paste("e_mu[",h-1,"]",sep='')
          fitted_params = list('mu_t'=outputs[[mu_t_str]],'gamma'=outputs[[gamma_str]], 'mu'=outputs[[mu_str]])
          
          coh = NA
          for (cohort in self$analysis$inputs$cohorts){
            if (cohort$id == i){
              coh=cohort
              break
            }
          }
          if (!is.na(coh)){
            self$analysis$inputs$generate_cohort_profile(coh,fitted_params)
          }
        }
      }
      
      # save workspace
      filename = paste(self$analysis$base_path,'workspace.RData',sep='')
      save.image(filename)
    },
    
    plot_hyper_posteriors = function(){
      base_path = self$analysis$base_path
      data = as.data.frame(self$analysis$stan_fit)

      
      filename= paste(base_path, 'hyper_posteriors',sep='')
      open_figure(filename, 'png', w=8, h=8)
      par(mfrow = c(2,2), mar=c(5.1,4.1,2.1,2.1))
      
      hist(data$lambda_mu_t, xlab='TB mortality rate (mean)', main='',breaks=20)
      hist(data$lambda_gamma, xlab='self-recovery rate (mean)', main='',breaks=20)
      
      hist(data$sigma_mu_t, xlab='TB mortality rate (sd)', main='',breaks=20)
      hist(data$sigma_gamma, xlab='self-recovery rate (sd)', main='',breaks=20)

      dev.off()
      
      filename= paste(base_path, 'hyper_posteriors_pairs',sep='')
      open_figure(filename, 'png', w=8, h=6)
      plot(data$lambda_mu_t, data$lambda_gamma, cex=.5, pch=19,xlab='mean TB mortality rate (/y)', ylab = 'mean self-recovery rate (/y)')
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
      print(summary(fit, pars= self$analysis$tracked_pars)$summary)
      
      # plots of posterior density
      filename = paste(base_path,'posterior_density',sep='')
      open_figure(filename, 'png')
        p=plot(fit, plotfun = "dens",pars= self$analysis$tracked_pars)
        print(p)
      dev.off()
      
      # plots of trace with warmup
      filename = paste(base_path,'trace_with_warmup',sep='')
      open_figure(filename, 'png')
        p =plot(fit, plotfun = "trace", inc_warmup = TRUE,pars= self$analysis$tracked_pars)
        print(p)
      dev.off()
      
      # plots of trace without warmup
      filename = paste(base_path,'trace_without_warmup',sep='')
      open_figure(filename, 'png')
        p=plot(fit, plotfun = "trace", inc_warmup = FALSE,pars= self$analysis$tracked_pars)
        print(p)
      dev.off()
      
      # save workspace
      # filename = paste(base_path,'workspace.RData',sep='')
      # save.image(filename)
    },
    
    plot_mortality_profile = function(params, on_existing_plot=TRUE,lwd=2,col='black'){
      attach(params)
      
      t = seq(0,50,by=0.0001)
      y = 1 - (gamma / (gamma + mu_t)) * exp(-mu * t) -
          (mu_t / (gamma + mu_t)) * exp(-(gamma + mu + mu_t) * t)
     
      if (on_existing_plot){
        lines(t,100*y,lwd=lwd,col=col)
      }else{
        plot(t,100*y,lwd=lwd,col=col,type='l',xlab="time (years)", ylab="death %", xlim=c(0,30),
             ylim=c(0,100))
      }
      print(1 - (gamma / (gamma + mu_t)) * exp(-mu * 10) -
              (mu_t / (gamma + mu_t)) * exp(-(gamma + mu + mu_t) * 10)) 
      detach(params)
    },

    write_disease_duration_and_cfr_to_file = function(){
      
      fit = self$analysis$stan_fit
      outputs = as.data.frame(fit)
      mu_t = outputs$lambda_mu_t
      gamma =outputs$lambda_gamma
      
      base_path = self$analysis$base_path 
      filename = paste(base_path,'tb_duration_and_cfr.txt',sep='')
      
      empty_matrix = matrix(data=NA,nrow=4,ncol=3)
      out_dict = list("duration"=empty_matrix, "cfr_2"=empty_matrix, "cfr_5"=empty_matrix, "cfr_10"=empty_matrix)
      
      to_write = c()
      fileConn<-file(filename)
        years_to_loop = c(2,5,10)
        for (year in years_to_loop){
          str = paste("CFR after ", year, " years:", sep='')
          to_write = c(to_write,str)
          
          mu_s = c(0.005, 0.01, 0.05, 0.1)
          cpt_mu = 0
          for(mu in mu_s){
            cpt_mu = cpt_mu + 1
            cf = 1 - (gamma / (gamma + mu_t)) * exp(-mu * year) -
              (mu_t / (gamma + mu_t)) * exp(-(gamma + mu + mu_t) * year)
            this_y = round(100*median(cf))
            this_y_low = round(100*quantile(cf,probs = 0.025))
            this_y_high = round(100*quantile(cf,probs = 0.975))
            
            str = paste("mu=", mu, ": CFR=", this_y, " (",this_y_low,'-',this_y_high,')', sep='')
            to_write = c(to_write,str)
            
            out_dict[[paste('cfr_',year, sep='')]][cpt_mu,1] = this_y
            out_dict[[paste('cfr_',year, sep='')]][cpt_mu,2] = this_y_low
            out_dict[[paste('cfr_',year, sep='')]][cpt_mu,3] = this_y_high
            
            if (year == years_to_loop[length(years_to_loop)]){
              durations = 1/(mu_t+gamma+mu)
              d = round(median(durations),2)
              d_low = round(quantile(durations,probs = 0.025),2)
              d_high = round(quantile(durations,probs = 0.975),2)
              str = paste("mu=", mu, ": duration=", d, " (",d_low,'-',d_high,')', sep='')
              
              out_dict$duration[cpt_mu,1] = d
              out_dict$duration[cpt_mu,2] = d_low
              out_dict$duration[cpt_mu,3] = d_high
              
              to_write = c(to_write,str)
            }
          }
        }
        writeLines(to_write, fileConn)
      close(fileConn)
      return(out_dict)
    }    
  )
)


plot_durations_and_cfrs <- function(values_to_plot){
  filename= paste('durations_cfrs',sep='')
  open_figure(filename, 'pdf', w=10, h=8)
  par(mfrow = c(2,2), mar=c(5.1,4.1,2.1,2.1))
  
  title=list('duration'='Duration of untreated TB disease', 'cfr_2'= 'Cumulative death proportion after 2 years', 
             'cfr_5'= 'Cumulative death proportion after 5 years', 'cfr_10' = 'Cumulative death proportion after 10 years')
  
  ylabs =list('duration'='years', 'cfr_2'= '%', 
             'cfr_5'= '%', 'cfr_10' = '%')
  ymax = list('duration'= 7 , 'cfr_2'= 100, 
              'cfr_5'= 100, 'cfr_10' = 100)
  
  for (key in c('duration', 'cfr_2', 'cfr_5', 'cfr_10')){
    plot(0, ylab=ylabs[[key]], xlab='', main=title[[key]], xaxt='n', ylim=c(0,ymax[[key]]), xlim=c(0,14))
    mtext('SP-TB',1,line = 1,at = 3.5)
    mtext('SN-TB',1,line = 1,at = 10.5)
    
  }
  
  dev.off()
} 

source('load_data.R')
