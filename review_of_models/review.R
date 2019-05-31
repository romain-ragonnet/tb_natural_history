setwd('C:/Users/rrag0004/Models/tb_natural_history/review_of_models')
library(xlsx)

classes = rep('character', 10)
classes[2] = 'numeric'
classes[10] = 'numeric'
data=read.xlsx(file = 'params.xlsx',sheetIndex = 1, colClasses = classes)

estimates = list(
  'mu_t_sp' = c(0.39,0.33,0.453),
  'mu_t_sn' = c(0.025,0.016,0.036),
  'gamma_sp' = c(0.234,0.178,0.294),
  'gamma_sn' = c(0.148,0.085,0.242)
)

plot_data = function(data){
  attach(data)
    
  
  detach(data)
}

plot_data(data)