setwd('C:/Users/rrag0004/Models/tb_natural_history/review_of_models')
library(xlsx)
source("G:/My Drive/R_toolkit/graph_tools.R")


classes = rep('character', 16)
data=read.xlsx(file = 'params.xlsx',sheetIndex = 1,colClasses = classes,stringsAsFactors=FALSE)

estimates = list(
  'mu_t_sp' = c(0.39,0.33,0.453),
  'mu_t_sn' = c(0.025,0.016,0.036),
  'gamma_sp' = c(0.234,0.178,0.294),
  'gamma_sn' = c(0.148,0.085,0.242),
  'duration_sp'=c(1.58,1.38,1.82),
  'duration_sn'=c(5.47,3.54,8.67),
  'ratio'=c(15.7422,10.51824,24.63153)
)

read_data_with_CI <- function(val){
  if (grepl("\\(", val)){ # there is a CI
    splits = strsplit(val,"\\(")[[1]]
    med = splits[1]
    
    CI_splits = strsplit(splits[2],"-")[[1]]
    low = CI_splits[1]
    high = strsplit(CI_splits[2],"\\)")[[1]][1]
    
    if (med == ""){
      med = 0.5*(as.numeric(low)+as.numeric(high))
    }
  }else{
    med=val
    low = NA
    high = NA
  }
  values=c(med,low,high)
  values = as.numeric(values)
  
  return(values)
}

plot_data = function(data,smear_status='sp'){
  filename = paste("review_",smear_status,sep='')
  open_figure(filename =filename ,format = 'png',w =15,h=12)
  par(mfrow=c(1,4),
      mar=c(5.1,0,4.1,2))
  n=nrow(data)
  col_estimates = my_red
  LWD = 2
  CEX = 2
  PCH = 16
  cex_header = 3  
  bg_grey = "gray95"
  
  par(cex.axis=2,cex.lab=2)
  # study name
  plot(0,0,type='n',xlim=c(0,10),ylim=c(0,n),main=expression('Study'),bty='n',yaxt='n',xaxt='n',
       cex.main=cex_header,xlab="",ylab="")
  text(x = 1, y=0,labels = "New estimates", pos=4,cex=2.5,col = col_estimates)
  for (i in 1:n){
    h= i
    name = paste(data$Author[i], data$Year[i], sep=" ")
    if (name == 'Menzies 2018'){
      name = 'Menzies 2018*' # agegroup 15-25
    }
    text(x = 1, y=h,labels = name,pos=4,cex=2.5)
  }
  
  par_names = list('gamma'=expression(paste('Self-recovery rate (', gamma, ')')),
                   'mu_t'=expression(paste('TB mortality rate (', mu[T],')'))
  )
  x_max = list('mu_t'=0.75, 'gamma'=0.4)
  for (par in c('mu_t','gamma')){
    param_name = paste(par,"_",smear_status,sep='')
    plot(0,0,type='n',xlim=c(0,x_max[[par]]),ylim=c(0,n),main=par_names[[par]],bty='n',yaxt='n',
         cex.main=cex_header,xlab="/year",ylab="")
    #if (par == 'mu_t'){
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = bg_grey)
    #}
    
    segments(x0 = estimates[[param_name]][2] ,x1= estimates[[param_name]][3], y0=0,y1=0,col=col_estimates,lwd=LWD)
    points(x= estimates[[param_name]][1],y = 0, col=col_estimates, cex=CEX, pch=PCH)
    for (i in 1:n){
      h= i
      if (!is.na(data[[param_name]][i])){
        
        values = read_data_with_CI(data[[param_name]][i])
        
        if (par == 'mu_t' && !is.na(data$mu_t_includes_mu[i])){
          if (data$mu_t_includes_mu[i] == "yes"){
            mu = read_data_with_CI(data$mu[i])[1]
            values = values - mu
          }
        }
        
        segments(x0 = values[2] ,x1= values[3], y0=h,y1=h,lwd=LWD)
        points(x= values[1],y = h, cex=CEX, pch=PCH)
      }else{
        text(x=0.02,y=h,labels = 'NA',cex=2)
      }
    }
  }

  # duration
  param_name = paste("duration_",smear_status,sep='')
  plot(0,0,type='n',xlim=c(0,10),ylim=c(0,n),main=expression("TB duration"),bty='n',yaxt='n',
       cex.main=cex_header,xlab="years",ylab="")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = bg_grey)
  segments(x0 = estimates[[param_name]][2] ,x1= estimates[[param_name]][3], y0=0,y1=0,col=col_estimates,lwd=LWD)
  points(x= estimates[[param_name]][1],y = 0, col=col_estimates, cex=CEX, pch=PCH)
  for (i in 1:n){
    h= i
    mu_t = 0
    gamma = 0
    mu = 0.01
    for (par in c('mu_t','gamma')){
      param_name = paste(par,"_",smear_status,sep='')
      if (!is.na(data[[param_name]][i])){
        values = read_data_with_CI(data[[param_name]][i])
        if (par == 'mu_t' && !is.na(data$mu_t_includes_mu[i])){
          if (data$mu_t_includes_mu[i] == "yes"){
            this_mu = read_data_with_CI(data$mu[i])[1]
            values = values - this_mu
          }
        }
        if (par == 'mu_t'){
          mu_t = values[1]
        }else{
          gamma = values[1]
        }
      }
    }
    duration = 1/(mu_t+gamma+mu)
    points(x=duration,y = h, cex=CEX, pch=5)
    if (data$Author[i] == "Garcia" || (data$Author[i] == "Vynnycky" && smear_status=='sn')){
      text(x=8.6,y=h,labels = ">10 years",cex=1.8)
    }
  }
  dev.off()
}

plot_ratios_sp_sn = function(data){
  filename = "review_ratios"
  open_figure(filename =filename ,format = 'png',w =15, h=12)
  par(mfrow=c(1,4),
      mar=c(5.1,0,4.1,2))
  n=nrow(data)
  col_estimates = my_red
  LWD = 2
  CEX = 2
  PCH = 16
  cex_header = 3  
  bg_grey = "gray95"
  
  par(cex.axis=2,cex.lab=2)
  # study name
  plot(0,0,type='n',xlim=c(0,10),ylim=c(0,n),main=expression('Study'),bty='n',yaxt='n',xaxt='n',
       cex.main=cex_header,xlab="",ylab="")
  text(x = 1, y=0,labels = "New estimates", pos=4,cex=2.5,col = col_estimates)
  for (i in 1:n){
    h= i
    name = paste(data$Author[i], data$Year[i], sep=" ")
    if (name == 'Menzies 2018'){
      name = 'Menzies 2018*' # agegroup 15-25
    }
    text(x = 1, y=h,labels = name,pos=4,cex=2.5)
  }
  
  plot(0,0,type='n',xlim=c(0,25),ylim=c(0,n),main=expression(""),bty='n',yaxt='n',
       cex.main=cex_header,xlab="",ylab="")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = bg_grey)
  
  segments(x0 = estimates$ratio[2] ,x1= estimates$ratio[3], y0=0,y1=0,col=col_estimates,lwd=LWD)
  points(x= estimates$ratio[1],y = 0, col=col_estimates, cex=CEX, pch=PCH)
  for (i in 1:n){
    h= i
    if (!is.na(data$mu_t_sp[i])){
      
      values_sp = read_data_with_CI(data$mu_t_sp[i])
      val_sp = values_sp[1]
      values_sn = read_data_with_CI(data$mu_t_sn[i])
      val_sn = values_sn[1]
      
      #segments(x0 = values[2] ,x1= values[3], y0=h,y1=h,lwd=LWD)
      points(x= val_sp/val_sn,y = h, cex=CEX, pch=5)
    }else{
      text(x=2,y=h,labels = 'NA',cex=2)
    }
  }
  
  plot(0,0,type='n',xlim=c(0,25),ylim=c(0,n),main=expression(""),bty='n',yaxt='n',
       cex.main=cex_header,xlab="",ylab="")
  
  plot(0,0,type='n',xlim=c(0,25),ylim=c(0,n),main=expression(""),bty='n',yaxt='n',
       cex.main=cex_header,xlab="",ylab="")
  
  dev.off()
}

#for (smear_status in c('sp','sn')){
#  plot_data(data,smear_status)
#}

plot_ratios_sp_sn(data)
