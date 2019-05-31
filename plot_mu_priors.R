names = c("Cohort #1 \n unknown age distribution", "Cohort #47 \n known age distribution")
mu_mean = c(0.01, 0.011082)
mu_sd = c(0.002, 0.001)

filename = 'outputs/prior_mu'
open_figure(filename, 'png', w=12, h=7)
par(mfrow=c(1,2))

x=seq(0,0.02,length.out = 1000)
for (i in 1:2){
  alpha = (mu_mean[i]/mu_sd[i])**2
  beta = alpha / mu_mean[i]
  
  y = dgamma(x,shape=alpha, rate=beta)
  plot(x,y,main=names[i],xlab="natural mortality rate (/y)",ylab="density",type='l',ylim=c(0,400))
  
}
dev.off()
