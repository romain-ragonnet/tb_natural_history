# fixed params
tsr = 0.8
tx_duration = 0.5
mu = 1/55

# variable cdr
cdr = seq(0.,0.9,by = 0.0001)

# natural history (old)
recovery_sp_old = 0.15
mu_t_sp_old = 0.15

recovery_sn_old = 0.13
mu_t_sn_old = 0.018


# natural history (new)
recovery_sp = 0.16
mu_t_sp = 0.325

recovery_sn = 0.15
mu_t_sn = 0.018

duration_sp = 1/(recovery_sp + mu + mu_t_sp + cdr*tsr/tx_duration)
duration_sn = 1/(recovery_sn + mu + mu_t_sn + cdr*tsr/tx_duration)
duration = 0.5*(duration_sn + duration_sp)

ymax = max(duration_sn)

plot(cdr, duration, type='l',ylim=c(0,ymax),col='blue')
lines(cdr, duration_sp, col='black')
lines(cdr, duration_sn, col='gray')

