# ODE based model to compare  predictions based on different natural history parameters

source('summer_model.R')

# an example script to call the generic model builder file that constructs a compartmental model
# from the instructions contained in this file

selfcure_rate = 0.19
cdr=0.6
tsr=0.8
tx_duration=0.5
recovery_rate = selfcure_rate + cdr*tsr/tx_duration

tb_model <- EpiModel$new(seq(from=0, to=100, by=1),
                          c("susceptible", "latent_early" , "latent_late", "infectious"),
                          list("infectious"=0.001),
                          list(beta=5000, epsi=365.25*1.1e-3, kappa=365.25*1.e-2, nu=365.25*5.5e-6 , recovery=recovery_rate),
                          list(c("standard_flows", "recovery", "infectious", "susceptible"),
                               c("infection_density", "beta", "susceptible", "latent_early"),
                               c("standard_flows", "kappa", "latent_early", "latent_late"),
                               c("standard_flows", "epsi", "latent_early", "infectious"),
                               c("standard_flows", "nu", "latent_late", "infectious")
                               ),
                          report_progress=FALSE)
tb_model$stratify("smear", c("negative", "positive"), c("infectious"),
                   list(recovery=list(adjustments=list("negative"=0.7, "positive"=0.5)),
                        epsi=list(adjustments=list("negative"=1, "positive"=1)),
                        nu=list(adjustments=list("negative"=1, "positive"=1)),
                        universal_death_rate=list(adjustments=list("negative"=1, "positive"=2))),
                   list("negative"=0.6, "positive"=0.4), report = TRUE)


tb_model$run_model()



interpreter <- ModelInterpreter$new(tb_model)
interpreter$plot_compartment("infectious")
