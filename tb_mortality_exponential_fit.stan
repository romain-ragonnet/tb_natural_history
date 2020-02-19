functions{
   // function to find the index of a given year in the vector conatining all years
  int[,] find_year_indices(int year, int[,] years){
    int cpt;
    int index[2,1]; 
    
    index[1,1] = 0;
    index[2,1] = 0;
    cpt = 0;
    for (i in 1:45){
      if (years[i,1] == year){
        cpt = cpt+1;
        index[cpt,1] = i;
      }
    } 
    return index;
  }
  
}

data{
  int<lower=0> nrow_tb_mortality_data; // number of datapoints for tb mortality (Sweden and Denmark)

  int<lower=0> tb_mortality_data_years[nrow_tb_mortality_data,1];
  vector<lower=0>[nrow_tb_mortality_data] tb_mortality_data_values;
}

parameters{
   real<lower=0, upper=3> a; // scaling parameter of the exponential model used for tb mortality (a*exp(-b(t-1900)))
  real<lower=0, upper=0.1> b; // rate parameter of the exponential model used for tb mortality (a*exp(-b(t-1900)))
  real<lower=0> sigma_tb_mort;  // sd of normal distribution for likelihood calculation around TB mortality data
}

model{
  real modelled_tb_mortality;
  int index[2,1];

  for (year in 1901:1935){
      modelled_tb_mortality = a*exp(-b*(year - 1900));
      index = find_year_indices(year, tb_mortality_data_years);
      for (j in 1:2){
        if (index[j,1]>0){
          tb_mortality_data_values[index[j,1]] ~ normal(modelled_tb_mortality, sigma_tb_mort);
        }
      }
  }
  
}
