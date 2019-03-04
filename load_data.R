setwd('C:/Users/rrag0004/Models/tb_natural_history/')
source('review_classes.R')

analysis = Analysis$new()


#_____________________________________________________________
#  Baart de la faille

#     smear_pos
analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'positive',
                    cohort_name = '22_25',
                    year_range = c(1922, 1925),
                    cohort_size = 177,
                    times = c(2, 4, 6, 8, 10),
                    perc_death = c(43, 56, 64, 68, 71),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'positive',
                    cohort_name = '26_29',
                    year_range = c(1926, 1929),
                    cohort_size = 149,
                    times = c(2, 4, 6),
                    perc_death = c(51, 61, 63),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'positive',
                    cohort_name = '30_33',
                    year_range = c(1930, 1933),
                    cohort_size = 125,
                    times = c(2, 4),
                    perc_death = c(55, 64),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'positive',
                    cohort_name = '34_35',
                    year_range = c(1934, 1935),
                    cohort_size = 83,
                    times = c(2),
                    perc_death = c(43),
                    perc_alive = NULL
)


#_________________
#     smear_pos then smear_neg
analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '22_25',
                    year_range = c(1922, 1925),
                    cohort_size = 103,
                    times = c(2, 4, 6, 8, 10),
                    perc_death = c(13, 22, 27, 30, 34),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '26_29',
                    year_range = c(1926, 1929),
                    cohort_size = 110,
                    times = c(2, 4, 6),
                    perc_death = c(3, 6, 13),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '30_33',
                    year_range = c(1930, 1933),
                    cohort_size = 220,
                    times = c(2, 4),
                    perc_death = c(5, 11),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '34_35',
                    year_range = c(1934, 1935),
                    cohort_size = 164,
                    times = c(2),
                    perc_death = c(2),
                    perc_alive = NULL
)

#_________________
#     smear_neg
analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '22_25_allneg',
                    year_range = c(1922, 1925),
                    cohort_size = NULL,
                    times = c(2, 4, 6),
                    perc_death = c(5, 7, 10),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '26_29_allneg',
                    year_range = c(1926, 1929),
                    cohort_size = NULL,
                    times = c(2, 4, 6),
                    perc_death = c(6, 9, 12),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '30_33_allneg',
                    year_range = c(1930, 1933),
                    cohort_size = NULL,
                    times = c(2, 4),
                    perc_death = c(5, 7),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '34_35_allneg',
                    year_range = c(1934, 1935),
                    cohort_size = NULL,
                    times = c(2),
                    perc_death = c(3),
                    perc_alive = NULL
)


#_____________________________________________________________
#  Buhl

analysis$add_cohort(author = 'Buhl',
                    smear_status = 'positive',
                    cohort_name = '25_29',
                    year_range = c(1925, 1929),
                    cohort_size = 314,
                    times = seq(1,10,by = 1),
                    perc_death = NULL,
                    perc_alive = c(72, 60, 56, 47, 45, 39, 38, 37, 35, 32)
)

#_____________________________________________________________
#  Griep

analysis$add_cohort(author = 'Griep',
                    smear_status = 'positive',
                    cohort_name = '20_25',
                    year_range = c(1920, 1925),
                    cohort_size = 372,
                    times = c(5, 10, 15),
                    perc_death = c(52, 67, 75),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Griep',
                    smear_status = 'positive',
                    cohort_name = '25_30',
                    year_range = c(1925, 1930),
                    cohort_size = 603,
                    times = c(5, 10),
                    perc_death = c(51, 67),
                    perc_alive = NULL
)


#_____________________________________________________________
#  Tattersall
analysis$add_cohort(author = 'Tattersall',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1914, 1940),
                    cohort_size = 1192,
                    times = seq(1,31,by=1),
                    perc_death = NULL,
                    perc_alive = c(71.1, 55.2, 44.2, 37, 32.1, 29.7, 26.9,
                                   25.1, 23.6, 22, 21.5, 19.9, 19.1, 18.2,
                                   17.2, 16.9, 16.2, 15.5, 15.5, 14.8, 14.8,
                                   14.5, 14.5, 14.5, 13.9, 13.9, 13.9, 12.9, 12.9, 12.9, 12.9)
)


#_____________________________________________________________
#  Thompson

analysis$add_cohort(author = 'Thompson',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1928, 1940),
                    cohort_size = 406,
                    times = seq(1,10,by = 1),
                    perc_death = NULL,
                    perc_alive = c(60, 43.2, 36.7, 30.5, 26.8, 22.3, 20.3, 18.9, 17.2, 13.9)
)


#_____________________________________________________________
#  Hartley (from Berg Table 12)
mean_men = (406*58+585*55+517*55+408*54+255*52+151*56+60*43)/2382
analysis$add_cohort(author = 'Hartley',
                    smear_status = 'positive',
                    cohort_name = 'Men',
                    year_range = c(1905, 1914),
                    cohort_size = 2382,
                    times = c(5),
                    perc_death = NULL,
                    perc_alive = c(mean_men)
)
mean_women = (200*65 + 294*69 + 227*65 + 119*61 + 64*67 + 30*63 + 10*80)/944
analysis$add_cohort(author = 'Hartley',
                    smear_status = 'positive',
                    cohort_name = 'Women',
                    year_range = c(1905, 1914),
                    cohort_size = 944,
                    times = c(5),
                    perc_death = NULL,
                    perc_alive = c(mean_women)
)

#_____________________________________________________________
#  Braeuning (from Berg, Table 3)

# function used to convert rates to percentages for Table 3 in Berg
convert_rates_to_perc_alive = function(rates){
  perc_alive = rep(0, length(rates))
  for (year in 1:length(rates)){
    if (year ==1){
      perc_alive[year] = 100 - rates[year]
    }else if(year<6){
      perc_alive[year] = perc_alive[year-1] * (1-rates[year]/100)
    }else{
      perc_alive[year] = perc_alive[year-1] * (1-rates[year]/100)**5
    }
  }
  return(perc_alive)
}

rates = c(39, 31, 21, 17, 12, 6)
perc_alive_Braeuning = convert_rates_to_perc_alive(rates)

analysis$add_cohort(author = 'Braeuning',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1920, 1921),
                    cohort_size = 607,
                    times = c(1,2,3,4,5,10),
                    perc_death = NULL,
                    perc_alive = perc_alive_Braeuning
)

#_____________________________________________________________
#  Backer (from Berg, Table 3)
rates_men = c(23.1, 20.2, 16.4, 14.5, 13.3, 9.3, 6.3, 3.7)
rates_women = c(30.3, 24.3, 18.5, 16.1, 11.8, 9.7, 5.6, 4.5)
perc_alive_Backer_men = convert_rates_to_perc_alive(rates_men)
perc_alive_Backer_women = convert_rates_to_perc_alive(rates_women)

analysis$add_cohort(author = 'Backer',
                    smear_status = 'positive',
                    cohort_name = 'Men',
                    year_range = c(1911, 1920),
                    cohort_size = 1172,
                    times = c(1,2,3,4,5,10,15,20),
                    perc_death = NULL,
                    perc_alive = perc_alive_Backer_men
)

analysis$add_cohort(author = 'Backer',
                    smear_status = 'positive',
                    cohort_name = 'Women',
                    year_range = c(1911, 1920),
                    cohort_size = 1140,
                    times = c(1,2,3,4,5,10,15,20),
                    perc_death = NULL,
                    perc_alive = perc_alive_Backer_women
)

#_____________________________________________________________
#  Trail (from Berg, Table 3)
rates_men = c(13.1, 14, 13.5, 12.9, 10.3, 7.6)
rates_women = c(12.3, 16.7, 13.9, 12.1, 10.4, 7.6)
perc_alive_Trail_men = convert_rates_to_perc_alive(rates_men)
perc_alive_Trail_women = convert_rates_to_perc_alive(rates_women)

analysis$add_cohort(author = 'Trail',
                    smear_status = 'positive',
                    cohort_name = 'Men',
                    year_range = c(1911, 1928),
                    cohort_size = 1671,
                    times = c(1,2,3,4,5,10),
                    perc_death = NULL,
                    perc_alive = perc_alive_Trail_men
)

analysis$add_cohort(author = 'Trail',
                    smear_status = 'positive',
                    cohort_name = 'Women',
                    year_range = c(1911, 1928),
                    cohort_size = 944,
                    times = c(1,2,3,4,5,10),
                    perc_death = NULL,
                    perc_alive = perc_alive_Trail_women
)

#_____________________________________________________________
#  Sinding-Larsen (from Berg, Table 3)
rates = c(32.3, 9.1, 9.1, 7.3, 7.3, 3.8)
perc_alive_Sinding= convert_rates_to_perc_alive(rates)

analysis$add_cohort(author = 'Sinding-Larsen',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1906, 1932),
                    cohort_size = 1114,
                    times = c(1,2,3,4,5,10),
                    perc_death = NULL,
                    perc_alive = perc_alive_Sinding
)


#_____________________________________________________________
#  Furth (from Berg, Table 3)
analysis$add_cohort(author = 'Furth',
                    smear_status = 'positive',
                    cohort_name = '12_15',
                    year_range = c(1912, 1915),
                    cohort_size = 303,
                    times = c(3, 5, 10),
                    perc_death = c(54.8, 70, 82.5),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Furth',
                    smear_status = 'positive',
                    cohort_name = '16_19',
                    year_range = c(1916, 1919),
                    cohort_size = 307,
                    times = c(3, 5, 10),
                    perc_death = c(61.6, 72.3, 81.8),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Furth',
                    smear_status = 'positive',
                    cohort_name = '20_24',
                    year_range = c(1920, 1924),
                    cohort_size = 386,
                    times = c(3, 5),
                    perc_death = c(53.1, 67.9),
                    perc_alive = NULL
)


#_____________________________________________________________
#  Berg (Using Nico's spreadsheet)

analysis$add_cohort(author = 'Berg',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1928, 1934),
                    cohort_size = 2042,
                    times = c(1,3,5,10),
                    perc_death = NULL,
                    perc_alive = c(69.3, 50.9, 41.8, 29.4)
)

#_____________________________________________________________
#  Munchbach (Using Berg, Table 11)

analysis$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_20_21',
                    year_range = c(1920, 1921),
                    cohort_size = 266,
                    times = c(2,3,5),
                    perc_death = c(30.1, 38, 48.1),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_22_23',
                    year_range = c(1922, 1923),
                    cohort_size = 681,
                    times = c(2,3,5),
                    perc_death = c(32.8, 38.8, 48.6),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_24_25',
                    year_range = c(1924, 1925),
                    cohort_size = 504,
                    times = c(2, 3, 5),
                    perc_death = c(33.9, 42.9, 46.6),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_26_27',
                    year_range = c(1926, 1927),
                    cohort_size = 660,
                    times = c(2),
                    perc_death = c(31.4),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'women_20_21',
                    year_range = c(1920, 1921),
                    cohort_size = 428,
                    times = c(2, 3, 5),
                    perc_death = c(44.9, 52.6, 61.2),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'women_22_23',
                    year_range = c(1922, 1923),
                    cohort_size = 497,
                    times = c(2, 3, 5),
                    perc_death = c(40.6, 49.1, 54.7),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'women_24_25',
                    year_range = c(1924, 1925),
                    cohort_size = 433,
                    times = c(2, 3, 5),
                    perc_death = c(30.0, 36.7, 39.0),
                    perc_alive = NULL
)

analysis$add_cohort(author = 'Munchbach',
                     smear_status = 'positive',
                     cohort_name = 'women_26_27',
                     year_range = c(1926, 1927),
                     cohort_size = 497,
                     times = c(2),
                     perc_death = c(32.2),
                     perc_alive = NULL
)


#______________________________________________________________-
# RUNNING methods
# analysis$plot_multi_cohort(plot_model=FALSE)
#analysis$plot_multi_cohort(plot_model=TRUE)

#analysis$plot_multi_cohort(smear_status = c('positive'), plot_model=FALSE)
#analysis$plot_multi_cohort(smear_status = c('negative'), plot_model=FALSE)

#analysis$plot_multi_cohort(smear_status = c('positive'), plot_model=TRUE)
#analysis$plot_multi_cohort(smear_status = c('negative'), plot_model=TRUE)

# analysis$plot_cohort_dates()

