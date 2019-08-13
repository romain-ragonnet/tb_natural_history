setwd('C:/Users/rrag0004/Models/tb_natural_history/')
#source('review_classes.R')

inputs = Inputs$new()

#_____________________________________________________________
#  Baart de la faille

#     smear_pos
inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'positive',
                    cohort_name = '22_25',
                    year_range = c(1922, 1925),
                    cohort_size = 177,
                    times = c(2, 4, 6, 8, 10),
                    perc_death = c(43, 56, 64, 68, 71),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'positive',
                    cohort_name = '26_29',
                    year_range = c(1926, 1929),
                    cohort_size = 149,
                    times = c(2, 4, 6),
                    perc_death = c(51, 61, 63),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'positive',
                    cohort_name = '30_33',
                    year_range = c(1930, 1933),
                    cohort_size = 125,
                    times = c(2, 4),
                    perc_death = c(55, 64),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
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
inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '22_25_smear_unsure',
                    year_range = c(1922, 1925),
                    cohort_size = 103,
                    times = c(2, 4, 6, 8, 10),
                    perc_death = c(13, 22, 27, 30, 34),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '26_29_smear_unsure',
                    year_range = c(1926, 1929),
                    cohort_size = 110,
                    times = c(2, 4, 6),
                    perc_death = c(3, 6, 13),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '30_33_smear_unsure',
                    year_range = c(1930, 1933),
                    cohort_size = 220,
                    times = c(2, 4),
                    perc_death = c(5, 11),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '34_35_smear_unsure',
                    year_range = c(1934, 1935),
                    cohort_size = 164,
                    times = c(2),
                    perc_death = c(2),
                    perc_alive = NULL
)

#_________________
#     smear_neg
inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '22_25_allneg',
                    year_range = c(1922, 1925),
                    cohort_size = NULL,
                    times = c(2, 4, 6),
                    perc_death = c(5, 7, 10),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '26_29_allneg',
                    year_range = c(1926, 1929),
                    cohort_size = NULL,
                    times = c(2, 4, 6),
                    perc_death = c(6, 9, 12),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
                    smear_status = 'negative',
                    cohort_name = '30_33_allneg',
                    year_range = c(1930, 1933),
                    cohort_size = NULL,
                    times = c(2, 4),
                    perc_death = c(5, 7),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Baart De La Faille',
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

inputs$add_cohort(author = 'Buhl',
                    smear_status = 'positive',
                    cohort_name = '25_29',
                    year_range = c(1925, 1929),
                    cohort_size = 314,
                    times = seq(1,10,by = 1),
                    perc_death = NULL,
                    perc_alive = c(72, 60, 56, 47, 45, 39, 38, 37, 35, 32),
                    age_distribution=list(c(16,25,157), c(26,35,86), c(36,45,32), c(46,55,20), c(56,100,19))
)

#_____________________________________________________________
#  Griep

inputs$add_cohort(author = 'Griep',
                    smear_status = 'positive',
                    cohort_name = '20_25',
                    year_range = c(1920, 1925),
                    cohort_size = 372,
                    times = c(5, 10, 15),
                    perc_death = c(52, 67, 75),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Griep',
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
inputs$add_cohort(author = 'Tattersall',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1914, 1940),
                    cohort_size = 1192,
                    times = seq(1,31,by=1),
                    perc_death = NULL,
                    perc_alive = c(71.1, 55.2, 44.2, 37, 32.1, 29.7, 26.9,
                                   25.1, 23.6, 22, 21.5, 19.9, 19.1, 18.2,
                                   17.2, 16.9, 16.2, 15.5, 15.5, 14.8, 14.8,
                                   14.5, 14.5, 14.5, 13.9, 13.9, 13.9, 12.9, 12.9, 12.9, 12.9),
                    age_distribution = list(c(0,19,86+104),c(20,29,242+188),c(30,39,74+92),c(40,100,226+70))
)


#_____________________________________________________________
#  Thompson

inputs$add_cohort(author = 'Thompson',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1928, 1940),
                    cohort_size = 406,
                    times = seq(1,10,by = 1),
                    perc_death = NULL,
                    perc_alive = c(60, 43.2, 36.7, 30.5, 26.8, 22.3, 20.3, 18.9, 17.2, 13.9),
                    age_distribution = list(c(10,19,87), c(20,29,148), c(30,39,95), c(40,100,76))
)


#_____________________________________________________________
#  Hartley (from Berg Table 12)
mean_men = (406*58+585*55+517*55+408*54+255*52+151*56+60*43)/2382
inputs$add_cohort(author = 'Hartley',
                    smear_status = 'positive',
                    cohort_name = 'Men',
                    year_range = c(1905, 1914),
                    cohort_size = 2382,
                    times = c(5),
                    perc_death = NULL,
                    perc_alive = c(mean_men),
                    age_distribution = list(c(16,20,406),c(21,25,585),c(26,30,517),c(31,35,408),
                                         c(36,40,255), c(41,45,151),c(46,50,60))
)
mean_women = (200*65 + 294*69 + 227*65 + 119*61 + 64*67 + 30*63 + 10*80)/944
inputs$add_cohort(author = 'Hartley',
                    smear_status = 'positive',
                    cohort_name = 'Women',
                    year_range = c(1905, 1914),
                    cohort_size = 944,
                    times = c(5),
                    perc_death = NULL,
                    perc_alive = c(mean_women),
                    age_distribution = list(c(16,20,200),c(21,25,294),c(26,30,227),c(31,35,119),
                                         c(36,40,64), c(41,45,30),c(46,50,10))
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

inputs$add_cohort(author = 'Braeuning',
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

inputs$add_cohort(author = 'Backer',
                    smear_status = 'positive',
                    cohort_name = 'Men',
                    year_range = c(1911, 1920),
                    cohort_size = 1172,
                    times = c(1,2,3,4,5,10,15,20),
                    perc_death = NULL,
                    perc_alive = perc_alive_Backer_men
)

inputs$add_cohort(author = 'Backer',
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

inputs$add_cohort(author = 'Trail',
                    smear_status = 'positive',
                    cohort_name = 'Men',
                    year_range = c(1911, 1928),
                    cohort_size = 1671,
                    times = c(1,2,3,4,5,10),
                    perc_death = NULL,
                    perc_alive = perc_alive_Trail_men
)

inputs$add_cohort(author = 'Trail',
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

inputs$add_cohort(author = 'Sinding-Larsen',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1906, 1932),
                    cohort_size = 1114,
                    times = c(1,2,3,4,5,10),
                    perc_death = NULL,
                    perc_alive = perc_alive_Sinding
)


#_____________________________________________________________
#  Furth smear-pos (from Berg, Table 3)
inputs$add_cohort(author = 'Furth',
                    smear_status = 'positive',
                    cohort_name = '12_15',
                    year_range = c(1912, 1915),
                    cohort_size = 303,
                    times = c(3, 5, 10),
                    perc_death = c(54.8, 70, 82.5),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Furth',
                    smear_status = 'positive',
                    cohort_name = '16_19',
                    year_range = c(1916, 1919),
                    cohort_size = 307,
                    times = c(3, 5, 10),
                    perc_death = c(61.6, 72.3, 81.8),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Furth',
                    smear_status = 'positive',
                    cohort_name = '20_24',
                    year_range = c(1920, 1924),
                    cohort_size = 386,
                    times = c(3, 5),
                    perc_death = c(53.1, 67.9),
                    perc_alive = NULL
)


#_____________________________________________________________
#   Furth smear-neg from Furth Table 5
data = list('1912'=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
            '1913'=c(0,2,0,1,2,0,1,2,0,0,2,0,0,0,1),
            '1914'=c(1,1,3,2,1,0,1,1,1,1,1,0,1,0,1),
            '1915'=c(0,3,2,2,1,5,1,0,1,0),
            '1916'=c(0,0,0,1,1,0,0,0,0,0),
            '1917'=c(1,2,1,1,1,0,0,0,0,0),
            '1918'=c(0,4,1,1,1,0,0,1,0,1),
            '1919'=c(0,2,1,1,1,0,2,0,1,0),
            '1920'=c(0,0,0,0,0),
            '1921'=c(0,1,0,2,0),
            '1922'=c(1,1,4,0,0),
            '1923'=c(1,1,0,3,0),
            '1924'=c(0,1,1,1,0)
            )
n = list('1912'=8, '1913'=36, '1914'=39, '1915'=39, '1916'=28, '1917'=38, '1918'=41, '1919'=37, '1920'=55, '1921'=36, '1922'=45, '1923'=33, '1924'=34)

get_perc_death_for_furth = function(year){
  cum_death = cumsum(data[[year]])
  perc_death = (100/n[[year]])*cum_death
  return(perc_death)
}

for (year in 1912:1924){
  inputs$add_cohort(author = 'Furth',
                      smear_status = 'negative',
                      cohort_name = as.character(year),
                      year_range = c(year, year+1),
                      cohort_size = n[[as.character(year)]],
                      times = 1:length(data[[as.character(year)]]),
                      perc_death = get_perc_death_for_furth(as.character(year)),
                      perc_alive = NULL
  )
}
#_____________________________________________________________
#  Berg (Using Nico's spreadsheet)

inputs$add_cohort(author = 'Berg',
                    smear_status = 'positive',
                    cohort_name = '',
                    year_range = c(1928, 1934),
                    cohort_size = 2042,
                    times = c(1,3,5,10),
                    perc_death = NULL,
                    perc_alive = c(69.3, 50.9, 41.8, 29.4),
                    age_distribution = list(c(15,19,307), c(20,24,509), c(25,29,413), c(30,34,277),
                                            c(35,39,158), c(40,44,141), c(45,49,86), c(50,54,57), c(55,59,38), c(60,100,56))
)

#_____________________________________________________________
#  Munchbach (Using Berg, Table 11)

inputs$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_20_21',
                    year_range = c(1920, 1921),
                    cohort_size = 266,
                    times = c(2,3,5),
                    perc_death = c(30.1, 38, 48.1),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_22_23',
                    year_range = c(1922, 1923),
                    cohort_size = 681,
                    times = c(2,3,5),
                    perc_death = c(32.8, 38.8, 48.6),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_24_25',
                    year_range = c(1924, 1925),
                    cohort_size = 504,
                    times = c(2, 3, 5),
                    perc_death = c(33.9, 42.9, 46.6),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'men_26_27',
                    year_range = c(1926, 1927),
                    cohort_size = 660,
                    times = c(2),
                    perc_death = c(31.4),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'women_20_21',
                    year_range = c(1920, 1921),
                    cohort_size = 428,
                    times = c(2, 3, 5),
                    perc_death = c(44.9, 52.6, 61.2),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'women_22_23',
                    year_range = c(1922, 1923),
                    cohort_size = 497,
                    times = c(2, 3, 5),
                    perc_death = c(40.6, 49.1, 54.7),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Munchbach',
                    smear_status = 'positive',
                    cohort_name = 'women_24_25',
                    year_range = c(1924, 1925),
                    cohort_size = 433,
                    times = c(2, 3, 5),
                    perc_death = c(30.0, 36.7, 39.0),
                    perc_alive = NULL
)

inputs$add_cohort(author = 'Munchbach',
                     smear_status = 'positive',
                     cohort_name = 'women_26_27',
                     year_range = c(1926, 1927),
                     cohort_size = 497,
                     times = c(2),
                     perc_death = c(32.2),
                     perc_alive = NULL
)

#_____________________________________________________________
#  Lindhart (Table 33, see excel calculations)
ages_men = list(c(0,14,4.9), c(15,19,11.6), c(20,24,16.6), c(25,29,12.8), c(30,34,10.8), c(35,44,14.8), c(45,54,12), c(55,64,9.5), c(65,100,7) )
ages_women = list(c(0,14,5.2), c(15,19,13.1), c(20,24,18.4), c(25,29,16.4), c(30,34,11.4), c(35,44,13.3), c(45,54,7), c(55,64,7.5), c(65,100,7.7) )

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'men_1930',
                    year_range = c(1930, 1931),
                    cohort_size = 1162,
                    times = c(1,2,3,4,5),
                    perc_death = c(44.7,56.2,61.6,64.9,68.3),
                    perc_alive = NULL,
                    age_distribution = ages_men
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'men_1931',
                    year_range = c(1931, 1932),
                    cohort_size = 1140,
                    times = c(1,2,3,4),
                    perc_death = c(42.5, 52.9, 59.1, 64),
                    perc_alive = NULL,
                    age_distribution = ages_men
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'men_1932',
                    year_range = c(1932, 1933),
                    cohort_size = 1138,
                    times = c(1,2,3),
                    perc_death = c(44.4, 54.6, 60.6),
                    perc_alive = NULL,
                    age_distribution = ages_men
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'men_1933',
                    year_range = c(1933, 1934),
                    cohort_size = 1004,
                    times = c(1,2),
                    perc_death = c(45.6, 55.7),
                    perc_alive = NULL,                    
                    age_distribution = ages_men
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'men_1934',
                    year_range = c(1934, 1935),
                    cohort_size = 968,
                    times = c(1),
                    perc_death = c(42.5),
                    perc_alive = NULL,
                    age_distribution = ages_men
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'women_1930',
                    year_range = c(1930, 1931),
                    cohort_size = 1383,
                    times = c(1,2,3,4,5),
                    perc_death = c(42.4, 56.5, 63.4, 67.1, 70.9),
                    perc_alive = NULL,
                    age_distribution = ages_women
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'women_1931',
                    year_range = c(1931, 1932),
                    cohort_size = 1342,
                    times = c(1,2,3,4),
                    perc_death = c(42.5, 54.6, 61.9, 66.6),
                    perc_alive = NULL,
                    age_distribution = ages_women
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'women_1932',
                    year_range = c(1932, 1933),
                    cohort_size = 1398,
                    times = c(1,2,3),
                    perc_death = c(44.6, 54.3, 60.7),
                    perc_alive = NULL,
                    age_distribution = ages_women
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'women_1933',
                    year_range = c(1933, 1934),
                    cohort_size = 1205,
                    times = c(1,2),
                    perc_death = c(45.1, 56.1),
                    perc_alive = NULL,
                    age_distribution = ages_women
)

inputs$add_cohort(author = 'Lindhart',
                    smear_status = 'positive',
                    cohort_name = 'women_1934',
                    year_range = c(1934, 1935),
                    cohort_size = 1057,
                    times = c(1),
                    perc_death = c(44.2),
                    perc_alive = NULL,
                    age_distribution = ages_women
)


#_____________________________________________________________
#  Magnusson (Tables 16 (Sm-) p44 and 17(Sm+) p46)
inputs$add_cohort(author = 'Magnusson',
                    smear_status = 'negative',
                    cohort_name = 'men',
                    year_range = c(1916, 1935),
                    cohort_size = 133,
                    times = c(4,10,20),
                    perc_death = c(9.8, 16.5, 20.3),
                    perc_alive = NULL,
                    age_distribution = list(c(10,14,4), c(15,19,33), c(20,24,44), c(25,29,23),c(30,39,20),c(40,50,9))
)

inputs$add_cohort(author = 'Magnusson',
                    smear_status = 'negative',
                    cohort_name = 'women',
                    year_range = c(1916, 1935),
                    cohort_size = 280,
                    times = c(4,10,20),
                    perc_death = c(7.5, 14.7, 17.9),
                    perc_alive = NULL,
                    age_distribution = list(c(10,14,20), c(15,19,61), c(20,24,72), c(25,29,55),c(30,39,55),c(40,50,17))
)

inputs$add_cohort(author = 'Magnusson',
                    smear_status = 'positive',
                    cohort_name = 'men',
                    year_range = c(1916, 1935),
                    cohort_size = 166,
                    times = c(4,10,20),
                    perc_death = c(55.4, 71.1, 75.3),
                    perc_alive = NULL,
                    age_distribution = list(c(10,14,5), c(15,19,32), c(20,24,49), c(25,29,38),c(30,39,29),c(40,100,13))
)

inputs$add_cohort(author = 'Magnusson',
                    smear_status = 'positive',
                    cohort_name = 'women',
                    year_range = c(1916, 1935),
                    cohort_size = 213,
                    times = c(4,10,20),
                    perc_death = c(68.1, 74.6, 77.5),
                    perc_alive = NULL,
                    age_distribution = list(c(8,14,11), c(15,19,34), c(20,24,52), c(25,29,46),c(30,39,52),c(40,56,18))
)

# ___________________________________
#   Preliminary work

generate_cohort_profiles <- function(inputs){
  outputs = Outputs$new(inputs)
  for (coh in inputs$cohorts){
    inputs$generate_cohort_profile(coh, c(0), c(0))
  }
}


