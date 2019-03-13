# create colors
my_blue_light=rgb(114, 147, 203, alpha = 255,maxColorValue = 255)
my_orange_light=rgb(225, 151, 76, alpha = 255,maxColorValue = 255)
my_green_light=rgb(132, 186, 91, alpha = 255,maxColorValue = 255)
my_red_light=rgb(211, 94, 96, alpha = 255,maxColorValue = 255)
my_grey_light =rgb(128, 133, 133, alpha = 255,maxColorValue = 255)
my_purple_light = rgb(144, 103, 167, alpha = 255,maxColorValue = 255)
my_brown_light = rgb(171, 104, 87, alpha = 255,maxColorValue = 255)
my_gold_light = rgb(204, 194, 16, alpha = 255,maxColorValue = 255)

my_blue=rgb(57, 106, 177, alpha = 255,maxColorValue = 255)
my_orange=rgb(218, 124, 48, alpha = 255,maxColorValue = 255)
my_green=rgb(62, 150, 81, alpha = 255,maxColorValue = 255)
my_red=rgb(204, 37, 41, alpha = 255,maxColorValue = 255)
my_grey =rgb(83, 81, 84, alpha = 255,maxColorValue = 255)
my_purple = rgb(107, 76, 154, alpha = 255,maxColorValue = 255)
my_brown = rgb(146, 36, 40, alpha = 255,maxColorValue = 255)
my_gold = rgb(148, 139, 61, alpha = 255,maxColorValue = 255)

all_colors = c(my_blue, my_orange, my_green, my_red, my_grey, my_purple,
               my_brown, my_gold,
               my_blue_light, my_orange_light, my_green_light, my_red_light, my_grey_light, my_purple_light,
               my_brown_light, my_gold_light)

open_figure <- function(filename, format, w=10, h=10){
  filepath = paste(filename, format, sep='.')
  if (format == 'pdf'){
    pdf(filepath,width=w, height=h)
  }else if (format == 'png'){
    png(filepath, width=w, height=h, units='in', res=300)
  }else if (format == 'jpg' || format == 'jpeg'){
    jpeg(filepath, width=w, height=h, units='in', res=300)
  }else{
    print("The file format is not recognised")
  }
}

## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
