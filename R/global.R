# heatmap for ratio of pass/fail
my_rainbow <- c("green", 
                "yellow", 
                "orange",
                "red")
# pass fraction as argument, return hex
my_temp_color <- function(x) {
  # set scale 
  # color ramp used for temp gradient
  myramp <- scales::colour_ramp(my_rainbow, na.color = "red")
  #scaled_x <- scales::rescale( x, from = c(0,1), to = c(1,0) ) # invert
  myramp(x)
}
