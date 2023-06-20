
# heatmap for ratio of pass/fail
my_rainbow <- c("#2ECC71", 
                "#DAF7A6", 
                "#FFC300",
                "#FF5733")
# pass fraction as argument, return hex
my_temp_color <- function(x, scale_from, scale_to) {
  # set scale 
  # color ramp used for temp gradient
  myramp <- scales::colour_ramp(my_rainbow, na.color = "grey")
  scaled_x <- scales::rescale( x, from = c(scale_from, scale_to), to = c(0,1) ) # invert
  myramp(scaled_x)
}

# merge overlapping intervals, by group
merge_overlaps <- function(dataframe, start_time, end_time, grouping) {
  dataframe %>%
    ungroup %>%
    mutate(rng = iv({{start_time}}, {{end_time}})) %>%
    reframe(rng = iv_groups(rng), .by = {{grouping}}) %>%
    #rowwise() %>%
    mutate(
      content = paste0({{grouping}}, '-merged'),
      start = vctrs::field(rng, 1),
      end = vctrs::field(rng, 2),
      dur = end - start
      ) 
}

