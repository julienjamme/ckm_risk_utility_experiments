# set theme for ggplots
library(ggplot2)


theme_set(theme_light(base_size = 18))
theme_update(
  axis.line = element_blank(),
  panel.border = element_blank(),
  panel.background = element_rect(fill = "#eff6fc"),
  panel.grid = element_line(colour = "grey85", linewidth = 0.5),
  strip.background = element_rect(colour="#e3f1fc", fill="#e3f1fc"), 
  strip.text = element_text(colour = "black")
)
