rm(list=ls())
library(tidyverse)
library(gganimate)

df <- tibble(
  time = 1:11,
  size = c(100,100,100,75,50,50,50,50,50,75,100)
)

initial_plot <- ggplot(data.frame(x=c(0,100), y=c(100,0)))
plot <- initial_plot + geom_point(aes(size=size, x=50, y=50), data=df) +
  transition_states(time) +
  theme_void() +
  theme(legend.position = "none") +
  ease_aes('sine-in-out')

animate(plot)
anim_save('test.gif', animate(plot))

# TODO
# * fix timing (make animation slower?)
# * label with time? 

