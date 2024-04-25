rm(list=ls())
library(tidyverse)
library(gganimate)

make_dot <- function(n_breaths = 5, 
                     inhale = 3,
                     hold_full = 3, 
                     exhale = 3,
                     hold_empty = 3){
  fps <- 10
  nframes <- fps * n_breaths * (inhale + hold_full + exhale + hold_empty)
  breath_cycle <- c(
    seq(1.5*pi, 2.5*pi, length.out = fps * inhale) %>% sin(),
    rep(1, fps * hold_full),
    seq(2.5*pi, 3.5*pi, length.out = fps * exhale) %>% sin(),
    rep(-1, fps * hold_empty)
  ) %>% 
    rep(n_breaths)
  action <- c(
    rep('inhale', fps * inhale),
    rep('hold', fps * hold_full),
    rep('exhale', fps * exhale),
    rep('hold', fps * hold_empty)
  ) %>% 
    rep(n_breaths)
  df <- data.frame(
    size = breath_cycle, 
    time = 1:length(breath_cycle),
    action = action 
    )
  #initial_plot <- ggplot(data.frame(x=c(0,100), y=c(100,0)))
  plot <- ggplot(df) + 
    geom_point(aes(size=size, color = action, x=50, y=50)) +
    transition_states(time) +
    theme_void() +
    theme(legend.position = "none") +
    # labs(caption = time) +
    ggtitle('{closest_state}') +
    ease_aes('sine-in-out')
 animate(plot, 
         nframes = nframes,
         fps = fps
         )
} 
make_dot(5, 3, 4, 3, 4)

anim_save('test.gif', make_dot(5, 3, 4, 3, 4))

# TODO
# * label with time
# * make circle bigger... sort out the size.

