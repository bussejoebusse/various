##load libraries
packages <- c("tidyverse", "gganimate", "gifski")

sapply(packages, function(x){
  if(!x %in% installed.packages()[,"Package"])
    install.packages(x)
  require(x, character.only = T)
})

##define colour palette
pal <- c("#00747A", "#002664")

##define sampling function
##pick a random point inside a 1x1 sqaure
sample_fun <- function(x){
  
  runif(x, 0, 1)
  
}

##choose number of runs
no_samples <- 10000

##create samples
##use distance / pythagoras formula group points "in circle" vs. "out of circle"
samples <- tibble(id = 1:no_samples,
                  x = sample_fun(no_samples),
                  y = sample_fun(no_samples)) %>% 
  mutate(circle = ifelse(sqrt(x^2+y^2)<=1, TRUE, FALSE),
         pi_estimate = 4 * (cumsum(circle) / id),
         pi_estimate_label = sprintf("%.5f",pi_estimate))

in_circle <- samples %>% 
  filter(circle == TRUE)

out_circle <- samples %>% 
  filter(circle == FALSE)

##define static lines for plot
x_lines <- tibble(x = seq(0, 1, by = 1 / no_samples)) %>% 
  mutate(circumference = sqrt(1-x^2),
         top = 1,
         bottom = 0) %>% 
  gather(type, y, 2:4)

y_lines <- tibble(y = seq(0, 1, by = 1 / no_samples)) %>% 
  mutate(left = 0,
         right = 1) %>% 
  gather(type, x, 2:3)
                        
##create plot
plot <- ggplot(NULL, aes(x, y))+
  geom_point(data = in_circle, aes(group = seq_along(id)), colour = pal[1])+
  geom_point(data = out_circle, aes(group = seq_along(id)), colour = pal[2])+
  geom_line(data = x_lines, aes(group = type))+
  geom_line(data = y_lines, aes(group = type))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

##animate plotting, adding each point in turn
anim <- plot+
  transition_reveal(id)+
  labs(title = "Runs = {prettyNum(frame_along, big.mark = ',')}
       Estimate = {samples$pi_estimate_label[as.integer(frame_along)]}")

##export
animate(anim, nframes = 100, fps = 100, end_pause = 30,
        renderer = gifski_renderer("estimate_pi.gif"))
