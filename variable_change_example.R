library(tidyverse)

doors <- 1:3

test <- function(x){
  
  set.seed(x)
  
  actual <- ceiling(runif(1, 0, 3))
  
  choice <- ceiling(runif(1, 0, 3))
  
  open_door <- doors[!doors %in% c(actual, choice)]
  
  if(length(open_door) > 1){
    
    open_door <- open_door[floor(runif(1,1,2))]
    
  }
  
  switch <- doors[!doors %in% c(choice, open_door)] 
  
  switch == actual
  
}

results <- map_lgl(1:100, test)

results_df <- tibble(test = 1:length(results),
                     win = results) %>%
  mutate(cum_win_switch = cumsum(win),
         cum_win_stay = test - cum_win_switch) %>% 
  gather(strategy, cum_wins, contains("cum_win_")) %>% 
  mutate(strategy = str_remove(strategy, "cum_win_"))

ggplot(results_df, aes(test, cum_wins))+
  geom_point(aes(shape = strategy, colour = strategy))
  


