##credit <https://nicercode.github.io/guides/mcmc/>

##define the mean and standard deviation of a normal distribution
##the aim is to estimate the mean using monte carlo methods
mean <- 0
sd <- 1

##this is just the starting point for the random number generator
##setting the seed allows reproducible results
##the random numbers generated will always be the same for a given seed
set.seed(1)

##generate 10000 samples from a normal distribition with the above parameters
samples <- rnorm(10000, mean, sd)

##the mean of the these samples is going to fairly close to the actual mean
mean(samples)

##we can replicate this a number of times, e.g. 1000 times
##again, the mean of these values is going to be close to the true mean
reps <- replicate(1000, mean(rnorm(10000, mean, sd)))

##this is made clear when looking at the summary stats of this repeated sampling
summary(reps)

##work out the cumulative mean of the samples
##this is the cumulative sum of n sampled values over n 
cum_mean <- tibble(cum_mean = cumsum(samples) / seq_along(samples),
                   sample = 1:10000)

##plot the cumulative mean
##as the number of samples increases, the estimated mean tends towards the true mean
ggplot(cum_mean, aes(sample, cum_mean))+
  geom_line()+
  geom_hline(yintercept = mean, linetype = 2)

##this is just one random approach though
##generate more random approaches (by changing where the random number generator starts)
##define a function which generates a set of samples
sampling_function <- function(x){
  
  set.seed(x)
  
  sample_size <- 10000
  
  samples <- tibble(sample_value = rnorm(sample_size, mean, sd)) %>% 
    mutate(sample = 0:(sample_size - 1),
           cum_sum = cumsum(sample_value),
           cum_mean = cum_sum / sample,
           approach = as.character(x))
  
}

##specificy number of approaches
num_approaches <- 30

##run the sampling function for the specified number of approaches, collect the results
more_samples <- map_dfr(1:num_approaches, sampling_function)

##plot the results to see that each approach converges on the true mean
ggplot(more_samples, aes(sample, cum_mean, colour = approach))+
  geom_line(alpha = 0.25)+
  scale_x_log10()+
  geom_hline(yintercept = mean, linetype = 2)+
  theme(legend.position = "none")

##the above all relates to a single independent variable, determined by mean and sd
##what happens when we consider a variable which depends on other variables?
##specifically, what happens when we consider a variable which depends on its last "draw"?
##this introduces time or state
##the variable at time t depends on the variable at time t-1
##this is what markov chains are all about
##consider a really simple example of a 3 state markov-process
##each row shows the probability a moving to a state from a given state
##rows sum to 1, because with each step we have to go somewehere (to one of the states)
markov <- tibble(state_a = c(0.5, 0.2, 0.25),
                 state_b = c(0.25, 0.1, 0.25),
                 state_c = c(0.25, 0.7, 0.5),
                 transition = paste("from", c("a", "b", "c")))

##convert it to a matrix
##we need to do this so we can do matrix multiplication
markov_matrix <- as.matrix(select(markov, -transition))

##define a function which starts in a particular state
##this state is given by assigning a probability of 1 to the state we're in
##and a probability of 0 to the other 2 states
##from this state, use the transition probability matrix to compute the probability
##of having moved to each of the 3 different states
iterate_markov <- function(x, n) {
  
  res <- matrix(NA, n+1, length(x))
  
  res[1,] <- x
  
  for (i in seq_len(n)){
    
    res[i+1,] <- x <- x %*% markov_matrix
    
  }
  
  res
  
}

##start in state a and iterate the function 15 times
##notice that after 7 steps (row 8), the system has "forgotten" its initial state
##(the probability of being in a given state flatlines)
##this means that the system is convergent
iterate_markov(c(1, 0, 0), 15)

start_lookup <- tibble(start_vector = c(c("1, 0, 0"), c("0, 1, 0"), c("0, 0, 1")),
                       start_state = c("a", "b", "c"))

##what happens if we start somewhere else?
##lets test every start state and collect the results
results_to_df <- function(x){
  
  n= 10
  
  as.data.frame(iterate_markov(x, n)) %>% 
    rename_at(1:3, ~ c("a", "b", "c")) %>%
    mutate(start_vector = paste(x, collapse = ", "),
           steps = 0:n) %>% 
    left_join(start_lookup)
  
}


results <- map_dfr(list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)), results_to_df) %>% 
  gather(state, value, 1:3)

##plot the results
##notice that each state has convereges on a probability irrespective of start state
ggplot(results, aes(steps, value, group = start_state, colour = start_state)) +
  geom_line()+
  facet_wrap(~state)

##calculate the leftmost eigenvector
v <- eigen(t(markov_matrix), FALSE)$vectors[,1]

##normalise the eigenvector
##see that the value are our convergent probabilities
v <- v/sum(v)

##interesting property of eigenvectors
##if you multiply an eigenvector by its matrix, it returns its eigenvector
matrix(v, nrow =1, ncol= 3) %*% markov_matrix

run <- function(i, n) {
  res <- integer(n)
  for (t in seq_len(n))
    ##i is the start state
    ##notice that i is redefined as the state that is arrived at each step
    res[[t]] <- i <- sample(x = nrow(markov_matrix), size = 1, prob = markov_matrix[i,])
  res
}

samples <- tibble(state = run(1, 10000),
                  step = 0:9999)

calc_proportion <- function(x){
  
  df <- samples %>% 
    mutate(cum_sum = cumsum(state == x),
           cum_mean = cum_sum / (step + 1),
           state = x)
           
}

proportion_over_steps <- map_dfr(1:3, calc_proportion) %>% 
  mutate(state = as.character(state))

ggplot(proportion_over_steps, aes(step, cum_mean, group = state, colour = state))+
  geom_line()+
  geom_hline(yintercept = v, linetype = 2)+
  scale_x_log10()


