# Chapter 1 Vries et al. (2006): A Course in Mathematical Biology
# Exercises 1.4.1
# Page 8

# Author: Lay Monica Ratna Dewi 
# DPhil in Interdisciplinary Life and Environmental Science, The University of Oxford 
# Date created: 31 December 2025

# Clear the environment
rm(list = ls())

# Turn off scientific notation 
options(scipen = 999)

#### Exercise 1.4.1. ####
# Discrete-time versus continuous-time models 
# Culture of bacteria growing in a petri dish, each cell divides into two identical copies of itself every 10 minutes

# a. Choose a unit of time, and find the corresponding probability of cell division
# Let the unit of time be 10 minute
# Corresponding probability of cell division depends on the unit of time 
# So since the rate of cell division is 2/10 or 1/5
# Then the probability of cell division every minute is r*t = 1/10*1/5 or if using standard formula of p = 1-e^-rt, we will get similar result (0.02)
# Each time unit equals to 10 minutes
t <- 10
p1 <- 1/10*t #2/10 * t
p2 <- round(1-(exp(-1/10*t)),2)

p1
p2

# Write down a discrete-time model which balances the amount of cells at time t and at time t + deltat
# N[n+1] = N[n]*(1 + p)
# N[n+1] = N[n]*(1 + 0.02)

# Define the growth rate, and derive the corresponding continuous-time model 
# dN = r*Nt
# dN = 0.2*Nt

# Solve both the discrete-time and continuous-time models, and compare the solutions
#### Solve the discrete-time model ####

#### Time unit equals 10 minute (1 per original time unit) ####

# Create a function for solving the discrete-time model
# Define the parameter 
start <- 0
end <- 60
time_unit <- 10
n_length = seq(start, end, time_unit) # each interval is separated by one minute 

discrete_time_model <- function(time_length, probability_cell_division){
  N <- numeric(length(n_length))
  N[1] <- 20 # define initial condition
  
  for (n in seq_along(time_length)){
    N[n + 1] = N[n]*(1 + probability_cell_division) # define the equation
  }
  return(N)
}
  
out_discrete <- discrete_time_model(n_length, p1)
out1 <- data.frame(time = n_length, n_bacteria = out_discrete[1:length(n_length)])

ggplot(out1, aes(x = time, y = n_bacteria))+
  geom_point()


#### Solve the continuous-time model ####
continuous_time_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
    dN <- r*N
    list(dN)
})
  }

# Define the initial state
istate <- c(N = 20)

# Define parameters
parameters <- c(r = 1/10)

# Define the timesteps
start <- 0
end <- 60
time_unit <- 10
times <- seq(start, end, time_unit)

# Solve the equation
out2 <- ode(y = istate, times = times, func = continuous_time_model, parms = parameters, method = "lsoda")

ggplot(out2, aes(x = time, y = N))+
  geom_line()

#### Comparing the solutions of the two models ####
# Plot the solutions of discrete-time model and continuous-time model
ggplot()+
  # point line plot for the discrete-time model
  geom_point(data = out1, aes(x = time, y = n_bacteria, color = "Discrete-time model"), size = 1.5) + 
  geom_line(data = out2, aes(x = time, y = N, color = "Continuous-time model"), linewidth = 1.2) +
  theme_minimal() +
  ggtitle("Solution for discrete-time and continuous-time model") +
  xlab("time (10-minute interval)") + ylab("number of bacteria")+
  scale_color_manual(
    name = "",
    values = c("Discrete-time model" = "black", "Continuous-time model" = "maroon") 
  ) +
  labs(color = "") +
  theme(
    legend.position = c(0.2, 0.8) # left and right margin
  )

# The solutions from both models start to widely diverge after t=30. Continuous-time model produce higher estimates for the number of bacteria. 



#### Time unit equals 1 minute ####
t <- 1
p1 <- 1/10 * t


# Create a function for solving the discrete-time model
# Define the parameter 
start <- 0
end <- 60
time_unit <- t
n_length = seq(start, end, time_unit) # each interval is separated by one minute 

discrete_time_model <- function(time_length, probability_cell_division){
  N <- numeric(length(n_length))
  N[1] <- 20 # define initial condition
  
  for (n in seq_along(time_length)){
    N[n + 1] = N[n]*(1 + probability_cell_division) # define the equation # which will equal to 2N(t)
  }
  return(N)
}

out_discrete <- discrete_time_model(n_length, p1)
out1 <- data.frame(time = n_length, n_bacteria = out_discrete[1:length(n_length)])

ggplot(out1, aes(x = time, y = n_bacteria))+
  geom_point()


#### Solve the continuous-time model ####
continuous_time_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
    dN <- r*N
    list(dN)
  })
}

# Define the initial state
istate <- c(N = 20)

# Define parameters
parameters <- c(r = 1/10)

# Define the timesteps
start <- 0
end <- 60
time_unit <- t
times <- seq(start, end, time_unit)

# Solve the equation
out2 <- ode(y = istate, times = times, func = continuous_time_model, parms = parameters, method = "lsoda")

ggplot(out2, aes(x = time, y = N))+
  geom_line()

#### Comparing the solutions of the two models ####
# Plot the solutions of discrete-time model and continuous-time model
ggplot()+
  # point line plot for the discrete-time model
  geom_point(data = out1, aes(x = time, y = n_bacteria, color = "Discrete-time model"), size = 1.5) + 
  geom_line(data = out2, aes(x = time, y = N, color = "Continuous-time model"), linewidth = 1.2) +
  theme_minimal() +
  ggtitle("Solution for discrete-time and continuous-time model") +
  xlab("time (1-minute interval)") + ylab("number of bacteria")+
  scale_color_manual(
    name = "",
    values = c("Discrete-time model" = "black", "Continuous-time model" = "maroon") 
  ) +
  labs(color = "") +
  theme(
    legend.position = c(0.2, 0.8) # left and right margin
  )



#### Time unit equals 1/10 minute ####
t <- 1/10
p1 <- 1/10 * t


# Create a function for solving the discrete-time model
# Define the parameter 
start <- 0
end <- 60
time_unit <- t
n_length = seq(start, end, time_unit) # each interval is separated by one minute 

discrete_time_model <- function(time_length, probability_cell_division){
  N <- numeric(length(n_length))
  N[1] <- 20 # define initial condition
  
  for (n in seq_along(time_length)){
    N[n + 1] = N[n]*(1 + probability_cell_division) # define the equation
  }
  return(N)
}

out_discrete <- discrete_time_model(n_length, p1)
out1 <- data.frame(time = n_length, n_bacteria = out_discrete[1:length(n_length)])

ggplot(out1, aes(x = time, y = n_bacteria))+
  geom_point()


#### Solve the continuous-time model ####
continuous_time_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
    dN <- r*N
    list(dN)
  })
}

# Define the initial state
istate <- c(N = 20)

# Define parameters
parameters <- c(r = 1/10)

# Define the timesteps
start <- 0
end <- 60
time_unit <- t
times <- seq(start, end, time_unit)

# Solve the equation
out2 <- ode(y = istate, times = times, func = continuous_time_model, parms = parameters, method = "lsoda")

ggplot(out2, aes(x = time, y = N))+
  geom_line()

#### Comparing the solutions of the two models ####
# Plot the solutions of discrete-time model and continuous-time model
ggplot()+
  # point line plot for the discrete-time model
  geom_point(data = out1, aes(x = time, y = n_bacteria, color = "Discrete-time model"), size = 1.5) + 
  geom_line(data = out2, aes(x = time, y = N, color = "Continuous-time model"), linewidth = 1.2) +
  theme_minimal() +
  ggtitle("Solution for discrete-time and continuous-time model") +
  xlab("time (0.1-minute interval)") + ylab("number of bacteria")+
  scale_color_manual(
    name = "",
    values = c("Discrete-time model" = "black", "Continuous-time model" = "maroon") 
  ) +
  labs(color = "") +
  theme(
    legend.position = c(0.2, 0.8) # left and right margin
  )

# The solutions from both models are more closely aligned to each other at smaller time unit? 


# When is a discrete-time model appropriate? When is a continuous-time model appropriate?
# Discrete-time model is appropriate when the events can be reasonably represented in discrete time steps.
# This is usually the case for seasonal events, where the events show a consistent pattern (ups or downs) in each "season". 
# Each season can be represented as a discrete time step
# Continuous-time model is appropriate when the events are more reasonably represented in continuous timeframe 
# I suspect that events which occur at extremely small timesteps are more appropriately modelled with differential equations.
# Events that show patterns over a larger timestep are better represented by discrete-time model. Differential equations tend to overestimate the outcome for this kind of events? 

# The rate is 1/10 instead of 2/10 because we get ONE additional bacterium per 10 minutes!
# For the time unit, for easier starting point, define the timestep as it is rather than converting it from 10 minutes to to "1"
# Effectively, 1/10 rate of cell division means 1 additional bacteria per 10 minutes, which means the probability of cell division is 1 every 10 minutes!
