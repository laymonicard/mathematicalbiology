# Chapter 1 Vries et al. (2006): A Course in Mathematical Biology
# Figure 1.2: Comparing solutions to the discrete-time model and continuous-time model 
# Page 6 

# Author: Lay Monica Ratna Dewi 
# DPhil in Interdisciplinary Life and Environmental Science, The University of Oxford 
# Date created: 30 December 2025


# Clear the environment
rm(list = ls())

# Turn off scientific notation 
options(scipen = 999)

#### Preparatory stage ####
# Install necessary packages and load their libraries
required_packages <- c("dplyr", "deSolve", "ggplot2")
for (package in required_packages){
  if(! package %in% installed.packages()){
    install.packages(package, dependencies = TRUE)
  }
  require(package)
}

library(deSolve)
library(ggplot2)
library(dplyr)

#### Discrete-time model ####
# Set up the time steps
n_length <- seq(0, 15, 0.5)

# Set up the parameters
# Probability of recovery  every half day
alpha <- 0.3 # rate of recovery per day 

# Convert the rate to probability of recovery for every half day
# In this case, alpha = probability of recovery every one day / one day
# Find the value of p such that alpha for a half day remains 0.3
p <- 0.3*0.5

# Set up loop for the difference equation
discrete_time_model <- function(time_length, p_recovery){
  # Initiate a blank container to store I values
  I <- numeric(length(time_length))
  
  # Set up the initial conditions
  I[1] <- 100
  
  for (n in seq_along(time_length)){
    I[n+1] <- I[n]*(1 - p_recovery)
  }
  #print(I)
  return(I)
  }

# Solve the equation
out_discrete <- discrete_time_model(n_length, p)

out1 <- data.frame(time = n_length, I = out_discrete[1:length(n_length)]) 
  
# Check the output
ggplot(out1, aes(x = time, y = I)) +
  geom_point() 


#### Continuous-time model ####

# Set up function for the differential equation
continuous_time_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)), {
    dI <- -alpha*I # number of infectious at time n
    list(dI)
  })
}

# Set up the initial conditions 
istate <- c(I = 100)

# Set up the parameters
parameters <- c(alpha = 0.3)

# Set up the time steps
start <- 0
end <- 15
tps <- 0.5
times <- seq(start, end, by = tps)

# Solve the equation
out2 <- ode(y = istate, times = times, func = continuous_time_model, parms = parameters, method = "lsoda")

# Check the output
ggplot(out2, aes(x = time, y = I)) +
  geom_line() 


#### Comparing the solutions of the two models ####
# Plot the solutions of discrete-time model and continuous-time model
ggplot()+
  # point line plot for the discrete-time model
  geom_point(data = out1, aes(x = time, y = I, color = "Discrete-time model"), size = 1.5) + 
  geom_line(data = out2, aes(x = time, y = I, color = "Continuous-time model"), linewidth = 1.2) +
  theme_minimal() +
  ggtitle("Solution for discrete-time and continuous-time model") +
  xlab("time (days)") + ylab("number of infectious")+
  scale_color_manual(
    name = "",
    values = c("Discrete-time model" = "black", "Continuous-time model" = "steelblue") 
  ) +
  labs(color = "") +
  theme(
    legend.position = c(0.8, 0.8) # left and right margin
  )

# The solutions from both models are quite close to each other

