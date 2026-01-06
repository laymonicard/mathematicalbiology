# Clear the environment
rm(list = ls())

# Introduction to Discrete-Time Models
packages <- c("dplyr", "tidyverse", "readxl", "ggplot2", "ggpubr")

# Install required packages
install_load_packages <- function(list_of_packages){
  for (package in list_of_packages){
    if (! package %in% installed.packages()){
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}

install_load_packages(packages)

# Visualise the behaviour for the deviation of the linear difference equation
curve <- function(yn, lam){
  # Define the length of the outputs
  n_length <- length(yn)
  y <- numeric(n_length)
  
  lambda <- lam
  y[1] <- yn[1]
  
  for (n in 2:length(y)){
    y[n] <- lambda*y[n-1]
    #y[n+1] <- lambda*yn[n] #alternative formula
  }
  return(y)
}

start <- 0.01
end <- 1
x_length <- 12
yn <- seq(start, end, length.out = x_length)

df_curve_1 <- curve(yn, 2)
df_curve_2 <- curve(yn, 0.5)
df_curve_3 <- curve(yn, -0.5)
df_curve_4 <- curve(yn, -2)


# We only care about what happens after the initial condition 
df_curve_a <- data.frame(n <- yn[2:length(yn)], yn = df_curve_1[2:length(yn)])
df_curve_b <- data.frame(n <- yn[2:length(yn)], yn = df_curve_2[2:length(yn)])
df_curve_c <- data.frame(n <- yn[2:length(yn)], yn = df_curve_3[2:length(yn)])
df_curve_d <- data.frame(n <- yn[2:length(yn)], yn = df_curve_4[2:length(yn)])


# Draw the parabola and the diagonal line
plot_curve <- function(curve){
  ggplot(curve, aes(x = n, y = yn)) +
    geom_point()+
    theme_minimal()
}

curve_a <- plot_curve(df_curve_a)
curve_b <- plot_curve(df_curve_b)
curve_c <- plot_curve(df_curve_c)
curve_d <- plot_curve(df_curve_d)


curve_a
curve_b
curve_c
curve_d

