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
curve <- function(xn, x1, r){
  # Define the length of the outputs
  n_length <- length(xn)
  x <- numeric(n_length)
  
  print(n_length)
  print(x)
  
  x[1] <- x1
  
  for (n in 2:length(x)){
    lambda <- r*(1-2*x[n])
    x[n] <- lambda*x[n-1]
  }
  return(x)
}

start <- 0
end <- 10
xn <- seq(start, end, by = 1)

df_curve_1 <- curve(xn, 0.55, 0.9)
df_curve_2 <- curve(xn, 0.8, 2)

# We only care about what happens after the initial condition 
df_curve_a <- data.frame(n <- xn, xn = df_curve_1[1:length(df_curve_1)])
df_curve_b <- data.frame(n <- xn, xn = df_curve_2[1:length(df_curve_1)])

# Draw the parabola and the diagonal line
plot_curve <- function(curve){
  ggplot(curve, aes(x = n, y = xn)) +
    geom_point()+
    ylim(0,1)+
    theme_minimal()
}

curve_a <- plot_curve(df_curve_a)
curve_b <- plot_curve(df_curve_b)

curve_a
curve_b
