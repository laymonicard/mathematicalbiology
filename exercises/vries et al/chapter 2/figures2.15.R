# Clear the environment
rm(list = ls())

# Introduction to Discrete-Time Models
packages <- c("dplyr", "tidyverse", "readxl", "ggplot2", "ggpubr", "patchwork")

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

# Figure 2.15(a): Graph of g(x) for r > 1
# The curve 
monotonic_curve <- function(xn, r){
  # Define the length of the outputs
  n_length <- length(xn)
  x <- numeric(n_length)
  
  # Define parameter values
  K <- 0.45
  
  for (n in seq_along(x)){
    x[n] <- r/(1+((r-1)/K*xn[n]))
  }
  return(x)
}

start <- 0
end <- 1
x_length <- 50
xn <- seq(start, end, length.out = x_length)
r1 <- 2.8

df_curve_xn <- xn
df_curve_xn_1 <- monotonic_curve(xn, r1)
df_curve1 <- data.frame(xn = df_curve_xn, xn_1 = df_curve_xn_1)

# Draw the parabola and the diagonal line
curve_plot <- function(df_curve){
  ggplot(df_curve, aes(x = xn, y = xn_1)) +
    geom_line()+
    theme_minimal()
}

# Plot the curve
curve1 <- curve_plot(df_curve1)
curve1


# Figure 2.15(b): Graph of g(x)x for any input of xn
monotonic_curve <- function(xn, r){
  # Define the length of the outputs
  n_length <- length(xn)
  x <- numeric(n_length)
  
  # Define parameter values
  K <- 0.45
  
  for (n in seq_along(x)){
    x[n] <- r*xn[n]/(1+(r-1)/K*xn[n])
  }
  return(x)
}

start <- 0
end <- 1
x_length <- 500
xn <- seq(start, end, length.out = x_length)
r1 <- 2.8

df_curve_xn <- xn
df_curve_xn_1 <- monotonic_curve(xn, r1)
df_curve2 <- data.frame(xn = df_curve_xn, xn_1 = df_curve_xn_1)

# Draw the parabola and the diagonal line
curve_plot <- function(df_curve){
  ggplot(df_curve, aes(x = xn, y = xn_1)) +
    geom_line()+
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
    #coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
    theme_minimal()
}

# Plot the curve
curve2 <- curve_plot(df_curve2)
curve2


# Stability when 0 < r < 1
r2 <- 0.5
df_curve_xn_2 <- monotonic_curve(xn, r2)
df_curve3 <- data.frame(xn = df_curve_xn, xn_1 = df_curve_xn_2)

curve3 <- curve_plot(df_curve3)
curve3



