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

# Geometric growth model
# Define the growth rate parameter
r1 <- 1.5
r2 <- 0.8

# The growth model function
# Draw a diagonal line on the second-iterate function
geometric_growth <- function(n, xn, r){
  # Define the length of the outputs
  n_length <- length(n)
  x <- numeric(n_length)
  
  # Define initial conditions
  x[1] <- xn[1]
  
  for (n in 1:length(x)){
    x[n+1] <- r*x[n]
  }
  return(x)
}

n <- 1:10
xn <- 0.1
df1 <- geometric_growth(n, xn, r1)
df2 <- geometric_growth(n, xn, r2)


# Check the growth rate
(df1[2]-df1[1])/df1[1]
(df1[3]-df1[2])/df1[2]

(df2[2]-df2[1])/df2[1]
(df2[3]-df2[2])/df2[2]

# Density independent
# Indeed the growth rate is constant (0.5 and -0.2), no matter what the previous xn is

# Plot 2.14(a)
# When the xn = K, r = 0; when xn = 0, g(xn) or x[n+1] = r


# The discrete logistic model function
discrete_logistic_model <- function(n, r, K){
  # Define the length of the outputs
  n_length <- length(n)
  x <- numeric(n_length)
  
  # Define initial conditions
  xn[1] <- 0
  xn[2] <- K
  
  for (n in 1:length(x)){
    x[n] <- r*(1-xn[n]/K)
  }
  return(x)
}

n <- 1:2
r <- 1.5
K <- 2


df <- data.frame(xn = c(0, K), gxn = discrete_logistic_model(n, r, K))

# The plot
ggplot(df, aes(x = xn, y = gxn)) +
  geom_line()+
  xlim(0, 2.5)+
  ylim(0, 2)+
  theme_minimal()

