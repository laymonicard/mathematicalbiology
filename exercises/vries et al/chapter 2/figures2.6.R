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

# Visualise the behaviour for the deviation of the linear difference equation
curve <- function(yn, lam){
  # Define the length of the outputs
  n_length <- length(1:12)
  y <- numeric(n_length)
  
  print(n_length)
  
  lambda <- lam
  y[1] <- yn[1]
  
  for (n in 2:length(y)){
    y[n] <- lambda*y[n-1]
    #y[n+1] <- lambda^n*y[1] #alternative formula
  }
  return(y)
}

# start <- 0.01
# end <- 1
# x_length <- 12

# initiate yn
yn <- 0.55 #seq(start, end, length.out = x_length)

df_curve_1 <- curve(yn, 2)
df_curve_2 <- curve(yn, 0.5)
df_curve_3 <- curve(yn, -0.5)
df_curve_4 <- curve(yn, -2)


# We only care about what happens after the initial condition 
df_curve_a <- data.frame(n <- 1:12, yn = df_curve_1)
df_curve_b <- data.frame(n <- 1:12, yn = df_curve_2)
df_curve_c <- data.frame(n <- 1:12, yn = df_curve_3)
df_curve_d <- data.frame(n <- 1:12, yn = df_curve_4)


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




# Visualise the behaviour for the deviation of the linear difference equation
# Alternative formula
curve <- function(yn, lam){
  # Define the length of the outputs
  n_length <- length(1:12)
  y <- numeric(n_length)
  
  print(n_length)
  
  lambda <- lam
  y[1] <- yn[1]
  
  for (n in 2:length(y)){
    #y[n] <- lambda*y[n-1]
    y[n] <- lambda^n*y[1] #alternative formula
  }
  return(y)
}

# start <- 0.01
# end <- 1
# x_length <- 12

# initiate yn
yn <- 0.55 #seq(start, end, length.out = x_length)

df_curve_1 <- curve(yn, 2)
df_curve_2 <- curve(yn, 0.5)
df_curve_3 <- curve(yn, -0.5)
df_curve_4 <- curve(yn, -2)


# We only care about what happens after the initial condition 
df_curve_a <- data.frame(n <- 1:12, yn = df_curve_1)
df_curve_b <- data.frame(n <- 1:12, yn = df_curve_2)
df_curve_c <- data.frame(n <- 1:12, yn = df_curve_3)
df_curve_d <- data.frame(n <- 1:12, yn = df_curve_4)


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

# Combine the plots to see how the dynamics change
curve_a + curve_b + curve_c + curve_d + plot_layout(ncol = 2, nrow = 2)