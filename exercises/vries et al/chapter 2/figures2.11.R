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

# Visualise the stability of the 2-cycle at different r values
r1 <- 2.5
r2 <- 3
r3 <- 3.2
r4 <- 3.55

# Draw a diagonal line on the second-iterate function
second_iter <- function(xn, r){
  # Define the length of the outputs
  n_length <- length(xn)-2
  x <- numeric(n_length)
  fx <- numeric(n_length)
  
  # Define initial conditions
  x[1] <- xn[1]
  x[2] <- xn[2]
  
  for (n in 1:length(x)){
    fx[n] <- r*xn[n]*(1-xn[n])
    x[n+2] <- r*fx[n]*(1-(fx[n]))
  }
  return(x)
}

start <- 0
end <- 1
x_length <- 1000
xn <- seq(start, end, length.out = x_length)

df_2nditer_xn <- xn
df_2nditer_xn_1 <- second_iter(xn, r1)
df_2nditer_xn_2 <- second_iter(xn, r2)
df_2nditer_xn_3 <- second_iter(xn, r3)
df_2nditer_xn_4 <- second_iter(xn, r4)

df_2nditer1 <- data.frame(xn = df_2nditer_xn, xn_1 = df_2nditer_xn_1)
df_2nditer2 <- data.frame(xn = df_2nditer_xn, xn_1 = df_2nditer_xn_2)
df_2nditer3 <- data.frame(xn = df_2nditer_xn, xn_1 = df_2nditer_xn_3)
df_2nditer4 <- data.frame(xn = df_2nditer_xn, xn_1 = df_2nditer_xn_4)

# Draw the seconditeration graph and the diagonal line
second_iter_plot <- function(df_2nditer){
  ggplot(df_2nditer, aes(x = xn, y = xn_1)) +
    geom_line()+
    theme_minimal()+
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
    coord_cartesian(xlim = c(0,1), ylim = c(0,1))
}

# Plot the 2nditer
second_iter1 <- second_iter_plot(df_2nditer1)
second_iter2 <- second_iter_plot(df_2nditer2)
second_iter3 <- second_iter_plot(df_2nditer3)
second_iter4 <- second_iter_plot(df_2nditer4)


second_iter1 # fixed point at the origin is unstable, non-trivial fixed point is stable
second_iter2 # flip bifurcation, 2 cycle arises later
second_iter3 # 2-cycle arises and stable at r = 3.2
second_iter4 # 2-cycle arises but then unstable as r > 1 + sqrt(6)


# Combine the plots to see how the dynamics change
second_iter1 + second_iter2 + second_iter3 + second_iter4 + plot_layout(ncol = 4, nrow = 1)