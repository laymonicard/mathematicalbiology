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

# Visualise the fixed points at different parabola to represent alternative steady states given different parameter values
r1 <- 0.5
r2 <- 1
r3 <- 2.5
r4 <- 3
r5 <- 3.5

# Draw a diagonal line for multiple parabolas to identify the alternative fixed points
# Define the function (discrete logistic model) for the parabola 
parabola <- function(xn, r){
  # Define the length of the outputs
  n_length <- length(xn)
  x <- numeric(n_length)
  
  for (n in seq_along(x)){
    x[n] <- r*xn[n]*(1-(xn[n]))
  }
  return(x)
}

start <- 0
end <- 1
x_length <- 500
xn <- seq(start, end, length.out = x_length)

df_parabola_xn <- xn
df_parabola_xn_1 <- parabola(xn, r1)
df_parabola_xn_2 <- parabola(xn, r2)
df_parabola_xn_3 <- parabola(xn, r3)
df_parabola_xn_4 <- parabola(xn, r4)
df_parabola_xn_5 <- parabola(xn, r5)

df_parabola1 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_1)
df_parabola2 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_2)
df_parabola3 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_3)
df_parabola4 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_4)
df_parabola5 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_5)


# Draw the parabola and the diagonal line
parabola_plot <- function(df1, df2, df3, df4, df5){
  ggplot() +
    geom_line(data = df1, aes(x = xn, y = xn_1))+
    geom_line(data = df2, aes(x = xn, y = xn_1))+
    geom_line(data = df3, aes(x = xn, y = xn_1))+
    geom_line(data = df4, aes(x = xn, y = xn_1))+
    geom_line(data = df5, aes(x = xn, y = xn_1))+
    theme_minimal()+
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
    coord_cartesian(xlim = c(0,1), ylim = c(0,1))
}

# Plot the parabola
# parabola1 <- parabola_plot(df_parabola1)
# parabola2 <- parabola_plot(df_parabola2)
# parabola3 <- parabola_plot(df_parabola3)
# parabola4 <- parabola_plot(df_parabola4)
# parabola5 <- parabola_plot(df_parabola5)
# 
# parabola1
# parabola2
# parabola3
# parabola4
# parabola5


parabola_all <- parabola_plot(df_parabola1, df_parabola2, df_parabola3, df_parabola4, df_parabola5)