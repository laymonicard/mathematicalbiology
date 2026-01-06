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
# As the map is iterative, we do not need inputs of xn within the domain of [0,1]
curve <- function(y0, r){
  # Define the length of the outputs
  y <- numeric(length(1:10))

  y[1] <- y0

  for (n in 2:length(y)){
    y[n] <- r*y[n-1]*(1-y[n-1])
  }
  return(y)
}

y1 <- 0.55
y2 <- 0.8

df_curve_1 <- curve(y1, 0.9)
df_curve_2 <- curve(y2, 2)

# We want to plot the population dynamics 
df_curve_a <- data.frame(n <- 1:length(df_curve_1), yn = df_curve_1)
df_curve_b <- data.frame(n <- 1:length(df_curve_2), yn = df_curve_2)


# Draw the parabola and the diagonal line
plot_curve <- function(curve){
  ggplot(curve, aes(x = n, y = yn)) +
    geom_point()+
    ylim(0,1)+
    theme_minimal()
}

curve_a <- plot_curve(df_curve_a)
curve_b <- plot_curve(df_curve_b)

curve_a # the population goes extinct
curve_b # the population grows and becomes stable later on


# Visualise the fixed points at different parabola to represent alternative steady states given different parameter values
r1 <- 0.9
r2 <- 2

# Draw a diagonal line for multiple parabolas to identify the alternative fixed points
# Define the function (discrete logistic model) for the parabola 
# Cobwebbing to investigate the behaviour of the system
# Define the function (discrete logistic model) for the parabola 
parabola <- function(xn, r){
  # Define the length of the outputs
  n_length <- length(xn)
  x <- numeric(n_length)
  
  # Define parameters values
  #r <- 2.8
  
  for (n in seq_along(x)){
    x[n] <- r*xn[n]*(1-(xn[n])) 
  }
  return(x)
}

start <- 0
end <- 1
x_length <- 1000
xn <- seq(start, end, length.out = x_length)

df_parabola_xn <- xn
df_parabola_xn_1 <- parabola(xn, r1)
df_parabola_xn_2 <- parabola(xn, r2)

df_parabola1 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_1)
df_parabola2 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_2)

# Draw the parabola and the diagonal line
parabola_plot <- function(df_parabola){
  ggplot(df_parabola1, aes(x = xn, y = xn_1)) +
    geom_line()+
    theme_minimal()+
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
    coord_cartesian(xlim = c(0,1), ylim = c(0,1))
}

# Plot the parabola
parabola1 <- parabola_plot(df_parabola1)
parabola2 <- parabola_plot(df_parabola2)

parabola1
parabola2

# Now draw the cobweb
# The cobweb requires a separate function
cobweb <- function(x, r, xn0){
  # Define the length of the outputs and initiate blank data frame for storing the outputs
  n_length <- length(x)
  xn <- numeric(n_length)
  yn <- numeric(n_length)
  xn_end <- numeric(n_length)
  yn_end <- numeric(n_length)
  step <- numeric(n_length)
  
  # Define initial conditions
  step[1] <- 1
  
  # Start with a vertical move
  xn[1] <- xn0
  xn_end[1] <- xn[1] # the end point of x is just x as it moves vertically
  yn[1] <- xn[1] # the vertical move starts from x
  yn_end[1] <- r*xn[1]*(1-(xn[1])) # the move ends at f(x0)
  
  
  for (n in 2:length(x)){
    if (n%%2 == 0){
      xn[n] <- yn[n-1] #the horizontal move starts at x0 (if it starts at previous yn_end, it will skip the required point by one step!)
      xn_end[n] <- yn_end[n-1] #the horizontal move ends at the value of x equals to the previous yn_end or f(x0)
      yn[n] <- yn_end[n-1] #no vertical move is made
      yn_end[n] <- yn_end[n-1] #no vertical move is made
      step[n] <- n
      
    } else {
      xn[n] <- yn_end[n-1] #no horizonal move is made
      xn_end[n] <- yn_end[n-1] #no horizonal move is made
      yn[n] <- yn_end[n-1] #vertical move starts at the previous yn_end
      yn_end[n] <- r*xn[n]*(1-(xn[n])) #vertical move ends at f(f(x0)) or f(x1)
      step[n] <- n
      
    }
    # define the end points as inputs for geom_segment
    df_cobweb <- data.frame(x = xn, 
                            x_end = xn_end, 
                            y = yn, 
                            y_end = yn_end,
                            step = step)
  }
  return(df_cobweb)
}

x01 <- 0.55
x02 <- 0.1
x03 <- 0.8

df_cobweb_1 <- cobweb(df_parabola1$xn_1, r1, x01)
df_cobweb_2 <- cobweb(df_parabola2$xn_1, r2, x02)
df_cobweb_3 <- cobweb(df_parabola2$xn_1, r2, x03)

# Overlay the cobweb on top of the parabola and the diagonal line
cobweb <- function(df_parabola, df_cob1, df_cob2){
  ggplot(df_parabola, aes(x = xn, y = xn_1)) +
    geom_line()+
    theme_minimal()+
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
    coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
    geom_segment(data = df_cob1, aes(x = x, y = y, xend = x_end, yend=y_end), linewidth = 0.15)+
    geom_segment(data = df_cob2, aes(x = x, y = y, xend = x_end, yend=y_end), linewidth = 0.15)
}

cobweb(df_parabola1, df_cobweb_1[2:1000,], df_cobweb_1[2:1000,])
cobweb(df_parabola2, df_cobweb_2, df_cobweb_3)
