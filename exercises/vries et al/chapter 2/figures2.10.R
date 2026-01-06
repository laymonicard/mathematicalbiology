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
start_iterate <- 1
end_iterate <- 50
all_iterates <- seq(start_iterate, end_iterate, by = 1)

plot(-1, -1, xlim = c(0, end_iterate), ylim = c(0, 1), xlab = "n", ylab = "xn")


cycles <- function(r){
  df_length <- length(all_iterates)
  #print(df_length)
  
  x <- numeric(df_length)
  x[1] <- 0.1
  
  # print(x)
  
  # Iterates for 100 times
  for (i in 2:length(all_iterates)){
    x[i] <- r*x[i-1]*(1-x[i-1])
    }
  return(x)
  }
r1 <- 3.2
r2 <- 3.55
r3 <- 3.88

df_cycles_1 <- cycles(r1)
df_cycles_2 <- cycles(r2)
df_cycles_3 <- cycles(r3)

df_cycles_1 <- data.frame(n = all_iterates, xn = df_cycles_1)
df_cycles_2 <- data.frame(n = all_iterates, xn = df_cycles_2)
df_cycles_3 <- data.frame(n = all_iterates, xn = df_cycles_3)

plot_cycles <- function(df_cycles){
  ggplot(df_cycles, aes(x = n, y = xn)) +
  geom_line()+
  theme_minimal()
}


cycle_plot_1 <- plot_cycles(df_cycles_1)
cycle_plot_2 <- plot_cycles(df_cycles_2)
cycle_plot_3 <- plot_cycles(df_cycles_3)

cycle_plot_1
cycle_plot_2
cycle_plot_3


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
x_length <- 500
xn <- seq(start, end, length.out = x_length)

df_parabola_xn <- xn
df_parabola_xn_1 <- parabola(xn, r1)
df_parabola_xn_2 <- parabola(xn, r2)
df_parabola_xn_3 <- parabola(xn, r3)

df_parabola1 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_1)
df_parabola2 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_2)
df_parabola3 <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_3)


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
parabola3 <- parabola_plot(df_parabola3)

parabola1
parabola2
parabola3


# Now draw the cobweb
# The cobweb requires a separate function
cobweb <- function(x, r){
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
  xn[1] <- 0.1 
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

df_cobweb_1 <- cobweb(df_parabola1$xn_1, r1)
df_cobweb_2 <- cobweb(df_parabola2$xn_1, r2)
df_cobweb_3 <- cobweb(df_parabola3$xn_1, r3)


# Overlay the cobweb on top of the parabola and the diagonal line
cobweb <- function(df_cobweb, df_parabola){
  ggplot(df_parabola, aes(x = xn, y = xn_1)) +
    geom_line()+
    theme_minimal()+
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
    coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
    geom_segment(data = df_cobweb, aes(x = x, y = y, xend = x_end, yend=y_end), linewidth = 0.15)
}

cobweb(df_cobweb_1[493:500,], df_parabola1)
cobweb(df_cobweb_2[493:500,], df_parabola2)
cobweb(df_cobweb_3[100:500,], df_parabola3)
