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


# Load the dataset
df <- read_xlsx("exercises/vries et al/chapter 2/P_aurelia_growth.xlsx")

head(df)

# Visualise the mean density
ggplot(df, aes(x = day, y = p_aurelia_mean_density))+
  geom_point() +
  theme_minimal()

# The dependent variable is p[n+1], the independent variable is p[n], the parameter is deltap[n]
# We need to determine the form of deltap[n] that will give raise to the data (in this case we pick 540 as the point above which the change in pn will be negative)
# We hypothesise that deltap[n] = p[n-1] - p[n] will be proportional to k(540-pn)pn, where value of k needs to be determined
# To test our hypothesis, we plot the two equations against each other
# Use the dataframe to produce the output value of the two equations
View(df)
p_n_observed <- df$p_aurelia_mean_density

# Remove missing observations that will not be used
p_n_observed <- p_n_observed[3:length(p_n_observed)]

# Get the output of the second equation
deltan2 <- function(dataframe){
  df_length <- length(dataframe)
  out <- numeric(df_length)
  for (n in seq_along(dataframe)){
    out[n] <- (540-dataframe[n])*dataframe[n]
    }
  return(out)
  }

# The output for the second equation
delta2 <- deltan2(p_n_observed)[1:23] # remove the last output as there is no corresponding observed deltan after the last data point

# The output of the first equation is just the observed delta
delta1 <- df$change_density[4:length(df$change_density)]


# Plot the outputs of the first and the second equation against each other to check proportionality
# First, combine them as one dataframe
df_compare <- data.frame(second_eq = delta2, first_eq = delta1)
df_compare

# Plot the outputs against each other
ggplot(df_compare, aes(x = second_eq, y = first_eq))+
  geom_point() +
  geom_smooth(aes(x = second_eq, y = first_eq), method = "lm", se = F) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  xlab("p[n](540-p[n]") + ylab("p[n+1]-p[n]")+
  stat_regline_equation(label.x = 4, label.y = 8)

# The estimated value of k is 0.0014 (in the textbook, it is said to be 0.00145)

# Based on the information we have so far, we can derive a new equation for our model to be fitted agains the experimental data
# The new equation will be p[n+1] = p[n] + 0.00145(540-p[n])pn
# Solve the equation and store the outputs as simulated data

predicted_values <- function(p){
  n_length <- length(p_n_observed)
  predicted_value <- numeric(n_length)
  k <- 0.0014
  approx_carrying_capacity <- 540
  
  for (n in seq_along(p)){
    predicted_value[n + 1] <- p[n] + k*(approx_carrying_capacity - p[n])*p[n]
  }
  return(predicted_value)
}

# Store the outputs as simulated data
simulated_data <- predicted_values(p_n_observed)

# Check the length of the simulated data
length(simulated_data)
length(p_n_observed)

# Plot the simulated data against the observed data
# Combine the simulated data and observed data as one dataframe for easier plotting
# Remove the first predicted value in simulated data as it has no corresponding observed data 
df_final <- data.frame(time = 1:length(p_n_observed), observed_data = p_n_observed, simulated_data = simulated_data[2:length(simulated_data)])

df_final

# Plot the graph
ggplot()+
  # point line plot for the discrete-time model
  geom_point(data = df_final, aes(x = time, y = observed_data, color = "observed data"), size = 1.5) + 
  geom_point(data = df_final, aes(x = time, y = simulated_data, color = "simulated data"), size = 1.5) +
  theme_minimal() +
  ggtitle("Observed vs simulated data") +
  xlab("time (days)") + ylab("mean density of P. aurelia (per 0.5 cm^3)")+
  scale_color_manual(
    name = "",
    values = c("observed data" = "black", "simulated data" = "lightblue") 
  ) +
  labs(color = "") +
  theme(
    legend.position = "inside" #c(0.6, 0.4, 0.4, 0.4) # bottom, top, left, right margin
  )

# Cobwebbing to investigate the behaviour of the system
# Define the function (discrete logistic model) for the parabola 
parabola <- function(xn, rn){
  # Define the length of the outputs
  n_length <- length(xn)
  x <- numeric(n_length)
  
  # Define parameters values
  r <- rn
  K <- 1
  
  for (n in seq_along(x)){
    x[n] <- r*xn[n]*(1-(xn[n]/K)) 
  }
  return(x)
}

start <- 0
end <- 1
x_length <- 500
xn <- seq(start, end, length.out = x_length)

rn1 <- 2.8
df_parabola_xn <-xn
df_parabola_xn_1 <- parabola(xn, rn1)

df_parabola <- data.frame(xn = df_parabola_xn, xn_1 = df_parabola_xn_1)


# Draw the parabola and the diagonal line
ggplot(df_parabola, aes(x = xn, y = xn_1)) +
  geom_line()+
  theme_minimal()+
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))


# Now draw the cobweb
# The cobweb requires a separate function
cobweb <- function(x, rn){
  # Define the length of the outputs and initiate blank data frame for storing the outputs
  n_length <- length(x)
  xn <- numeric(n_length)
  yn <- numeric(n_length)
  xn_end <- numeric(n_length)
  yn_end <- numeric(n_length)
  step <- numeric(n_length)
  
  # Define parameters values
  r <- rn
  K <- 1
  
  # Define initial conditions
  step[1] <- 1
  
  # Start with a vertical move
  xn[1] <- 0.08 
  xn_end[1] <- xn[1] # the end point of x is just x as it moves vertically
  yn[1] <- xn[1] # the vertical move starts from x
  yn_end[1] <- r*xn[1]*(1-(xn[1]/K)) # the move ends at f(x0)
  
  
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
      yn_end[n] <- r*xn[n]*(1-(xn[n]/K)) #vertical move ends at f(f(x0)) or f(x1)
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

plot_cobweb <- cobweb(df_parabola$xn_1, rn1)


# Overlay the cobweb on top of the parabola and the diagonal line
ggplot(df_parabola, aes(x = xn, y = xn_1)) +
  geom_line()+
  theme_minimal()+
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  geom_segment(data = plot_cobweb, aes(x = x, y = y, xend = x_end, yend=y_end), linewidth = 0.15)


