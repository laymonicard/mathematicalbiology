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

# Example from https://rpubs.com/DistribEcology/880

# rmax <- 30
# plot(-1, -1, xlim = c(0, rmax), ylim = c(0, 1000), xlab = "r", ylab = "N")
# a <- 0.01
# r <- seq(0, rmax, by = 0.1)
# 
# #Next we simply iterate a time series function (the Ricker equation in this case) n times. So we simply start with the first value of r, run the population to n and then select all the unique values after we think it's come to a steady state (40 is a reasonable point but you can change it) and just plot them as points! That's it, you're done
# 
# n <- 100
# 
# for (z in 1:length(r)) {
#   xl <- vector()
#   xl[1] <- 10
#   for (i in 2:n) {
#     
#     xl[i] <- xl[i - 1] * r[z] * exp(-a * xl[i - 1])
#     
#     print(xl)
#     
#   }
#   uval <- unique(xl[40:n]) # unique points are selected as the system has reached the steady state because in the steady state, the values will oscillate only between certain range of values or within a certain range of values
#   points(rep(r[z], length(uval)), uval, cex = 0.1, pch = 19)
# }
# 
# 
# library(ggplot2)
# rmax <- 30
# out.df <- matrix(NA, ncol = 2, nrow = 0)
# a <- 0.01
# r <- seq(0, rmax, by = 0.01)
# n <- 100
# 
# for (z in 1:length(r)) {
#   
#   xl <- vector()
#   xl[1] <- 10
#   for (i in 2:n) {
#     
#     xl[i] <- xl[i - 1] * r[z] * exp(-a * xl[i - 1])
#     
#   }
#   uval <- unique(xl[40:n])
#   ### Here is where we can save the output for ggplot
#   out.df <- rbind(out.df, cbind(rep(r[z], length(uval)), uval))
# }
# out.df <- as.data.frame(out.df)
# colnames(out.df) <- c("r", "N")
# ggplot(out.df, aes(x = r, y = N)) + geom_point(size = 0.5)



#Example from https://rpubs.com/DistribEcology/880

rmax <- 4
plot(-1, -1, xlim = c(0, rmax), ylim = c(0, 1), xlab = "r", ylab = "N")
a <- 0.01
r <- seq(0, rmax, by = 0.1)

#Next we simply iterate a time series function (the Ricker equation in this case) n times. So we simply start with the first value of r, run the population to n and then select all the unique values after we think it's come to a steady state (40 is a reasonable point but you can change it) and just plot them as points! That's it, you're done

n <- 100

for (z in 1:length(r)) {
  xl <- vector()
  xl[1] <- 0.1
  for (i in 2:n) {

    xl[i] <- r[z]*xl[i-1]*(1-xl[i-1])

    #print(xl)

  }
  uval <- unique(xl[40:n]) # unique points are selected as the system has reached the steady state because in the steady state, the values will oscillate only between certain range of values or within a certain range of values
  points(rep(r[z], length(uval)), uval, cex = 0.1, pch = 19)
}

# Visualise the behaviour for the deviation of the linear difference equation
start_iterate <- 1
end_iterate <- 300
all_iterates <- seq(start_iterate, end_iterate, by = 1)


r_start <- 0
r_end <- 4
r <- seq(r_start, r_end, by = 0.009)

plot(-1, -1, xlim = c(0, r_end), ylim = c(0, 1), xlab = "r", ylab = "N")


orbital_bifurcation <- function(r){

  # For each r value
  for (n in seq_along(r)){
    # Prepare values storage
    df_length <- length(all_iterates)
    
    # print(df_length)
    
    x <- numeric(df_length)
    x[1] <- 0.1
    # print(x)
    
    # Iterates for 100 times
    for (i in 2:length(all_iterates)){
      x[i] <- r[n]*x[i-1]*(1-x[i-1])
    }
    unique_value <- unique(x[200:i])
    #return(unique_value)
    points(rep(r[n], length(unique_value)), unique_value, cex = 0.1, pch = 19)
  }
  # unique_value <- unique(x[40:n])
  # #return(unique_value)
  # points(rep(r[n], length(unique_value)), unique_value, cex = 0.1, pch = 19)
  }

orbital_bifurcation(r)

