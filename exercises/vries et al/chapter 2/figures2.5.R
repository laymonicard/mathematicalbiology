# Cobwebbing to investigate the behaviour of the system
# Define the function (xt = f(xt-1)) for the curve 
curve <- function(xn){
  # Define the length of the outputs
  n_length <- length(xn)
  x <- numeric(n_length)
  
  i <- 2
  
  for (n in seq_along(x)){
    x[n+1] <- xn[n]^i /(xn[n]^i + (1-xn[n])^i)
    # x[n+1] <- 1/(1 + exp(-i))
  }
  return(x)
}

start <- 0
end <- 1
x_length <- 400
xn <- seq(start, end, length.out = x_length)

df_curve_xn <-xn
df_curve_xn_1 <- curve(xn)

df_curve <- data.frame(xn = df_curve_xn, xn_1 = df_curve_xn_1[1:length(df_curve_xn)])


# Draw the parabola and the diagonal line
ggplot(df_curve, aes(x = xn, y = xn_1)) +
  geom_line()+
  theme_minimal()+
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))




# Now draw the cobweb
# The cobweb requires a separate function
cobweb <- function(x){
  # Define the length of the outputs and initiate blank data frame for storing the outputs
  n_length <- length(x)
  xn <- numeric(n_length)
  yn <- numeric(n_length)
  xn_end <- numeric(n_length)
  yn_end <- numeric(n_length)
  step <- numeric(n_length)
  
  # Define parameters values
  i <- 2
  
  # Define initial conditions
  step[1] <- 1
  
  # Start with a vertical move
  xn[1] <- 0.49995
  xn_end[1] <- xn[1] # the end point of x is just x as it moves vertically
  yn[1] <- xn[1] # the vertical move starts from x
  yn_end[1] <- xn[1]^i/(xn[1]^i + (1-xn[1])^i)# the move ends at f(x0)
  
  
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
      yn_end[n] <- xn[n]^i /(xn[n]^i + (1-xn[n])^i) #vertical move ends at f(f(x0)) or f(x1)
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

plot_cobweb <- cobweb(df_curve$xn_1)


# Overlay the cobweb on top of the parabola and the diagonal line
ggplot(df_curve, aes(x = xn, y = xn_1)) +
  geom_line()+
  theme_minimal()+
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash")+
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  geom_segment(data = plot_cobweb, aes(x = x, y = y, xend = x_end, yend=y_end), linewidth = 0.15)


