install.packages(c("coda", "mvtnorm", "devtools", "loo", "dagitty", "shape"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")

library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)
