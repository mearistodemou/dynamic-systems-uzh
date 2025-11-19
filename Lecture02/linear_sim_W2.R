#####################################################
# Script to simulate from a linear regression model
# Lecture 2: 25.09.2025
# Michael E. Aristodemou
#####################################################

# install package if it is missing
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2) # package for plotting

Time = 0:20 # sequence of time points
beta = 5 # intercept
gamma = 0.51 # slope
eps  = rnorm(n = length(Time), mean = 0, sd = 1) # residuals

Affect = NA # empty container to store Affect

for (t in 1:length(Time)) { # tell R to solve for each time point
  Affect[t] <- beta + gamma * Time[t] + eps[t] # model to generate data from
  } 

data <- data.frame(Time = Time, Affect = Affect) # store variables in dataset

# Plot the data generated from the linear regression model
ggplot(data, aes(x = Time, y = Affect)) +
  geom_point() + # draw the observed y at each x
  geom_line() + # draw a line connecting the dots
  geom_smooth(method = "lm") + # expected mean line
  theme_bw() + # set the background (just aesthetics)
  labs(x = "Time (weeks)", y = "Affect") + # name the axes
  theme(legend.position = "none") # remove legend
