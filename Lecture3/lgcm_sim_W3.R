######################################
# Hierarchical regression simulation
# Lecture 3: 02.10.2025
# Michael E. Aristodemou
######################################

# install package if it is missing
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2) # package for plotting

Nsubject <- 20 # specify number of subjects
times <- seq(from=0, to=20, by=1) # create a sequence of time points to store
Nobs <- length(times) # specify number of observations

initialAffect <- rnorm(n = Nsubject, mean = 5, sd =2) # sample intercept for each subject
timeCoefficients <- rnorm(n = Nsubject, mean = 0.51, sd = .2) + # sample slope slope for each subject
  scale(initialAffect)* -0.3 # correlate slope with intercept

cor(initialAffect, timeCoefficients) # check correlation between slope and intercept

# create data frame to hold observations
data <- data.frame(Subject = rep(NA,Nsubject*Nobs),
                   Time = rep(NA,Nsubject*Nobs),
                   Affect = rep(NA, Nsubject*Nobs)
)

row <- 0 #initialize row counter, to track which row of the data.frame we are on
for(subi in 1:Nsubject){
  for(obsi in 1:Nobs){ #for each observation of a subject
    row <- row + 1 # add an integer to the row to keep track of the current observation (row)
    data$Affect[row] <- initialAffect[subi] + # currentt affect is initial affect plus...
      times[obsi] * timeCoefficients[subi] # the effect of time that has passed since.
    data$Time[row] <- times[obsi] # store time point for current observation (row)
    data$Subject[row] <- subi # store subject identifier for current observation (row)
  }
}


data$Affect <- data$Affect + rnorm(n=nrow(data), mean = 0, sd = .1) # add random noise to the affect data

# plot subject specific trajectories, color-coded by subject
ggplot(data, aes(x = Time, y = Affect, color = as.factor(Subject))) +
  geom_point() + # draw points that show affect during each week
  geom_line() + # line that connects the points to map the trajectory
  theme_bw()+ # set the background for the plot
  labs(x = "Time (weeks)", y = "Affect", color = "Subject")+ # labels for figure axes
  theme(legend.position = "none") # remove legend
