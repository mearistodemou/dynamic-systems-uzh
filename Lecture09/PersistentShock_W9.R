#######################################
# Adding a persistent shock
# Michael E. Aristodemou
# Week 9: 13.11.2025
#######################################

# install package if it is missing
if (!requireNamespace(c("ggplot2","dplyr","tidyr"), quietly = TRUE)) {
  install.packages(c("ggplot2","dplyr","tidyr"))
}

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Generate data for multiple subjects with individual differences
NSubjects <- 20
times <- seq(from=0, to=100, by=1) #generate sequence of time points when subjects are measured
Nobs <- length(times) #number of observations per subject
initialAffect <- rnorm(n = NSubjects, mean = 5, sd = 2)
A <- -.1 #continuous time state dependence
B <- 1 #continuous intercept
G <- .2 #system noise coefficient
M <- -0.4 #input variable
AM <- 0 #latent input process autoregression
inputTime <- times[ceiling(length(times)/2)] #intervention after ~ 1/2 of time window passed
#create empty data.frame to fill step by step
data <- data.frame(Subject= rep(NA,NSubjects*Nobs),
                   Time = rep(NA,NSubjects*Nobs),
                   Affect = rep(NA,NSubjects*Nobs))
Nsteps <- 100 #number of steps in time to compute between each observation (increased precision)
row <- 0 #initialize row counter, to track which row of the data.frame we are on
for(subi in 1:NSubjects){
  for(obsi in 1:Nobs){ #for each observation of a subject
    row <- row + 1
    if(obsi==1){
      AffectState <- initialAffect[subi] #if first time point, set to initial affect
      InputState <- 0 # initialize latent input process
    }
    if(obsi>1){ #else compute new affect state by taking a sequence of small steps in time
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        # Compute intervention effect
        dInput <- AM*InputState # deterministic slope of our intervention at earlier time point
        if(times[obsi]==inputTime) dInput <- dInput + M # add input effect if intervention time
        InputState <- InputState + dInput * 1/Nsteps # update state using slope and time step
        # Compute Affect state
        dAffect <- A*AffectState + B + InputState # deterministic slope at earlier time point
        AffectState <- AffectState + dAffect * 1/Nsteps + # update state using slope and time step
          G * rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) # and add system noise
      }
    }
    data$Affect[row] <- AffectState #input affect data
    data$Time[row] <- times[obsi] #input time data
    data$Subject[row] <- subi #input subject data
    data$Input <- ifelse(data$Time == inputTime, 1, 0) #input effect data
    data$InputState[row] <- InputState #input latent intervention process data
  }
}

data$Affect <- data$Affect + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error

# -- plots -------------
plot_data <- data %>%
  select(Subject, Time, Affect, InputState) %>%
  pivot_longer(c(Affect, InputState), names_to = "Series", values_to = "Value")

p1=ggplot(plot_data, aes(x = Time, y = Value, color = as.factor(Subject))) +
  geom_line() +
  geom_point(size = 0.6) +
  facet_grid(rows = vars(Series),
             scales = "free_y",
             labeller = as_labeller(c(Affect = "Affect",
                                      InputState = "Latent input state"))) +
  geom_vline(xintercept = inputTime, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Time (weeks)", y = NULL)
