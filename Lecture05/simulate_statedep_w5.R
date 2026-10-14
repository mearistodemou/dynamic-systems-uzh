## -------------------------------------------------------
## Code to generate data from state-dependence ct model
## Michael Aristodemou
## Date: 14.10.2026
## -------------------------------------------------------

library(ggplot2) # load package for plotting

times <- seq(from = 0, to = 20, by = 1) #generate sequence of time points when subjects are measured
A <- -0.4 #continuous time state dependence
B <- 2 #continuous intercept
initialAffect <- 3 #affect at the first time point

simulateAffect <- function(Nsteps){ #function to simulate affect, given the number of steps between observations
  Nobs <- length(times) #number of observations per subject
  Affect <- rep(NA, Nobs) #create empty affect vector
  for(i in 1:Nobs){ #for each time point
    if(i == 1) Affect[i] <- initialAffect #if first time point, set to initial affect
    else{ #compute new affect state by taking a sequence of small steps in time
      AffectState <- Affect[i-1] #initialise with state at previous time point
      dt <- (times[i] - times[i-1]) / Nsteps #size of each small time step
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        dAffect <- A*AffectState + B #compute slope of affect at earlier time point
        AffectState <- AffectState + dAffect * dt #update state using slope and time step
      }
      Affect[i] <- AffectState #store state reached at this time point
    }
  }
  Affect #return the simulated affect vector
}

manySteps <- 100 #number of small steps between observations

plotData <- data.frame(Time = times, Affect = simulateAffect(manySteps)) #simulated trajectory

p1 = ggplot(plotData, aes(x = Time, y = Affect)) + # Plot the data
  geom_line() + #draw line
  geom_point() + #draw points at observed time points
  theme_bw(base_size = 22) + #clean theme with large text
  labs(x = "Time (weeks)", y = "Affect") #axis labels
p1 #show plot
