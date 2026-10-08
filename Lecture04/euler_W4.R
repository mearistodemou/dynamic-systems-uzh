library(ggplot2)

times <- seq(from = 0, to = 20, by = 1) #generate sequence of time points when subjects are measured
A <- -.2 #continuous time state dependence
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

fewSteps <- 1 #number of steps for the coarse approximation
manySteps <- 100 #number of steps for the fine approximation (increased precision)

plotData <- rbind( #combine both simulations into one data frame
  data.frame(Time = times, Affect = simulateAffect(fewSteps), #coarse approximation
             Approximation = paste(fewSteps, "step(s)")), #label for the legend
  data.frame(Time = times, Affect = simulateAffect(manySteps), #fine approximation
             Approximation = paste(manySteps, "steps")) #label for the legend
)

p1 = ggplot(plotData, # Plot the data
            aes(x = Time, y = Affect, colour = Approximation)) + #colour lines by approximation
  geom_line() + #draw lines
  geom_point() + #draw points at observed time points
  theme_bw(base_size = 22) + #clean theme with large text
  labs(x = "Time (weeks)", y = "Affect", colour = "Steps per interval") #axis and legend labels
p1 #show plot

# add exact trajectory as dashed black line
p1 + stat_function(fun = function(t) -B/A + (initialAffect + B/A) * exp(A * t),
                   colour = "black", linetype = "dashed")
