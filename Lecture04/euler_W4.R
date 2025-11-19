################################################
# CT Approximation using Euler-Maruyama method
# Michael E. Aristodemou
# Week 4
################################################

times <- seq(from=0, to=20, by=1) #generate sequence of time points when subjects are measured
Nobs <- length(times) #number of observations per subject
A <- -.2 #continuous time state dependence
initialAffect <- 3
B <- 2 #continuous intercept
Affect <- rep(NA,Nobs) #create empty affect vector
Nsteps <- 100 #number of steps in time to compute between each observation (increased precision)
for(i in 1:Nobs){ #for each time point
  if(i==1) Affect[i] <- initialAffect #if first time point, set to initial affect
  else{ #compute new affect state by taking a sequence of small steps in time
    AffectState <- Affect[i-1] #initialise with state at previous time point
    for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
      dAffect <- A*AffectState + B #compute slope of affect at earlier time point
      AffectState <- AffectState + dAffect * 1/Nsteps #update state using slope and time step
    }
    Affect[i] <- AffectState
  }
}

#Affect <- Affect + rnorm(Nobs,0,.01) #add noise

p1 = ggplot(data.frame(Affect=Affect), # Plot the data
       aes(x = Time, y = Affect)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = 22)+
  labs(x = "Time (weeks)", y = "Affect")


