################################################
# Adding noise to Jill's trajectory
# Michael E. Aristodemou
# Week 4
################################################

# Generate data for multiple subjects with individual differences
NSubjects <- 1
times <- seq(from=0, to=20, by=1) #generate sequence of time points when subjects are measured
Nobs <- length(times) #number of observations per subject
initialAffect <- rnorm(n = NSubjects, mean = 5, sd = 2)
A <- -.1 #continuous time state dependence
B <- 1 #continuous intercept
G <- 0.4 #system noise coefficient
#create empty data.frame to fill step by step
data <- data.frame(Subject= rep(NA,NSubjects*Nobs),
                   Time = rep(NA,NSubjects*Nobs),
                   Affect = rep(NA,NSubjects*Nobs))
Nsteps <- 100 #number of steps in time to compute between each observation (more precise)
row <- 0 #initialize row counter, to track which row of the data.frame we are on
for(subi in 1:NSubjects){
  for(obsi in 1:Nobs){ #for each observation of a subject
    row <- row + 1
    if(obsi==1) AffectState <- initialAffect[subi] #if first time point, set to initial affect
    if(obsi>1){ #else compute new affect state by taking a sequence of small steps in time
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        dAffect <- A*AffectState + B #compute deterministic slope of affect at earlier time point
        AffectState <- AffectState + dAffect * 1/Nsteps + #update state using slope and time step
          G * rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #and add system noise
      }
    }
    data$Affect[row] <- AffectState #input affect data
    data$Time[row] <- times[obsi] #input time data
    data$Subject[row] <- subi #input subject data
  }
}

data$Affect <- data$Affect + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error

p2 = ggplot(data, # Plot the data
       aes(x = Time, y = Affect, color = as.factor(Subject))) +
  geom_line() +
  geom_point() +
  theme_bw()+
  labs(x = "Time (weeks)", y = "Affect")+
  theme(legend.position = "none")

