##############################
# Adding cross-noise
# Michael E. Aristodemou
# Week 5: 16.10.2025
##############################

# install package if it is missing
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2) # load for visualization

# Generate data for multiple subjects with individual differences
NSubjects <- 20
times <- seq(from=0, to=40, by=1) #generate sequence of time points when subjects are measured
Nobs <- length(times) #number of observations per subject

initialAffect1 <- rnorm(n = NSubjects, mean = 5, sd = 2)
initialAffect2 <- rnorm(n = NSubjects, mean = 5, sd = 2)

A <- -.2 #continuous time state dependence
Across <- .1 #cross-effect state dependence
B <- 1 #continuous intercept
G <- .2 #unique system noise coefficient
Gcross <- .1 #common system noise coefficient

#create empty data.frame to fill step by step
data <- data.frame(Subject= rep(NA,NSubjects*Nobs),
                   Time = rep(NA,NSubjects*Nobs),
                   Affect1 = rep(NA,NSubjects*Nobs),
                   Affect2 = rep(NA,NSubjects*Nobs)) #now with affect for two individuals

Nsteps <- 100 #number of steps in time to compute between each observation (increased precision)
row <- 0 #initialize row counter, to track which row of the data.frame we are on
for(subi in 1:NSubjects){
  for(obsi in 1:Nobs){ #for each observation of a subject
    row <- row + 1
    if(obsi==1){
      Affect1State <- initialAffect1[subi] #if first time point, set to initial affect
      Affect2State <- initialAffect2[subi]
    }
    if(obsi>1){ #else compute new affect state by taking a sequence of small steps in time
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        # compute deterministic slopes of affect at earlier time point
        dAffect1 <- A*Affect1State + Across * Affect2State + B
        dAffect2 <- A*Affect2State + Across * Affect1State + B
        systemNoiseState1 <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #unique noise for subj 1
        systemNoiseState2 <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #unique noise for subj 2
        systemNoiseCrossState <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #common noise
        Affect1State <- Affect1State + dAffect1 * 1/Nsteps + #update state using slope and time step
          G * systemNoiseState1 + Gcross * systemNoiseCrossState #and add unique and common noise
        Affect2State <- Affect2State + dAffect2 * 1/Nsteps + #update state using slope and time step
          G * systemNoiseState2 + Gcross * systemNoiseCrossState #and add unique and common noise
      }
    }
    data$Affect1[row] <- Affect1State #input affect data
    data$Affect2[row] <- Affect2State #input affect data
    data$Time[row] <- times[obsi] #input time data
    data$Subject[row] <- subi #input subject data
  }
}

data$Affect1 <- data$Affect1 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error
data$Affect2 <- data$Affect2 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error

# Visualize model implied values + measurement error
p3 = ggplot(data[data$Subject==1,], # Plot the data for the first couple
       aes(x = Time, y = Affect1, color = as.factor(Subject))) +
  geom_line() +
  geom_line(aes(y = Affect2), linetype = "dashed") +
  geom_point() +
  theme_bw(base_size = 22)+
  theme(legend.position = "none")
