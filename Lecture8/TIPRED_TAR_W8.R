######################################################
# Adding a time-dependent threshold as a moderator
# Michael E. Aristodemou
# Week 8: 06.11.2025
######################################################

# install package if it is missing
if (!requireNamespace(c("ggplot2"), quietly = TRUE)) {
  install.packages(c("ggplot2"))
}

library(ggplot2)
library(ctsem)

# Generate data for multiple subjects with individual differences
NSubjects <- 20
times <- seq(from=0, to=80, by=1) #generate sequence of time points when subjects are measured
Nobs <- length(times) #number of observations per subject

initialAffect1 <- rnorm(n = NSubjects, mean = 2, sd = 0)
initialAffect2 <- rnorm(n = NSubjects, mean = 0, sd = 0)

A <- rnorm(n= NSubjects, mean = -0.6, sd=0) #continuous time state dependence
A_tar <- -0.6 # threshold effect on state-dependence

Across <- 0 # coupling coefficient

# generate continuous intercepts for each individual,
# assuming couples tend to achieve similar affect levels by the end of therapy
Bcommon <- rnorm(n = NSubjects, mean = 0, sd = 0) #common continuous intercept variance
B1 <- Bcommon + rnorm(n = NSubjects, mean = 2, sd = 0) #unique continuous intercept for subj 1
B2 <- Bcommon + rnorm(n = NSubjects, mean = 0, sd = 0) #continuous intercept for subj 2
cor(B1,B2) #check if people with higher end-of-therapy affect tend to be in a relationship

G <- 0.4 #unique system noise coefficient
Gcross <- 0 #common system noise coefficient

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
        #compute deterministic slope of affect at earlier time point
        # threshold indicator per state: +1 if >=0, else -1
        TAR1 <- ifelse(Affect1State >= 0,  1, -1)
        TAR2 <- ifelse(Affect2State >= 0,  1, -1)
        dAffect1 <- A[subi]*Affect1State + A_tar*TAR1*Affect1State + Across * Affect2State + B1[subi]
        dAffect2 <- A[subi]*Affect2State + A_tar*TAR2*Affect2State + Across * Affect1State + B2[subi]
        
        systemNoiseState1 <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #unique noise for subj 1
        systemNoiseState2 <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #unique noise for subj 2
        systemNoiseCrossState <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #common noise for both
        
        #update states using slope and time step, and add unique and common system noise
        Affect1State <- Affect1State + dAffect1 * 1/Nsteps +
          G * systemNoiseState1 + Gcross * systemNoiseCrossState
        Affect2State <- Affect2State + dAffect2 * 1/Nsteps +
          G * systemNoiseState2 + Gcross * systemNoiseCrossState
      }
    }
    
    data$Affect1[row] <- Affect1State #input affect data
    data$Affect2[row] <- Affect2State #input affect data
    data$Time[row] <- times[obsi] #input time data
    data$Subject[row] <- subi #input subject data
    data$TimeTogetherZ[row] <- TimeTogetherZ[subi] #input time together data
  }
}

#data$Affect1 <- data$Affect1 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error
#data$Affect2 <- data$Affect2 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error

# Visualize model generated data
p = ggplot(data[data$Subject==2,], # Plot the data for all couples
           aes(x = Time, y = Affect1, color = as.factor(Subject))) +
  geom_line() +
  geom_line(aes(y = Affect2), linetype = "dashed") +
  geom_point() +
  theme_bw()+
  theme(legend.position = "none")


