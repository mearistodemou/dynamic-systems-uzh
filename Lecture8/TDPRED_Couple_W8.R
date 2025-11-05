#################################################
# Turning coupling on and off based on presence
# Michael Aristodemou
# Week 8: 06.11.2025
#################################################

# Generate data for multiple subjects with individual differences
NSubjects <- 20
times <- seq(from=0, to=40, by=1) #generate sequence of time points when subjects are measured
Nobs <- length(times) #number of observations per subject

initialAffect1 <- rnorm(n = NSubjects, mean = 5, sd = 2)
initialAffect2 <- rnorm(n = NSubjects, mean = 5, sd = 2)

TimeTogether <- runif(n=NSubjects, min = 0, max = 20) #time together in years
TimeTogetherZ <- scale(TimeTogether) #standardise time together

A <- -.4 #continuous time state dependence
Across <- rnorm(n= NSubjects, mean = .1, sd=.05) #cross-effect state dependence

# Sample from a binomial distribution with prob based on time spent together
Comm <- lapply(TimeTogether, function(t) rbinom(n = Nobs, size = 1, prob = t / 20))
# Check the relationship: compute the mean interaction for each subject and correlate with TimeTogetherZ
Comm_means <- sapply(Comm, mean)
print(cor(Comm_means, TimeTogetherZ))

# generate continuous intercepts for each individual,
# assuming high affect individuals may partner with high affect individuals
Bcommon <- rnorm(n = NSubjects, mean = 1, sd = .2) #common continuous intercept variance
B1 <- Bcommon + rnorm(n = NSubjects, mean = 2, sd = .3) #unique continuous intercept for subj 1
B2 <- Bcommon + rnorm(n = NSubjects, mean = 2, sd = .3) #continuous intercept for subj 2

cor(B1,B2) #check if people with higher affect tend to couple

G <- .4 #unique system noise coefficient
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
    comm_val <- Comm[[subi]][obsi]
    if(obsi==1){
      Affect1State <- initialAffect1[subi] #if first time point, set to initial affect
      Affect2State <- initialAffect2[subi]
    }
    if(obsi>1){ #else compute new affect state by taking a sequence of small steps in time
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        #compute deterministic slope of affect at earlier time point
        dAffect1 <- A*Affect1State + Across[subi] * Affect2State * comm_val + B1[subi]
        dAffect2 <- A*Affect2State + Across[subi] * Affect1State * comm_val + B2[subi]
        systemNoiseState1 <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #unique noise for subj 1
        systemNoiseState2 <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #unique noise for subj 2
        systemNoiseCrossState <- rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) #common noise for both individuals
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
    data$Comm[row] <- comm_val
    data$Subject[row] <- subi #input subject data
    data$TimeTogetherZ[row] <- TimeTogetherZ[subi] #input time together data
    data$const[row] <- 1 # add constant for ctsem spec
  }
}

data$Affect1 <- data$Affect1 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error
data$Affect2 <- data$Affect2 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error

ggplot(data, # Plot the data for all couples
       aes(x = Time, y = Affect1, color = as.factor(Subject))) +
  geom_line() +
  geom_line(aes(y = Affect2), linetype = "dashed") +
  geom_point() +
  theme_bw()+
  theme(legend.position = "none")

