####################################################
# Adding a time-independent moderator
# Michael E. Aristodemou
# Week 7: 30.10.2025
####################################################

# install package if it is missing
if (!requireNamespace(c("ggplot2"), quietly = TRUE)) {
  install.packages(c("ggplot2"))
}

library(ggplot2)
library(ctsem)

# Generate data for multiple subjects with individual differences
NSubjects <- 20
times <- seq(from=0, to=40, by=1) #generate sequence of time points when subjects are measured
Nobs <- length(times) #number of observations per subject

initialAffect1 <- rnorm(n = NSubjects, mean = 5, sd = 2)
initialAffect2 <- rnorm(n = NSubjects, mean = 5, sd = 2)

TimeTogether <- runif(n=NSubjects, min = 0, max = 20) #time together in years
TimeTogetherZ <- scale(TimeTogether) #standardise time together

A <- -.4 #continuous time state dependence
Across <- rnorm(n= NSubjects, mean = .1, sd=.05) + .05*TimeTogetherZ #cross-effect state dependence

# generate continuous intercepts for each individual,
# assuming couples tend to achieve similar affect levels by the end of therapy
Bcommon <- rnorm(n = NSubjects, mean = 1, sd = .2) #common continuous intercept variance
B1 <- Bcommon + rnorm(n = NSubjects, mean = 2, sd = .3) #unique continuous intercept for subj 1
B2 <- Bcommon + rnorm(n = NSubjects, mean = 2, sd = .3) #continuous intercept for subj 2
cor(B1,B2) #check if people with higher end-of-therapy affect tend to be in a relationship

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
    if(obsi==1){
      Affect1State <- initialAffect1[subi] #if first time point, set to initial affect
      Affect2State <- initialAffect2[subi]
    }
    if(obsi>1){ #else compute new affect state by taking a sequence of small steps in time
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        #compute deterministic slope of affect at earlier time point
        dAffect1 <- A*Affect1State + Across[subi] * Affect2State + B1[subi]
        dAffect2 <- A*Affect2State + Across[subi] * Affect1State + B2[subi]
        
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

data$Affect1 <- data$Affect1 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error
data$Affect2 <- data$Affect2 + rnorm(n=nrow(data), mean = 0, sd = .05) #add measurement error

# Visualize model generated data
ggplot(data, # Plot the data for all couples
       aes(x = Time, y = Affect1, color = as.factor(Subject))) +
  geom_line() +
  geom_line(aes(y = Affect2), linetype = "dashed") +
  geom_point() +
  theme_bw()+
  theme(legend.position = "none")


##########################################
# Fitting a ctsem model
##########################################

# Fit continuous time structural equation model
ct_model <- ctModel( #define the ctsem model
  tipredDefault = FALSE, #moderation disabled unless explicitly specified
  TIpredNames = c('TimeTogetherZ'), #names of time independent predictors in dataset
  manifestNames = c("Affect1",'Affect2'), #names of observed variables in dataset
  latentNames = c("Affect1",'Affect2'), #names of latent processes
  time = 'Time', #name of time column in dataset
  id = 'Subject', #name of subject column in dataset
  type='ct', #use continuous time / differential equation model (dt for discrete-time)
  MANIFESTVAR = c(
    'residualSD1',0,
    0, 'residualSD2'), #sd of the residual / measurement error
  LAMBDA = diag(1,2), #relating latent process to observed variables
  MANIFESTMEANS=0, #no measurement intercept (1 observed variable relates directly to latent)
  CINT=c('B1||TRUE','B2||TRUE'), #continuous intercept with random effects
  T0MEANS=c('initialAffect1||TRUE','initialAffect2||TRUE'), #initial affect with random effects
  DRIFT = c(
    'auto1', 'cross21||TRUE||TimeTogetherZ', #auto effect for subj 1 and cross-effect from 2 to 1
    'cross21||TRUE||TimeTogetherZ','auto2' ), #cross-effect from 1 to 2 and auto effect for subj 2
  DIFFUSION = c(
    'systemNoise1', 0, #system noise sd for subj 1, 0 in upper triangle (1 par for correlation)
    'systemNoiseCross', 'systemNoise2')) #correlation in system noise, and sd for subj 2 noise

ct_fit <- ctStanFit(datalong = data, ctstanmodel = ct_model) #fit the model to our data
tipredplots <- ctPredictTIP(ct_fit, plot=T, tipreds = c('TimeTogetherZ')) # plot moderation for 3 discrete values of the moderator
