################################################
# ctsem specification
# Michael E. Aristodemou
# Week 4
################################################

library(ctsem)
library(ggplot2)
library(data.table)

# Generate data for multiple subjects with individual differences
NSubjects <- 20
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

# -- Specify ctsem model ----------------------------------------------------------

# Fit continuous time structural equation model
ct_model <- ctModel( #define the ctsem model
  # Specify features of the data
  manifestNames = "Affect", #names of observed variables in dataset
  latentNames = "Affect", #names of latent processes
  time = 'Time', #name of time column in dataset
  id = 'Subject', #name of subject column in dataset
  type='ct', #use continuous time / differential equation model (dt for discrete-time)
  # Specify features of the model
  MANIFESTVAR = 'residualSD', #sd of the residual / measurement error
  LAMBDA = matrix(1,nrow=1,ncol=1), #relating latent process to observed variables
  MANIFESTMEANS=0, #no measurement intercept (1 observed variable relates directly to latent)
  CINT='B||FALSE', #continuous intercept with *no* random effects
  T0MEANS='initialAffect||TRUE', #initial affect with random effects
  DRIFT = 'stateDependence',
  DIFFUSION = 'systemNoise')

# -- Fit mode with ctsem ---------------------------------------------------------------
ct_fit <- ctStanFit(datalong = data, ctstanmodel = ct_model) #fit the model to our data
summary(ct_fit, parmatrices= FALSE) # print summary of the fit, some output disabled

# -- Visualize fit to data -------------------------------------------------------------
fit <- ctStanGenerateFromFit(ct_fit, nsamples=200, #add generated data to the fit object
                             fullposterior = F, cores = 2)
gendat <- as.data.table(fit$generated$Y) #extract generated data
gendat[,row:=as.integer(row)] #set the row column type appropriately
truedat <- melt(data.table(row=1:nrow(data),data), #get original data in melted form, ready for merging
                measure.vars = 'Affect', variable.name = 'V1',value.name = 'TrueValue')
gendat <- merge(gendat,truedat,by = c('row','V1')) #merge original and melted data, ready for plotting

ggplot(gendat,aes(y=value,x=Time))+ 
  stat_summary(fun.data=mean_sdl,geom='ribbon',alpha=.3,fill='red')+
  stat_summary(aes(y = value), fun.y=mean, colour="red", geom="line",size=1)+
  stat_summary(aes(y = TrueValue), fun.y=mean, geom="line",linetype='dashed',size=1)+
  geom_line(aes(y=TrueValue,group=Subject),alpha=.2)+
  facet_wrap(vars(V1),scales = 'free')+
  theme_bw()
