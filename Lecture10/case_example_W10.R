#######################################
# Treatment under ideal conditions
# Michael E. Aristodemou
# Week 10: 20.11.2025
#######################################

## --- Preamble ------------------------------------------------------------
# install package if it is missing
if (!requireNamespace(c("ggplot2","dplyr","tidyr"), quietly = TRUE)) {
  install.packages(c("ggplot2","dplyr","tidyr"))
}

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# -- Define data characteristics ----------------------------------------------
NSubjects <- 1 # only 1 dog
times <- seq(from=0, to=200, by=1) # create a sequence of 100 time points
Nobs <- length(times) #number of observations per subject

## --- Define variables -------------------------------------------------------

# Allergens
A1 = -0.1 # natural decay rate of allergen levels
G = 0 # unobserved influences on allergen levels

# Protein food
Pinput = 1 # food based input of allergens (start the protein input at 1)

# Elimination diet
Pelim = -1 # reduce protein input to 0, cancel out the 1 given by the food
inputTime <- times[ceiling(length(times)/4)] # intervention after ~ 1/4 of time window passed
AM <- 0 # natural decay of intervention effect

# Starting amount of allergens in system
initialAllergen <- rnorm(n = NSubjects, mean = 0, sd = 0)

#create empty data.frame to fill step by step
data <- data.frame(Subject= rep(NA,NSubjects*Nobs),
                   Time = rep(NA,NSubjects*Nobs),
                   Allergen = rep(NA,NSubjects*Nobs))

Nsteps <- 100 #number of steps in time to compute between each observation (increased precision)
row <- 0 #initialize row counter, to track which row of the data.frame we are on
for(subi in 1:NSubjects){
  for(obsi in 1:Nobs){ #for each observation of a subject
    row <- row + 1
    if(obsi==1){
      AllergenState <- initialAllergen[subi] #if first time point, set to initial affect
      InputState <- Pinput # initialize latent input process
    }
    if(obsi>1){ #else compute new affect state by taking a sequence of small steps in time
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        # Compute intervention effect
        dInput <- AM*InputState # deterministic slope of our intervention at earlier time point
        if(times[obsi]==inputTime) dInput <- dInput + Pelim # add input effect if intervention time
        InputState <- InputState + dInput * 1/Nsteps # update state using slope and time step
        # Compute Affect state
        dAllergen <- A1*AllergenState + (InputState) # deterministic slope at earlier time point
        AllergenState <- AllergenState + dAllergen * 1/Nsteps + # update state using slope and time step
          G * rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) # and add system noise
      }
    }
    data$Allergen[row] <- AllergenState #input affect data
    data$Time[row] <- times[obsi] #input time data
    data$Subject[row] <- subi #input subject data
    data$Input <- ifelse(data$Time == inputTime, 1, 0) #input effect data
    data$InputState[row] <- InputState #input latent intervention process data
  }
}

data$Allergen <- data$Allergen + rnorm(n=nrow(data), mean = 0, sd = 0) #add measurement error

# -- plots -------------
plot_data <- data %>%
  select(Subject, Time, Allergen, InputState) %>%
  pivot_longer(c(Allergen, InputState), names_to = "Series", values_to = "Value")

p1=ggplot(plot_data, aes(x = Time, y = Value, color = Series)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  facet_grid(rows = vars(Series),
             scales = "free_y",
             labeller = as_labeller(c(Allergen = "Allergen",
                                      InputState = "Protein Input"))) +
  geom_vline(xintercept = inputTime, linetype = "dashed") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none") +
  labs(x = "Time (Days)", y = NULL)
p1
