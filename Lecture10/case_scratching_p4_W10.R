##########################################
# From latents to observables
# Michael E. Aristodemou
# Week 10: 20.11.2025
##########################################

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
A1 = -0.05 # natural decay rate of allergen levels
G = 0 # unobserved influences on allergen levels

# Eating protein food
Pinput = 3 # food based constant input of allergens

# Elimination diet
Pelim = -3
inputTime <- times[ceiling(length(times)/4)] # intervention after ~ 1/4 of time window passed
AM = 0 # natural decay of intervention effect

# Lavender eating
Linput = 4 # amount of allergens in system due to lavender
Al = 0.05 # accumulation of allergens with repeated lavender eating
lavStart <- times[ceiling(length(times)/3.5)]  # start at ~1/3.5 of time window
lavEnd   <- times[floor(length(times)/3)]      # end at ~1/3 of time window

# Random protein input
Walks <- 6 # increase the probability of snacking (e.g. going on more walks to the park)
# Sample from a binomial distribution with prob based on number of walks
snacking <- lapply(Walks, function(t) rbinom(n = Nobs, size = 1, prob = t / 20))
# Compute the average amount of snacking across days
snack_mean <- sapply(snacking, mean)
# scale effect of snacking
AS = 1 # amount of protein intake
k = 10 # absorption rate

# Urge to scratch
AEffect = 1 # effect of allergen amount on urge to scratch
G_urge = 50 # random fluctuations in urge to scratch

# Scratching
alpha0 <- -10   # baseline log-odds
alpha1 <- 0.6  # effect of UrgeState

# Starting amount of allergens in system
initialAllergen <- rnorm(n = NSubjects, mean = 0, sd = 0)

#create empty data.frame to fill step by step
data <- data.frame(Subject= rep(NA,NSubjects*Nobs),
                   Time = rep(NA,NSubjects*Nobs),
                   Allergen = rep(NA,NSubjects*Nobs),
                   Urge = rep(NA,NSubjects*Nobs))

Nsteps <- 100 #number of steps in time to compute between each observation (increased precision)
row <- 0 #initialize row counter, to track which row of the data.frame we are on
for(subi in 1:NSubjects){
  for(obsi in 1:Nobs){ #for each observation of a subject
    row <- row + 1
    snack_val <- snacking[[subi]][obsi]
    if(obsi==1){
      AllergenState <- initialAllergen[subi] #if first time point, set to initial affect
      InputState <- Pinput # initialize latent input process
      LavenderState <- 0 # initialize latent toxin process
      UrgeState <- 0 # initialize latent urge process
      pScratch <- plogis(alpha0 + alpha1 * UrgeState)
    }
    if(obsi>1){ #else compute new affect state by taking a sequence of small steps in time
      for(stepi in 1:Nsteps){ #take Nsteps in time between each observation
        # Compute intervention effect
        dInput <- AM*InputState # deterministic slope of our intervention at earlier time point
        dLavender <- Al*LavenderState
        if (times[obsi] > inputTime) {
          dSnack <- k * (AS * snack_val - InputState)
        } else {
          dSnack <- 0
        }
        if(times[obsi]==inputTime) dInput <- dInput + Pelim # add input effect if intervention time
        if(times[obsi]==lavStart) dLavender <- dLavender + Linput
        if(times[obsi]>=lavEnd) dLavender <- dLavender*-5
        InputState <- InputState + dInput * 1/Nsteps + dSnack * 1/Nsteps # update state using slope and time step
        LavenderState <- LavenderState + dLavender * 1/Nsteps # update state using slope and time step
        
        # Compute Allergen state
        dAllergen <- A1*AllergenState + InputState + LavenderState # deterministic slope at earlier time point
        AllergenState <- AllergenState + dAllergen * 1/Nsteps + # update state using slope and time step
          G * rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) # and add system noise
        
        # Compute Urge state
        dUrge <- dAllergen # deterministic slope at earlier time point
        UrgeState <- AllergenState + dUrge * 1/Nsteps + # update state using slope and time step
          G_urge * rnorm(n=1, mean=0, sd=sqrt(1/Nsteps)) # and add system noise
        
        # Probability of scratching
        pScratch <- plogis(alpha0 + alpha1 * UrgeState)
      }
    }
    data$Allergen[row] <- AllergenState #input allergen data
    data$Urge[row] <- UrgeState #input urge data
    data$Time[row] <- times[obsi] #input time data
    data$Subject[row] <- subi #input subject data
    data$Input <- ifelse(data$Time == inputTime, 1, 0) #input effect data (elimination diet)
    data$LavInput <- ifelse(data$Time == lavStart, 1, 0) #input effect data (lavender consumption)
    data$InputState[row] <- InputState #input latent intervention process data
    data$LavenderState[row] <- LavenderState #input latent intervention process data
    data$Snack[row] <- snack_val # input effect data (random snacking)
    data$Scratch[row] <- rbinom(1, size = 1, prob = pScratch)
    data$probScratch[row] <- pScratch
  }
}

# -- plots ---------------------------------------------------------
plot_data <- data %>%
  select(Subject, Time, Allergen, InputState, LavenderState, Urge, Scratch) %>%
  pivot_longer(c(Allergen, InputState, LavenderState, Urge, Scratch),
               names_to = "Series",
               values_to = "Value")

p1 <- ggplot(plot_data, aes(x = Time, y = Value, color = Series)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  facet_grid(rows = vars(Series),
             scales = "free_y",
             labeller = as_labeller(c(
               Urge = "Urge to Scratch",
               Scratch = "Scratching",
               Allergen = "Allergen",
               InputState = "Protein Input",
               LavenderState = "Lavender Toxins"
             ))) +
  geom_vline(xintercept = inputTime, linetype = "dashed") +
  geom_vline(xintercept = lavStart,  linetype = "dotted") +
  geom_vline(xintercept = lavEnd,    linetype = "dotted") +
  theme_bw() +
  labs(x = "Time (Days)", y = NULL, color = "Variable")
p1

## -- Measurement link -------------------------------------
p_rel <- ggplot(data, aes(x = Urge, y = probScratch)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +   # raw 0/1 points
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE) +                               # logistic fit
  theme_bw(base_size = 24) +
  labs(x = "Urge to scratch",
       y = "Probability of scratching",
       title = "Relationship between Urge and Scratching")

p_rel
