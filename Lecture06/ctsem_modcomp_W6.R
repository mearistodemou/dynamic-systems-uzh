############################
# Fitting models with ctsem
# Michael E. Aristodemou
# Week 5: 16.10.2025
#############################

# install package if it is missing
if (!requireNamespace(c("ggplot2","patchwork","curl","parallel"), quietly = TRUE)) {
  install.packages(c("ggplot2","patchwork","curl","parallel"))
}

library(ggplot2) # for making plots
library(ctsem) # load ctsem for modeling
library(patchwork) # for stiching plots together
library(curl)
library(parallel) # load package to detect cores

# link to data on github
url <- "https://raw.githubusercontent.com/mearistodemou/dynamic-systems-uzh/main/Lecture6/groundtruth_w6.rds"

# functions to load data from R
con <- gzcon(curl::curl(url, "rb"))
on.exit(close(con), add = TRUE)

data <- readRDS(con) # load file


################################
# Specify four competing models
################################

# Model 1: Unidirectional model (2 processes, 2 indicators, unidirectional causation)
ct_unidirectional <- ctModel( #define the ctsem model
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
  CINT=0, #continuous intercept with *no* random effects
  T0MEANS=c('initialAffect1||FALSE','initialAffect2||FALSE'), #initial affect with random effects
  DRIFT = c(
    'auto1', 0, #auto effect for subj 1 and cross-effect from 2 to 1
    'cross21','auto2' ), #cross-effect from 1 to 2 and auto effect for subj 2
  DIFFUSION = c(
    'systemNoise1', 0, #system noise for subj 1, 0 in upper triangle (correlation only needs 1 par)
    0, 'systemNoise2')) #correlation in system noise, and sd for subj 2
#ctModelLatex(ct_unidirectional) #generate LaTeX representation of the model


# Model 2: Bidirectional model (2 processes, 2 indicators, bidirectional causation)
ct_bidirectional <- ctModel( #define the ctsem model
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
  CINT=0, #continuous intercept with *no* random effects
  T0MEANS=c('initialAffect1||FALSE','initialAffect2||FALSE'), #initial affect with random effects
  DRIFT = c(
    'auto1', 'cross12', #auto effect for subj 1 and cross-effect from 2 to 1
    'cross21','auto2' ), #cross-effect from 1 to 2 and auto effect for subj 2
  DIFFUSION = c(
    'systemNoise1', 0, #system noise for subj 1, 0 in upper triangle (correlation only needs 1 par)
    0, 'systemNoise2')) #correlation in system noise, and sd for subj 2
#ctModelLatex(ct_bidirectional) #generate LaTeX representation of the model


# Model 3: Common cause model (2 processes, 2 indicators, one unobserved cause)
ct_commoncause <- ctModel( #define the ctsem model
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
  CINT=0, #continuous intercept with *no* random effects
  T0MEANS=c('initialAffect1||FALSE','initialAffect2||FALSE'), #initial affect with random effects
  DRIFT = c(
    'auto1', 0, #auto effect for subj 1 and cross-effect from 2 to 1
    0,'auto2' ), #cross-effect from 1 to 2 and auto effect for subj 2
  DIFFUSION = c(
    'systemNoise1', 0, #system noise for subj 1, 0 in upper triangle (correlation only needs 1 par)
    'systemNoiseCross', 'systemNoise2')) #correlation in system noise, and sd for subj 2
#ctModelLatex(ct_commoncause) #generate LaTeX representation of the model


# Model 4: Multiple indicators 1 process (1 process, 2 indicators)
ct_indicator <- ctModel( #define the ctsem model
  manifestNames = c("Affect1",'Affect2'), #names of observed variables in dataset
  latentNames = c("Affect1"), #names of latent processes
  time = 'Time', #name of time column in dataset
  id = 'Subject', #name of subject column in dataset
  type='ct', #use continuous time / differential equation model (dt for discrete-time)
  MANIFESTVAR = c(
    'residualSD1',0,
    0, 'residualSD2'), #sd of the residual / measurement error
  LAMBDA = matrix(c(1, "lambda1"), nrow = 2, ncol = 1), #relating latent process to observed variables
  MANIFESTMEANS=c(0, "manif1"), #no measurement intercept (1 observed variable relates directly to latent)
  CINT=0, #continuous intercept with *no* random effects
  T0MEANS=c('initialAffect1||FALSE'), #initial affect with random effects
  DRIFT = 'auto1', #cross-effect from 1 to 2 and auto effect for subj 2
  DIFFUSION = 'systemNoise1') #correlation in system noise, and sd for subj 2
#ctModelLatex(ct_indicator) #generate LaTeX representation of the model

###############################
# Fit competing models
###############################

ncore = detectCores() # detect number of cores (to speed-up computation)

# fit model 1: unidirectional
ct_fit_uni <- ctStanFit(datalong = data, # the name of our dataset
                    ctstanmodel = ct_unidirectional, # the name of our model
                    cores = ncore) #fit the model to our data

# fit model 2: bidirectional
ct_fit_bi <- ctStanFit(datalong = data, # the name of our dataset
                    ctstanmodel = ct_bidirectional, # the name of our model
                    cores = ncore) #fit the model to our data

# fit model 3: common cause
ct_fit_cc <- ctStanFit(datalong = data, # the name of our dataset
                       ctstanmodel = ct_commoncause, # the name of our model
                       cores = ncore) #fit the model to our data

# fit model 3: indicators
ct_fit_ind <- ctStanFit(datalong = data, # the name of our dataset
                       ctstanmodel = ct_indicator, # the name of our model
                       cores = ncore) #fit the model to our data

##################################################
# Script to summarize all fit indices in a table
##################################################

# Collect fits in a named list
fits <- list(
  unidirectional = ct_fit_uni,
  bidirectional  = ct_fit_bi,
  common_cause   = ct_fit_cc,
  indicators     = ct_fit_ind
)

# Get a summary of each model fit
sums <- lapply(fits, summary, parmatrices = FALSE)

# Function to extract the estimates we want
get_field_num <- function(s, candidates) {
  for (nm in candidates) if (!is.null(s[[nm]])) return(as.numeric(s[[nm]]))
  return(NA_real_)
}

# use the above function for each fit
aics    <- sapply(sums, get_field_num, candidates = c("aic","AIC")) # extract AIC
logliks <- sapply(sums, get_field_num, candidates = c("loglik","logLik")) # extract log-likelihood

# Build comparison table
aic_tbl <- data.frame(
  model   = names(aics),
  AIC     = as.numeric(aics),
  logLik  = as.numeric(logliks),
  row.names = NULL
)

# Sort by AIC (lower is better), add deltaAIC and Akaike weights
aic_tbl <- aic_tbl[order(aic_tbl$AIC), ]
aic_tbl$deltaAIC <- aic_tbl$AIC - min(aic_tbl$AIC, na.rm = TRUE)
aic_tbl$akaikeWeight <- with(aic_tbl, exp(-0.5 * deltaAIC) / sum(exp(-0.5 * deltaAIC)))

print(aic_tbl) # print table in output terminal

########################################
# Impulse response plots
# You don't need to run this in class
########################################

# Impulse response function
p1 = ctStanDiscretePars(ct_fit_uni,plot=T) +
  theme()# plot dynamics for unidirectional model

p2 = ctStanDiscretePars(ct_fit_bi,plot=T) +
  theme()# plot dynamics for bidirectional model

p3 = ctStanDiscretePars(ct_fit_cc,plot=T) +
  theme()# plot dynamics for cc model

p4 = ctStanDiscretePars(ct_fit_ind,plot=T) +
  theme()# plot dynamics for ind model

#  Add titles to each subplot
p1 <- p1 + ggtitle("Unidirectional")
p2 <- p2 + ggtitle("Bidirectional")
p3 <- p3 + ggtitle("Common cause")
p4 <- p4 + ggtitle("Indicators")

# 2x2 grid
grid_2x2 <- (p1 | p2) /
  (p3 | p4)

# Add a title to the overall plot
grid_2x2 <- grid_2x2 + plot_annotation(
  title = "Impulse Response Functions",
)

# Print to viewer
grid_2x2
