# =============================================================================
#  LECTURE 6 -- SIMULATE AFFECT WITH PROCESS NOISE, THEN FIT IT WITH ctsem
#  Dynamic Systems Thinking in Psychology
#
#  The model:   dy = (A * y + B) dt + G dW
#    A : how strongly affect is pulled back to its equilibrium (per week)
#    B : the constant push (affect units per week); equilibrium = -B/A
#    G : how hard affect is shaken by random kicks (process noise)
#
#  Part 1  simulate affect for a few people, with small steps
#  Part 2  look at the data
#  Part 3  write the same model down for ctsem and fit it
#  Part 4  compare the estimates with the true values
#  Part 5  things to try
#
#  Run the script part by part (select the lines and press Ctrl+Enter).
# =============================================================================

# install.packages(c("ctsem", "ggplot2"))   # run once if you do not have them
library(ctsem)
library(ggplot2)

# =============================================================================
#  PART 1: SIMULATE
# =============================================================================

set.seed(2026)                     # same random numbers every time you run this

A <- -0.2                          # pull back to the equilibrium (per week)
B <- 2                             # constant push (per week): equilibrium = -B/A = 10
G <- 1.5                           # strength of the random kicks (process noise)
initialAffect <- 3                 # everyone starts at affect 3

times <- seq(from = 0, to = 20, by = 1/7)   # one rating per day for 20 weeks (time in weeks)
Nsteps <- 20                       # small steps between two ratings
Npeople <- 10                      # number of people (all with the same A, B and G)

# This is the function from Lecture 4, with ONE extra term: the random kick.
simulateAffect <- function(Nsteps){
  Nobs <- length(times)                        #number of ratings
  Affect <- rep(NA, Nobs)                      #empty vector for the ratings
  for(i in 1:Nobs){
    if(i == 1) Affect[i] <- initialAffect      #first rating: the starting value
    else{
      AffectState <- Affect[i-1]               #start from the previous rating
      dt <- (times[i] - times[i-1]) / Nsteps   #size of each small step
      for(stepi in 1:Nsteps){
        dAffect <- A*AffectState + B           #the rule: where affect is pushed
        kick <- G * sqrt(dt) * rnorm(1)        #NEW: random kick, variance G^2 * dt
        AffectState <- AffectState + dAffect * dt + kick
      }
      Affect[i] <- AffectState                 #store the state reached at this rating
    }
  }
  Affect
}

# simulate every person and put them below each other in one data frame
dat <- NULL
for (person in 1:Npeople) {
  dat <- rbind(dat, data.frame(id = person, time = times, Affect = simulateAffect(Nsteps)))
}
head(dat)        # long format: one row per rating, with columns id, time and Affect


# =============================================================================
#  PART 2: LOOK AT THE DATA
# =============================================================================

ggplot(dat, aes(x = time, y = Affect, group = id)) +
  geom_line(colour = "grey40", alpha = 0.6) +
  geom_hline(yintercept = -B/A, linetype = "dashed", colour = "darkgreen") +  #equilibrium
  theme_bw(base_size = 16) +
  labs(x = "Time (weeks)", y = "Affect")

# Q: Where do the trajectories go? Why do they not settle on the dashed line?


# =============================================================================
#  PART 3: THE SAME MODEL IN ctsem
# =============================================================================
# ctsem writes the model as   d eta = (DRIFT * eta + CINT) dt + DIFFUSION dW
# (eta is the latent, true affect) plus a measurement part
#   Affect = LAMBDA * eta + MANIFESTMEANS + measurement error (MANIFESTVAR).
# So: DRIFT is our A, CINT is our B, and DIFFUSION is our G.
# A text such as "drift" means: estimate this parameter. A number means: fix it.

model <- ctModel(
  type          = "ct",                # continuous-time model
  manifestNames = "Affect",            # the column with the ratings
  latentNames   = "AffectState",       # the true affect behind the ratings
  LAMBDA        = matrix(1),           # rating = true affect (+ error)
  DRIFT         = matrix("drift"),     # A: estimate
  CINT          = matrix("cint"),      # B: estimate
  DIFFUSION     = matrix("diffusion"), # G: estimate
  MANIFESTMEANS = matrix(0),           # no extra offset in the ratings
  MANIFESTVAR   = matrix(0),           # no measurement error (our data have none)
  T0MEANS       = matrix(initialAffect), # affect at the first rating: we know it is 3
  T0VAR         = matrix(0))           # everyone starts at exactly that value

# Everyone was simulated with the same A, B and G, so we switch off
# person-specific differences in the parameters (i.e. no random effects).
model$pars$indvarying <- FALSE

fit <- ctStanFit(datalong = dat, model = model,
                 optimize = TRUE,      # quick maximum-likelihood style estimation
                 priors = FALSE,       # no priors: plain maximum likelihood
                 cores = 2) # number of cores to use for estimation

# =============================================================================
#  PART 4: COMPARE WITH THE TRUE VALUES
# =============================================================================

summary(fit)$popmeans                  # estimates with standard errors (sd) and 95% intervals

est <- summary(fit)$popmeans
comparison <- data.frame(
  parameter = c("A (drift)", "B (cint)", "G (diffusion)"),
  true      = c(A, B, G),
  estimate  = round(est[c("drift", "cint", "diffusion"), "mean"], 3),
  lower95   = round(est[c("drift", "cint", "diffusion"), "2.5%"], 3),
  upper95   = round(est[c("drift", "cint", "diffusion"), "97.5%"], 3))
comparison
# The estimates will not hit the true values exactly: the data contain random
# kicks, so every new simulation gives slightly different estimates. The true
# value should usually lie inside the 95% interval.

# Two things you can compute from the estimates:
Ahat <- est["drift", "mean"]; Bhat <- est["cint", "mean"]
-Bhat / Ahat                           # estimated equilibrium (true: 10)

# And the discrete-time AR coefficient this implies for daily and for weekly data:
exp(Ahat * 1/7)                        # AR(1) for one day apart
exp(Ahat * 1)                          # AR(1) for one week apart

# ctsem can plot that AR coefficient for every interval length: how much of a
# deviation from the equilibrium is still there after 1, 2, ... weeks.
ctStanDiscretePars(fit, plot = TRUE)


# =============================================================================
#  PART 5: THINGS TO TRY
# =============================================================================
#
# 1. Irregular ratings. Give every rating a random time:
#       times <- sort(c(0, runif(140, 0, 20)))
#    Simulate and fit again. ctsem uses the actual time between two ratings.
#
# 2. Add measurement error on top of the simulated ratings:
#       dat$Affect <- dat$Affect + rnorm(nrow(dat), 0, 2)
#    a) Fit the model above (which assumes no measurement error). What happens
#       to the estimates of A and G?
#    b) Now let ctsem estimate the measurement error as well, by changing
#       MANIFESTVAR = matrix(0) into MANIFESTVAR = matrix("merror").
#       Can it separate the measurement error from the process noise?
