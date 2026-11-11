# =============================================================================
#  LECTURE 7 -- SIMULATE DATA (3): NEUROTICISM MODERATES THE KICKS
#  Dynamic Systems Thinking in Psychology
#
#  Every person i follows the model from Lecture 5, with their own parameters:
#
#     dAffect = (A_i * Affect + B_i) dt + G_i dW
#
#    A_i : state dependence: how strongly affect is pulled back to the level
#    B_i : continuous intercept; together with A_i it sets the level = -B_i / A_i
#    G_i : kick strength (process noise)
#
#  Neuroticism N_i is measured ONCE, at the start of the study. It does not
#  change over time: it is a time-independent predictor. In this script it
#  moderates only the KICK STRENGTH G:
#
#     G_i = Gtypical * exp(GeffectN * N_i) * exp(u_i),     u_i ~ Normal(0, Gsd)
#
#  The level and A are the same for everyone.
#
#  Part 1  settings
#  Part 2  make the people: their neuroticism and their parameters
#  Part 3  the simulation function (the one from Lecture 5)
#  Part 4  simulate everyone and save the data
#  Part 5  look at the data
#  Part 6  things to try
#
#  Run the script part by part (select the lines and press Ctrl+Enter).
# =============================================================================

# install.packages("ggplot2")   # run once if you do not have it
library(ggplot2)


# =============================================================================
#  PART 1: SETTINGS
# =============================================================================

set.seed(2026)                     # same random numbers every time you run this

Npeople <- 50                      # number of people
times <- seq(from = 0, to = 28, by = 0.25)  # 4 ratings a day for 4 weeks (time in days)
Nsteps <- 20                       # small steps between two ratings

savePlots <- TRUE                  # TRUE: save every plot in Part 5 as a png file
plotFolder <- "plots_kicks"        # folder for the png files (made in your working directory)

# --- the kick strength G: moderated by neuroticism ------------------------------
# G must stay positive. So we do not ADD an effect of neuroticism, we MULTIPLY
# G by exp(GeffectN * N): exp() of any number is positive, so G stays positive.
#   GeffectN = 0         no effect of neuroticism
#   GeffectN positive    more neuroticism: stronger kicks
Gtypical <- 1                      # kick strength of a person with N = 0
GeffectN <- 0.2                    # 1 SD more neuroticism: G multiplied by exp(0.2) = 1.22 (stronger kicks)
Gsd <- 0.25                        # differences in G that neuroticism does NOT explain (see Part 2)

# --- level and A: the same for everyone -------------------------------------------
levelTypical <- 6                  # level
Atypical <- -0.5                   # state dependence (per day)


# =============================================================================
#  PART 2: MAKE THE PEOPLE
# =============================================================================
# One row per person. We fill in the rows one by one.

people <- data.frame(id = 1:Npeople, Neuroticism = NA, level = NA, A = NA, B = NA, G = NA)

for (i in 1:Npeople) {
  N <- rnorm(1, mean = 0, sd = 1)                     #this person's neuroticism (a z-score)

  level <- levelTypical                               #the same for everyone
  A <- Atypical                                       #the same for everyone
  B <- -A * level                                     #the B that puts the level at 6 (level = -B/A)

  G <- Gtypical * exp(GeffectN * N) *                 #G predicted from neuroticism ...
       exp(rnorm(1, mean = 0, sd = Gsd))              #... times this person's own factor (always positive, around 1)

  people[i, ] <- c(i, N, level, A, B, G)              #store this person's values in row i
}

head(round(people, 2))

# How far affect typically wanders from the level (Lecture 5):
people$longRunSD <- people$G / sqrt(2 * -people$A)
head(round(people, 2))


# =============================================================================
#  PART 3: THE SIMULATION FUNCTION
# =============================================================================
# This is the function from Lecture 5. The only change: A, B and G are now
# given to the function, because every person has their own.

simulateAffect <- function(A, B, G, initialAffect){
  Nobs <- length(times)                        #number of ratings
  Affect <- rep(NA, Nobs)                      #empty vector for the ratings
  for(i in 1:Nobs){
    if(i == 1) Affect[i] <- initialAffect      #first rating: the starting value
    else{
      AffectState <- Affect[i-1]               #start from the previous rating
      dt <- (times[i] - times[i-1]) / Nsteps   #size of each small step
      for(stepi in 1:Nsteps){
        dAffect <- A*AffectState + B           #the rule: where affect is pushed
        kick <- G * sqrt(dt) * rnorm(1)        #random kick, variance G^2 * dt
        AffectState <- AffectState + dAffect * dt + kick
      }
      Affect[i] <- AffectState                 #store the state reached at this rating
    }
  }
  Affect
}


# =============================================================================
#  PART 4: SIMULATE EVERYONE AND SAVE THE DATA
# =============================================================================

dat <- NULL
for (i in 1:Npeople) {
  Affect <- simulateAffect(A = people$A[i], B = people$B[i], G = people$G[i],
                           initialAffect = people$level[i])   #everyone starts at their own level
  dat <- rbind(dat, data.frame(id = i, time = times, Affect = Affect,
                               Neuroticism = people$Neuroticism[i]))  #neuroticism is the same in every row of a person
}
head(dat)        # long format: one row per rating, with columns id, time, Affect and Neuroticism

# write.csv(dat, "lecture07_data_kicks.csv", row.names = FALSE)


# =============================================================================
#  PART 5: LOOK AT THE DATA
# =============================================================================
# Every plot is stored in an object (p5a, p5b_..., p5c_...), shown, and, if
# savePlots is TRUE, saved as a png file in plotFolder.

if (savePlots) dir.create(plotFolder, showWarnings = FALSE)   #make the folder (no warning if it already exists)

# --- 5a. Six people, from lowest to highest neuroticism ---------------------
chosen <- people$id[order(people$Neuroticism)][round(seq(1, Npeople, length.out = 6))]
plotDat <- dat[dat$id %in% chosen, ]
levelDat <- people[people$id %in% chosen, ]
plotDat$who <- paste0("N = ", round(plotDat$Neuroticism, 2))      #panel title: this person's neuroticism
levelDat$who <- paste0("N = ", round(levelDat$Neuroticism, 2))
levelDat <- levelDat[order(levelDat$Neuroticism), ]
plotDat$who <- factor(plotDat$who, levels = levelDat$who)      #panels in order of neuroticism
levelDat$who <- factor(levelDat$who, levels = levelDat$who)

p5a <- ggplot(plotDat, aes(x = time, y = Affect, colour = Neuroticism)) +
  geom_hline(data = levelDat, aes(yintercept = level), linetype = "dashed", colour = "grey50") +
  geom_line() +
  facet_wrap(~ who) +
  scale_colour_gradient2(low = "#2a78d6", mid = "grey60", high = "#b0306a", midpoint = 0) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Affect")
p5a   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5a_six_people.png"), p5a, width = 10, height = 6, dpi = 150)

# Q: What differs between the six people? Compare with the people in the
#    state-dependence script: can you tell from the picture alone whether
#    someone is kicked harder or pulled back more weakly?

# --- 5b. The true G against neuroticism ----------------------------------------
# (In a real study you never see this: you have to estimate it.)

p5b_G <- ggplot(people, aes(x = Neuroticism, y = G)) +
  geom_point(size = 2.5) +
  geom_function(fun = function(N) Gtypical * exp(GeffectN * N)) +   #the curve the G's come from
  theme_bw(base_size = 16) +
  labs(x = "Neuroticism (z-score)", y = "Kick strength G")
p5b_G   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5b_true_G.png"), p5b_G, width = 7, height = 5, dpi = 150)

# --- 5c. What neuroticism does to the ratings ---------------------------------
# Three simple summaries of each person's ratings:
#   mean            where the ratings are on average
#   SD              how much they vary
#   autocorrelation how similar two ratings 6 hours apart are

perPerson <- data.frame(id = 1:Npeople, Neuroticism = people$Neuroticism,
                        mean = NA, SD = NA, autocorrelation = NA)
for (i in 1:Npeople) {
  y <- dat$Affect[dat$id == i]                                     #this person's ratings
  perPerson$mean[i] <- mean(y)
  perPerson$SD[i] <- sd(y)
  perPerson$autocorrelation[i] <- cor(y[-length(y)], y[-1])        #each rating next to the following one
}
head(round(perPerson, 2))

p5c_mean <- ggplot(perPerson, aes(x = Neuroticism, y = mean)) +
  geom_point(size = 2.5) + theme_bw(base_size = 16) +
  labs(x = "Neuroticism (z-score)", y = "Mean of the ratings")
p5c_mean   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5c_mean.png"), p5c_mean, width = 7, height = 5, dpi = 150)

p5c_SD <- ggplot(perPerson, aes(x = Neuroticism, y = SD)) +
  geom_point(size = 2.5) + theme_bw(base_size = 16) +
  labs(x = "Neuroticism (z-score)", y = "SD of the ratings")
p5c_SD   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5c_SD.png"), p5c_SD, width = 7, height = 5, dpi = 150)

p5c_autocorrelation <- ggplot(perPerson, aes(x = Neuroticism, y = autocorrelation)) +
  geom_point(size = 2.5) + theme_bw(base_size = 16) +
  labs(x = "Neuroticism (z-score)", y = "Autocorrelation (6 hours apart)")
p5c_autocorrelation   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5c_autocorrelation.png"), p5c_autocorrelation, width = 7, height = 5, dpi = 150)

# Q: Which of the three summaries is related to neuroticism? Compare with the
#    state-dependence script: which summary tells "harder kicks" apart from
#    "weaker pull"?


# =============================================================================
#  PART 6: THINGS TO TRY
# =============================================================================
# 1. Make neuroticism the ONLY reason people differ in G: set Gsd <- 0 and
#    run Parts 2 to 5 again. What happens in 5b?
#
# 2. Make the kicks much stronger for everyone: Gtypical <- 3. Does the
#    autocorrelation change? Should it?
#
# 3. Add measurement error to the ratings, as in Lecture 5:
#       dat$Affect <- dat$Affect + rnorm(nrow(dat), mean = 0, sd = 1)
#    and rerun 5c. The SD still rises with neuroticism. Why could a researcher
#    who ignores measurement error now draw the wrong conclusion about how
#    strongly neuroticism moderates the kicks?
