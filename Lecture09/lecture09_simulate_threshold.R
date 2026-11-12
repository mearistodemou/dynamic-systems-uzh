# =============================================================================
#  LECTURE 9 -- SIMULATE DATA: A MODERATOR THAT SWITCHES ON BELOW A THRESHOLD
#  Dynamic Systems Thinking in Psychology
#
#  The model from Lecture 5, with one extra term:
#
#     dMood = (A * Mood + B + Beffect * moderator) dt + G dW
#
#     moderator = 1 while mood is BELOW the threshold (here: 50)
#     moderator = 0 while mood is ABOVE the threshold
#
#  Example: when mood drops below 50, a person starts to ruminate, and
#  rumination pushes mood further down (Beffect negative). Above 50 the
#  rumination stops.
#
#  The moderator depends on mood itself, which changes all the time. So it is
#  checked in EVERY small step, not only at the observations. This makes the
#  model nonlinear: there are two rules for change, one below and one above
#  the threshold.
#
#  Part 1  settings (choose one of three cases)
#  Part 2  where are the equilibria?
#  Part 3  the simulation function
#  Part 4  simulate a few people
#  Part 5  look at the data
#  Part 6  a variant: two thresholds (hysteresis)
#  Part 7  things to try
#
#  Run the script part by part (select the lines and press Ctrl+Enter).
# =============================================================================

# install.packages("ggplot2")   # run once if you do not have it
library(ggplot2)


# =============================================================================
#  PART 1: SETTINGS
# =============================================================================

set.seed(2026)                     # same random numbers every time you run this

Npeople <- 6                       # number of people
times <- seq(from = 0, to = 56, by = 0.25)  # 4 ratings a day for 8 weeks (time in days)
Nsteps <- 20                       # small steps between two ratings

A <- -0.5                          # state dependence (per day): the same in both regimes
G <- 6                             # kick strength
threshold <- 50                    # below this mood, the moderator is switched on

# Choose one of three cases ("two", "one" or "neither"); see Part 2 for what they mean.
case <- "two"

if (case == "two") {               # two stable states
  B <- 30                          # above 50: mood is pushed towards -B/A = 60
  Beffect <- -10                   # below 50: rumination lowers the push, towards -(B+Beffect)/A = 40
}
if (case == "one") {               # one stable state
  B <- 30                          # above 50: towards 60
  Beffect <- 5                     # below 50: an extra push UP (e.g. coping), towards 70
}
if (case == "neither") {           # no stable state on either side
  B <- 24                          # above 50: towards 48 (but that is below 50!)
  Beffect <- 4                     # below 50: towards 56 (but that is above 50!)
}

savePlots <- TRUE                  # TRUE: save every plot in Part 5 as a png file
plotFolder <- paste0("plots_threshold_", case)  # folder for the png files (made in your working directory)


# =============================================================================
#  PART 2: WHERE ARE THE EQUILIBRIA?
# =============================================================================
# Each side of the threshold has its own rule, and so its own candidate
# equilibrium (where the slope is 0). A candidate only counts if it lies on
# its own side of the threshold: otherwise mood leaves that side before it
# gets there.

eqAbove <- -B / A                  # rule above the threshold: A*Mood + B = 0
eqBelow <- -(B + Beffect) / A      # rule below the threshold: A*Mood + B + Beffect = 0

eqAboveValid <- eqAbove > threshold    #does it lie above the threshold?
eqBelowValid <- eqBelow < threshold    #does it lie below the threshold?

cat("Candidate above the threshold:", eqAbove, " valid:", eqAboveValid, "\n")
cat("Candidate below the threshold:", eqBelow, " valid:", eqBelowValid, "\n")

# Q: Before running anything else: where do you expect mood to settle?


# =============================================================================
#  PART 3: THE SIMULATION FUNCTION
# =============================================================================
# The function from Lecture 5. New: in every small step, we first check
# whether mood is below the threshold, and switch the moderator on or off.

simulateMood <- function(initialMood){
  Nobs <- length(times)                        #number of ratings
  Mood <- rep(NA, Nobs)                        #empty vector for the ratings
  for(i in 1:Nobs){
    if(i == 1) Mood[i] <- initialMood          #first rating: the starting value
    else{
      MoodState <- Mood[i-1]                   #start from the previous rating
      dt <- (times[i] - times[i-1]) / Nsteps   #size of each small step
      for(stepi in 1:Nsteps){
        moderator <- as.numeric(MoodState < threshold)    #NEW: 1 while mood is below the threshold, 0 above
        dMood <- A*MoodState + B + Beffect*moderator      #the rule for change depends on the moderator
        kick <- G * sqrt(dt) * rnorm(1)                   #random kick, variance G^2 * dt
        MoodState <- MoodState + dMood * dt + kick
      }
      Mood[i] <- MoodState                     #store the state reached at this rating
    }
  }
  Mood
}


# =============================================================================
#  PART 4: SIMULATE A FEW PEOPLE
# =============================================================================
# Everyone has the same parameters. They only differ in where they start
# (from 20 to 80) and in their random kicks.

startValues <- seq(from = 20, to = 80, length.out = Npeople)

dat <- NULL
for (person in 1:Npeople) {
  Mood <- simulateMood(initialMood = startValues[person])
  dat <- rbind(dat, data.frame(id = person, time = times, Mood = Mood))
}
dat$below <- dat$Mood < threshold        #was this rating below the threshold?
head(dat)

# write.csv(dat, paste0("lecture08_data_threshold_", case, ".csv"), row.names = FALSE)


# =============================================================================
#  PART 5: LOOK AT THE DATA
# =============================================================================
# Every plot is stored in an object (p5a, p5b, p5c), shown, and, if savePlots
# is TRUE, saved as a png file in plotFolder.

if (savePlots) dir.create(plotFolder, showWarnings = FALSE)   #make the folder (no warning if it already exists)

validEq <- c(eqAbove[eqAboveValid], eqBelow[eqBelowValid])    #only the equilibria that count

# --- 5a. The people over time -------------------------------------------------
dat$who <- paste0("starts at ", round(startValues[dat$id]))
dat$who <- factor(dat$who, levels = paste0("starts at ", round(startValues)))

p5a <- ggplot(dat, aes(x = time, y = Mood)) +
  geom_hline(yintercept = threshold, colour = "#eb6834", linewidth = 1) +    #the threshold
  geom_hline(yintercept = validEq, linetype = "dashed", colour = "grey40") + #the equilibria that count
  geom_line(colour = "grey30") +
  geom_point(aes(colour = below), size = 0.8) +
  scale_colour_manual(values = c("FALSE" = "#2a78d6", "TRUE" = "#b0306a"),
                      labels = c("above 50: moderator off", "below 50: moderator on")) +
  facet_wrap(~ who) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5a   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5a_people.png"), p5a, width = 11, height = 7, dpi = 150)

# Q: Where does each person spend their time? Does anyone switch sides?

# --- 5b. The rule for change: slope against mood --------------------------------
# For every possible mood, how fast does mood change (without kicks)?
# Where the line crosses 0, mood stops changing: an equilibrium.

rule <- data.frame(Mood = seq(from = 20, to = 80, by = 0.1))
rule$moderator <- as.numeric(rule$Mood < threshold)
rule$slope <- A*rule$Mood + B + Beffect*rule$moderator
rule$side <- ifelse(rule$moderator == 1, "below 50: moderator on", "above 50: moderator off")

p5b <- ggplot(rule, aes(x = Mood, y = slope, colour = side)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = threshold, colour = "#eb6834", linewidth = 1) +
  geom_line(linewidth = 1.5) +
  geom_point(data = data.frame(Mood = validEq, slope = rep(0, length(validEq))),   #the equilibria that count
             colour = "black", size = 4) +
  scale_colour_manual(values = c("above 50: moderator off" = "#2a78d6",
                                 "below 50: moderator on" = "#b0306a")) +
  theme_bw(base_size = 16) +
  labs(x = "Mood", y = "Slope: change in mood per day", colour = NULL) +
  theme(legend.position = "bottom")
p5b   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5b_rule.png"), p5b, width = 9, height = 6, dpi = 150)

# Q: Above the zero line mood goes up, below it mood goes down. Follow the
#    arrows in your head from a mood of 45 and from a mood of 55. Where do
#    they end up?

# --- 5c. All ratings together -----------------------------------------------------
p5c <- ggplot(dat, aes(x = Mood)) +
  geom_histogram(binwidth = 2, fill = "grey60", colour = "white") +
  geom_vline(xintercept = threshold, colour = "#eb6834", linewidth = 1) +
  geom_vline(xintercept = validEq, linetype = "dashed", colour = "grey20") +
  theme_bw(base_size = 16) +
  labs(x = "Mood", y = "Number of ratings")
p5c   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "5c_histogram.png"), p5c, width = 9, height = 6, dpi = 150)

mean(dat$Mood)                     # the mean of all ratings
# Q: Is the mean of all ratings a good summary of where mood "usually" is?


# =============================================================================
#  PART 6: A VARIANT: TWO THRESHOLDS (HYSTERESIS)
# =============================================================================
# Now the moderator switches ON when mood drops below a low threshold, and
# OFF only when mood climbs above a high threshold. In between, it stays as it
# was. Example: someone starts seeking support when mood drops below 40, and
# keeps going until mood is back above 60.
#
# The moderator now has a memory, so it is set once before the loop and is
# NOT reset at each rating.

lowThreshold <- 40                 # below this: switch on
highThreshold <- 60                # above this: switch off
Bhyst <- 22                        # push without support: mood drifts towards -Bhyst/A = 44
BeffectSupport <- 10               # extra push UP while support is on: towards -(Bhyst+10)/A = 64

simulateMoodHysteresis <- function(initialMood){
  Nobs <- length(times)
  Mood <- rep(NA, Nobs)
  Support <- rep(NA, Nobs)                     #also store whether support is on at each rating
  moderator <- 0                               #support starts off (set ONCE, before the loop)
  for(i in 1:Nobs){
    if(i == 1) Mood[i] <- initialMood
    else{
      MoodState <- Mood[i-1]
      dt <- (times[i] - times[i-1]) / Nsteps
      for(stepi in 1:Nsteps){
        if (MoodState < lowThreshold) moderator <- 1      #switch on below the low threshold
        if (MoodState > highThreshold) moderator <- 0     #switch off above the high threshold
                                                          #in between: keep the current value
        dMood <- A*MoodState + Bhyst + BeffectSupport*moderator
        kick <- G * sqrt(dt) * rnorm(1)
        MoodState <- MoodState + dMood * dt + kick
      }
      Mood[i] <- MoodState
    }
    Support[i] <- moderator
  }
  data.frame(time = times, Mood = Mood, Support = Support)
}

hyst <- simulateMoodHysteresis(initialMood = 50)

p6 <- ggplot(hyst, aes(x = time, y = Mood)) +
  geom_hline(yintercept = c(lowThreshold, highThreshold), colour = "#eb6834", linetype = "dashed") +
  geom_line(colour = "grey30") +
  geom_point(aes(colour = factor(Support)), size = 1) +
  scale_colour_manual(values = c("0" = "#2a78d6", "1" = "#2f9e6e"),
                      labels = c("support off", "support on")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p6   #show the plot
if (savePlots) ggsave(file.path(plotFolder, "6_hysteresis.png"), p6, width = 11, height = 5, dpi = 150)

# Q: Find a moment where mood is 50 and support is on, and one where mood is
#    50 and support is off. Why can the same mood come with different rules?
# Q: Without support mood drifts towards 44, with support towards 64. Why does
#    mood keep going up and down between the two thresholds?


# =============================================================================
#  PART 7: THINGS TO TRY
# =============================================================================
# 1. Run the script with case <- "one" and with case <- "neither". Before you
#    run each one, use Part 2 and the plot in 5b to predict what mood will do.
#
# 2. Two stable states and the kicks: with case <- "two", set G to 3 and then
#    to 10. How often do people switch sides? What does that do to the mean
#    in 5c?
#
# 3. Check the small steps: in case "neither", mood keeps crossing the
#    threshold. Set Nsteps to 5 and then to 100. Does the picture change?
#    Why is this model more sensitive to Nsteps than the models of Lecture 7?
#
# 4. A smooth switch: replace the moderator line in simulateMood() by
#       moderator <- plogis((threshold - MoodState) / 2)
#    The moderator now goes gradually from 1 (well below 50) to 0 (well
#    above 50), and is 0.5 at exactly 50. Which case changes most?
#
# 5. Check only at the ratings: move the moderator line out of the small-step
#    loop and compute it from Mood[i-1] instead of MoodState. What goes wrong,
#    and why does it get worse when you measure less often?
