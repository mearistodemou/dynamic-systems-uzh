# =============================================================================
#  LECTURE 10 -- SIMULATE DATA: A TRANSIENT SHOCK
#  Dynamic Systems Thinking in Psychology
#
#  The model from Lecture 5, plus one shock:
#
#     dMood = (A * Mood + B) dt + G dW        (+ dose, at the moment of the shock)
#
#  Example: on day 5, Albert gets an unexpected compliment from his boss.
#  The compliment itself is over in a moment: it has no life of its own. It
#  lifts Albert's mood by 'dose' at that moment, and after that, only
#  Albert's own pull back to his resting level (A) brings his mood down again.
#
#  This is the simplest shock, and the one the other Lecture 10 scripts are
#  compared with. In those scripts, the shock starts a process of its own.
#  Here, there is no such process: the shock appears and disappears at once.
#
#  Part 1  settings
#  Part 2  what to expect
#  Part 3  the simulation function
#  Part 4  simulate Albert
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

times <- seq(from = 0, to = 20, by = 0.25)  # 4 ratings a day for 20 days (time in days)
Nsteps <- 20                       # small steps between two ratings

A <- -0.5                          # Albert's own state dependence (per day)
restMood <- 50                     # where Albert's mood rests without any shock
B <- -A * restMood                 # the intercept that makes him rest there (= 25)
G <- 3                             # kick strength (set to 0 to see the clean response)

shockTimes <- 5                    # the compliment comes on day 5 (must be one of the rating times)
dose <- 15                         # it lifts mood by 15, right away

seed <- 2026                       # same random kicks every time you run this
savePlots <- TRUE                  # TRUE: save every plot in Part 5 as a png file
plotFolder <- "plots_transient"    # folder for the png files (made in your working directory)


# =============================================================================
#  PART 2: WHAT TO EXPECT
# =============================================================================
# Right after the shock, mood is at restMood + dose. After that, the distance
# to the resting level shrinks by the same fraction every day, because the
# pull back (A * distance) is proportional to the distance (Lecture 5):
#
#     Mood(t) = restMood + dose * exp(A * (t - shockTime))
#
# Half of the shock is gone after log(2) / -A days: the half-life.

halfLife <- log(2) / -A
cat("Right after the shock, mood is at:", restMood + dose, "\n")
cat("Half of the shock is gone after:", round(halfLife, 2), "days\n")
cat("After 5 days, what is left of the shock:", round(dose * exp(A * 5), 2), "\n")

# Q: Sketch Albert's mood before you run anything else. Is mood highest at
#    the shock, or later?


# =============================================================================
#  PART 3: THE SIMULATION FUNCTION
# =============================================================================
# The function from Lecture 5. New: when the shock arrives, mood jumps by
# 'dose'. That is all; nothing about the shock is remembered afterwards.

simulateAlbert <- function(A, G, times){
  set.seed(seed)                               #same kicks for every version we compare
  B <- -A * restMood                           #rest at restMood, whatever A is
  Nobs <- length(times)
  Mood <- rep(NA, Nobs)
  MoodState <- restMood                        #start at the resting level
  for(i in 1:Nobs){
    if(i > 1){
      dt <- (times[i] - times[i-1]) / Nsteps   #size of each small step
      for(stepi in 1:Nsteps){
        tNow <- times[i-1] + stepi * dt        #the current time (only needed for Part 6, item 4)
        dMood <- A*MoodState + B               #Albert's own rule for change
        kick <- G * sqrt(dt) * rnorm(1)        #random kick, variance G^2 * dt
        MoodState <- MoodState + dMood * dt + kick
      }
    }
    if(any(abs(times[i] - shockTimes) < 1e-8)) MoodState <- MoodState + dose   #NEW: the shock, at this moment only
    Mood[i] <- MoodState                       #store the rating (just after the shock, if there was one)
  }
  data.frame(time = times, Mood = Mood)
}


# =============================================================================
#  PART 4: SIMULATE ALBERT
# =============================================================================

albert <- simulateAlbert(A = A, G = G, times = times)          #with random kicks
albertClean <- simulateAlbert(A = A, G = 0, times = times)     #the same, without kicks
head(albert)

# write.csv(albert, "lecture10_data_transient.csv", row.names = FALSE)


# =============================================================================
#  PART 5: LOOK AT THE DATA
# =============================================================================
if (savePlots) dir.create(plotFolder, showWarnings = FALSE)

# --- 5a. Albert over time ---------------------------------------------------------
p5a <- ggplot(albert, aes(x = time, y = Mood)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_vline(xintercept = shockTimes[1] + halfLife, linetype = "dashed", colour = "#eb6834") +  #half of the shock gone
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(data = albertClean, colour = "#b0306a", linewidth = 1.2) +    #without kicks
  geom_line(colour = "grey40") +                                          #with kicks
  geom_point(colour = "grey20", size = 0.8) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood",
       title = "Transient shock: with kicks (grey) and without (pink)")
p5a
if (savePlots) ggsave(file.path(plotFolder, "5a_albert.png"), p5a, width = 11, height = 5, dpi = 150)

# Q: Where is the shock easiest to see? How long would you say it "lasts"?

# --- 5b. How fast does Albert recover? ------------------------------------------
# The same shock for three versions of Albert that differ only in A.
speeds <- c(-2, -0.5, -0.1)
recover <- NULL
for (a in speeds) {
  sim <- simulateAlbert(A = a, G = 0, times = seq(from = 0, to = 20, by = 0.05))
  recover <- rbind(recover, data.frame(sim, A = paste("A =", a)))
}
recover$A <- factor(recover$A, levels = paste("A =", speeds))

p5b <- ggplot(recover, aes(x = time, y = Mood, colour = A)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("#b0306a", "#2a78d6", "#2f9e6e")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5b
if (savePlots) ggsave(file.path(plotFolder, "5b_recovery_speeds.png"), p5b, width = 11, height = 5, dpi = 150)

# Q: The shock is the same in all three. What decides how long it is visible?
# Q: Where is the slope of mood steepest: right after the shock, or later? Why?

# --- 5c. What a daily study sees --------------------------------------------------
# Now Albert rates his mood once a day, every evening (day 0.75, 1.75, ...),
# and the compliment comes in the morning of day 5 (5.25). Fast-recovering
# Albert (A = -2) and our Albert (A = -0.5), without kicks.
shockTimesKeep <- shockTimes
shockTimes <- 5.25                             #the compliment comes in the morning
fine <- seq(from = 0, to = 20, by = 0.05)      #what really happens
daily <- seq(from = 0.75, to = 19.75, by = 1)  #what the study sees
study <- NULL
for (a in c(-2, -0.5)) {
  sim <- simulateAlbert(A = a, G = 0, times = fine)
  study <- rbind(study, data.frame(sim, A = paste("A =", a),
                                   rated = sapply(sim$time, function(t) any(abs(t - daily) < 1e-8))))
}
shockTimes <- shockTimesKeep                   #back to the setting of Part 1

p5c <- ggplot(study, aes(x = time, y = Mood)) +
  geom_vline(xintercept = 5.25, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(colour = "grey60") +                                   #what really happens
  geom_point(data = study[study$rated, ], colour = "#2a78d6", size = 2.5) +  #the daily ratings
  facet_wrap(~ A, ncol = 1) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood",
       title = "Grey: Albert's mood. Blue: what a once-a-day rating sees")
p5c
if (savePlots) ggsave(file.path(plotFolder, "5c_daily_study.png"), p5c, width = 11, height = 7, dpi = 150)

# Q: How big does the shock look in the daily ratings, for each Albert?
#    What would a researcher conclude about the compliment from these ratings?


# =============================================================================
#  PART 6: THINGS TO TRY
# =============================================================================
# 1. Make the shock negative (dose <- -15: a fight with a friend). Does
#    anything change, apart from the direction?
#
# 2. Turn up the kicks (G <- 8). Can you still find the shock in 5a? How
#    would you look for it in data from many people who all got a compliment
#    on day 5?
#
# 3. Two shocks: give Albert a second compliment 1 day after the first
#    (shockTimes <- c(5, 6)). Does the second one add to the first? What if
#    it comes 10 days later (shockTimes <- c(5, 15))?
#
# 4. A shock that lasts a few hours: set shockTimes <- -1 (no jump), and add
#    this line in the small-step loop, right after the dMood line:
#       if(tNow > 5 && tNow <= 5.25) dMood <- dMood + dose / 0.25
#    Mood now gets the same total push, spread over 6 hours. How different
#    does it look from the jump? What if you spread it over 3 days?
