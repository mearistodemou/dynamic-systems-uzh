# =============================================================================
#  LECTURE 10 -- SIMULATE DATA: AN OSCILLATING SHOCK
#  (a shock that starts a process which swings, like a spring with friction)
#  Dynamic Systems Thinking in Psychology
#
#  On day 3, a shock starts a process that does not simply fade: it swings
#  past 0 to the other side, then back, with smaller and smaller swings,
#  until it settles. The shock process swings around 0 like a spring with
#  friction: a damped oscillation. Mood is pushed up while the process is
#  above 0, and pulled down while it is below 0.
#
#  A spring needs two states: where it is (Input) and how fast it moves
#  (Speed). The shock gives Input a push; after that:
#
#     dMood  = (A * Mood + B + C * Input) dt + G dW
#     dInput = Speed dt
#     dSpeed = (Aspring * Input + Adamp * Speed) dt
#
#     Aspring < 0: the further Input is from 0, the harder it is pulled back
#     Adamp   < 0: friction; the swings get smaller and smaller
#
#  In matrix form:
#
#     d/dt (Mood, Input, Speed) = | A  C        0     | (Mood, Input, Speed) + (B, 0, 0)
#                                 | 0  0        1     |
#                                 | 0  Aspring  Adamp |
#
#  Both Input and Speed are latent: we only measure Albert's mood.
#
#  Compare with lecture10_simulate_dissipative.R, where the shock process
#  simply fades.
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

times <- seq(from = 0, to = 30, by = 0.25)  # 4 ratings a day for 30 days (time in days)
Nsteps <- 20                       # small steps between two ratings

A <- -0.5                          # Albert's own state dependence (per day)
restMood <- 50                     # where Albert's mood rests without any shock
B <- -A * restMood                 # the intercept that makes him rest there (= 25)
C <- 1                             # how strongly the shock process pushes mood
G <- 3                             # kick strength (set to 0 to see the clean response)

shockTimes <- 3                    # the shock comes on day 3 (must be one of the rating times)
dose <- 10                         # how big the shock process is right after the shock
period <- 6                        # one full swing (high, low, high) takes about 6 days
Aspring <- -(2*pi/period)^2        # pull back towards 0 (sets the period)
Adamp <- -0.3                      # friction (closer to 0: longer swinging)

seed <- 2026                       # same random kicks every time you run this
savePlots <- TRUE                  # TRUE: save every plot in Part 5 as a png file
plotFolder <- "plots_oscillating"  # folder for the png files (made in your working directory)


# =============================================================================
#  PART 2: WHAT TO EXPECT
# =============================================================================
# As before, mood chases the target restMood + C * Input / (-A). When Input
# swings below 0, the target is below the resting level, and mood follows.
#
# How fast the swings die out: the size of each swing shrinks by the factor
# exp(Adamp / 2 * period) per period.

cat("Target right after the shock:", restMood + C * dose / (-A), "\n")
cat("Each full swing is", round(100 * exp(Adamp / 2 * period)), "% of the one before\n")
cat("Albert covers half the distance to a target in", round(log(2) / -A, 2), "days\n")

# Q: Sketch the shock process and Albert's mood before you run anything else.
#    Will Albert's mood drop below 50 at some point?


# =============================================================================
#  PART 3: THE SIMULATION FUNCTION
# =============================================================================
# The function from Lecture 5 with two extra states. One technical detail: in
# each small step we first update Speed, and then move Input with the NEW
# Speed. With the usual order, every small step adds a little energy to the
# spring, and a fast swing would grow instead of dying out.

simulateAlbert <- function(period, Adamp, G = 3, times){
  set.seed(seed)                               #same kicks for every version we compare
  Aspring <- -(2*pi/period)^2
  Nobs <- length(times)
  Mood <- Input <- rep(NA, Nobs)
  MoodState <- restMood; InputState <- 0; SpeedState <- 0
  for(i in 1:Nobs){
    if(i > 1){
      dt <- (times[i] - times[i-1]) / Nsteps
      for(stepi in 1:Nsteps){
        dMood <- A*MoodState + B + C*InputState          #mood: own pull plus the push of the shock process
        dSpeed <- Aspring*InputState + Adamp*SpeedState  #the spring pulls back, friction slows down
        kick <- G * sqrt(dt) * rnorm(1)
        MoodState <- MoodState + dMood * dt + kick
        SpeedState <- SpeedState + dSpeed * dt
        InputState <- InputState + SpeedState * dt       #Input moves with the NEW speed
      }
    }
    if(any(abs(times[i] - shockTimes) < 1e-8)) InputState <- InputState + dose   #the shock
    Mood[i] <- MoodState
    Input[i] <- InputState
  }
  data.frame(time = times, Mood = Mood, Input = Input)
}


# =============================================================================
#  PART 4: SIMULATE ALBERT
# =============================================================================

albert <- simulateAlbert(period = period, Adamp = Adamp, G = G, times = times)       #with kicks
albertClean <- simulateAlbert(period = period, Adamp = Adamp, G = 0, times = times)  #without kicks
albertClean$target <- restMood + C * albertClean$Input / (-A)
head(albert)

# write.csv(albert, "lecture10_data_oscillating.csv", row.names = FALSE)


# =============================================================================
#  PART 5: LOOK AT THE DATA
# =============================================================================
if (savePlots) dir.create(plotFolder, showWarnings = FALSE)

# --- 5a. Albert and the shock process, without kicks -------------------------------
panels <- c("Mood (measured)", "Shock process (not measured)")
long <- rbind(data.frame(time = albertClean$time, value = albertClean$Mood,   panel = panels[1], line = "mood"),
              data.frame(time = albertClean$time, value = albertClean$target, panel = panels[1], line = "target"),
              data.frame(time = albertClean$time, value = albertClean$Input,  panel = panels[2], line = "process"))
long$panel <- factor(long$panel, levels = panels)
zeroLines <- data.frame(panel = factor(panels, levels = panels), y = c(restMood, 0))

p5a <- ggplot(long, aes(x = time, y = value)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(data = zeroLines, aes(yintercept = y), colour = "grey70") +
  geom_line(aes(colour = line, linetype = line), linewidth = 1) +
  scale_colour_manual(values = c(mood = "#2a78d6", target = "#eb6834", process = "#7a4fb5")) +
  scale_linetype_manual(values = c(mood = "solid", target = "dashed", process = "solid")) +
  facet_grid(rows = vars(panel), scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "Time (days)", y = NULL,
       title = "Without kicks: mood chases a target (dashed) that swings")
p5a
if (savePlots) ggsave(file.path(plotFolder, "5a_mood_and_process.png"), p5a, width = 11, height = 7, dpi = 150)

# Q: A few days after the shock, Albert's mood drops below 50. Is that
#    because of Albert's own rule (A), or because of the shock process?
#    Could the mood model from Lecture 5, on its own and without kicks, ever
#    overshoot its resting level?
# Q: The target swings down to about 38, but mood only gets down to about 45.
#    Why?

# --- 5b. With kicks ------------------------------------------------------------------
p5b <- ggplot(albert, aes(x = time, y = Mood)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(data = albertClean, colour = "#2a78d6", linewidth = 1.2) +   #without kicks
  geom_line(colour = "grey40") +                                         #with kicks
  geom_point(colour = "grey20", size = 0.8) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood",
       title = "With kicks (grey) and without (blue)")
p5b
if (savePlots) ggsave(file.path(plotFolder, "5b_with_kicks.png"), p5b, width = 11, height = 5, dpi = 150)

# Q: Would you see the low after the shock in these ratings? What kind of
#    study would you need to find it?

# --- 5c. Fast and slow swings ----------------------------------------------------------
# The same shock with three periods, without kicks.
periods <- c(1, 6, 14)
swing <- NULL
for (p in periods) {
  sim <- simulateAlbert(period = p, Adamp = Adamp, G = 0, times = times)
  swing <- rbind(swing, data.frame(sim, period = paste("period =", p, "days")))
}
swing$period <- factor(swing$period, levels = paste("period =", periods, "days"))

p5c <- ggplot(swing, aes(x = time, y = Mood, colour = period)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("#b0306a", "#2a78d6", "#2f9e6e")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5c
if (savePlots) ggsave(file.path(plotFolder, "5c_periods.png"), p5c, width = 11, height = 5, dpi = 150)

# Q: Which swing reaches Albert's mood, and which is smoothed away? Why?


# =============================================================================
#  PART 6: THINGS TO TRY
# =============================================================================
# 1. Friction: set Adamp to -3. Does Albert still have a low after the
#    shock? Set Adamp to 0: what happens, and is that realistic?
#
# 2. A faster Albert: set A to -2 (and keep B <- -A * restMood). Run 5c
#    again. Does the fast swing (period = 1) reach his mood now?
#
# 3. A negative shock: set dose to -10. What does the low-then-high look
#    like in mood?
#
# 4. Measure once a day (times <- seq(from = 0, to = 30, by = 1)) and use
#    period = 1. What do the daily ratings show? (Compare with 5c.)
