# =============================================================================
#  LECTURE 10 -- SIMULATE DATA: A PERSISTENT SHOCK
#  (a shock that starts a process of its own, which never fades)
#  Dynamic Systems Thinking in Psychology
#
#  Example: on day 5, Albert moves to a new flat with a garden. The move
#  starts something (a nicer home, sunlight, space) that keeps pushing his
#  mood up, and that does not fade:
#
#     dMood  = (A * Mood + B + C * Input) dt + G dW
#     dInput = (Ainput * Input) dt              (+ dose, at the shock)
#
#     with Ainput = 0: the shock process stays where the shock put it.
#
#  In matrix form (the coupled model from Lecture 7):
#
#     d/dt (Mood, Input) = | A  C | (Mood, Input) + (B, 0)
#                          | 0  0 |
#
#  The shock process is latent: we only measure Albert's mood. Because it
#  never fades, it acts like a new, larger intercept: B becomes B + C * dose.
#  So the shock moves Albert's resting level.
#
#  Compare with lecture10_simulate_transient.R (no process) and
#  lecture10_simulate_dissipative.R (a process that fades).
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
restMood <- 50                     # where Albert's mood rests before the shock
B <- -A * restMood                 # the intercept that makes him rest there (= 25)
C <- 1                             # how strongly the shock process pushes mood
G <- 3                             # kick strength (set to 0 to see the clean response)

shockTimes <- 5                    # the move is on day 5 (must be one of the rating times)
dose <- 10                         # the new flat starts a process of size 10 ...
Ainput <- 0                        # ... which never fades

seed <- 2026                       # same random kicks every time you run this
savePlots <- TRUE                  # TRUE: save every plot in Part 5 as a png file
plotFolder <- "plots_persistent"   # folder for the png files (made in your working directory)


# =============================================================================
#  PART 2: WHAT TO EXPECT
# =============================================================================
# Mood always moves towards the target where its own pull cancels the push:
#
#     target = restMood + C * Input / (-A)
#
# Here Input jumps to 'dose' and stays there, so the target jumps once and
# stays: that is Albert's new resting level.

newRest <- restMood + C * dose * length(shockTimes) / (-A)
cat("Before the shock, mood rests at:", restMood, "\n")
cat("After the shock, mood moves to a new resting level:", newRest, "\n")
cat("Half of the way there after:", round(log(2) / -A, 2), "days (set by Albert's own A)\n")

# Q: Sketch the shock process and Albert's mood before you run anything else.


# =============================================================================
#  PART 3: THE SIMULATION FUNCTION
# =============================================================================
# The same function as in lecture10_simulate_dissipative.R. With direct = TRUE,
# the same shock hits mood directly (a transient shock), for comparison.

simulateAlbert <- function(Ainput, direct = FALSE, G = 3, times){
  set.seed(seed)                               #same kicks for every version we compare
  Nobs <- length(times)
  Mood <- Input <- rep(NA, Nobs)
  MoodState <- restMood                        #start at the resting level
  InputState <- 0                              #no shock yet
  for(i in 1:Nobs){
    if(i > 1){
      dt <- (times[i] - times[i-1]) / Nsteps   #size of each small step
      for(stepi in 1:Nsteps){
        dMood <- A*MoodState + B + C*InputState          #mood: own pull plus the push of the shock process
        dInput <- Ainput*InputState                      #the shock process's own rule (here: no change)
        kick <- G * sqrt(dt) * rnorm(1)                  #random kick, variance G^2 * dt
        MoodState <- MoodState + dMood * dt + kick
        InputState <- InputState + dInput * dt
      }
    }
    if(any(abs(times[i] - shockTimes) < 1e-8)){          #does a shock arrive at this moment?
      if(direct) MoodState <- MoodState + dose           #transient: the shock hits mood directly
      else InputState <- InputState + dose               #persistent: the shock starts a process that stays
    }
    Mood[i] <- MoodState
    Input[i] <- InputState
  }
  data.frame(time = times, Mood = Mood, Input = Input)
}


# =============================================================================
#  PART 4: SIMULATE ALBERT
# =============================================================================

albert <- simulateAlbert(Ainput = Ainput, G = G, times = times)
albertDirect <- simulateAlbert(Ainput = Ainput, direct = TRUE, G = G, times = times)
albert$target <- restMood + C * albert$Input / (-A)
head(albert)

# write.csv(albert, "lecture10_data_persistent.csv", row.names = FALSE)


# =============================================================================
#  PART 5: LOOK AT THE DATA
# =============================================================================
if (savePlots) dir.create(plotFolder, showWarnings = FALSE)

# --- 5a. Albert and the shock process over time --------------------------------
panels <- c("Mood (measured)", "Shock process (not measured)")
long <- rbind(data.frame(time = albert$time, value = albert$Mood,   panel = panels[1], line = "mood"),
              data.frame(time = albert$time, value = albert$target, panel = panels[1], line = "target"),
              data.frame(time = albert$time, value = albert$Input,  panel = panels[2], line = "process"))
long$panel <- factor(long$panel, levels = panels)

p5a <- ggplot(long, aes(x = time, y = value, colour = line, linetype = line)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(mood = "#2a78d6", target = "#eb6834", process = "#eb6834")) +
  scale_linetype_manual(values = c(mood = "solid", target = "dashed", process = "solid")) +
  facet_grid(rows = vars(panel), scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "Time (days)", y = NULL,
       title = "The target (dashed) jumps once and stays: a new resting level")
p5a
if (savePlots) ggsave(file.path(plotFolder, "5a_mood_and_process.png"), p5a, width = 11, height = 7, dpi = 150)

# Q: The shock process jumps at once. Why does mood not?

# --- 5b. Persistent versus transient -----------------------------------------------
both <- rbind(data.frame(time = albert$time, Mood = albert$Mood, version = "persistent: shock starts a process that stays"),
              data.frame(time = albertDirect$time, Mood = albertDirect$Mood, version = "transient: shock hits mood directly"))

p5b <- ggplot(both, aes(x = time, y = Mood, colour = version)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = c(restMood, newRest), linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#eb6834", "#b0306a")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5b
if (savePlots) ggsave(file.path(plotFolder, "5b_persistent_vs_transient.png"), p5b, width = 11, height = 5, dpi = 150)

# Q: The persistent shock and the transient shock have the same dose. Why
#    does one move mood by 10 for a moment, and the other by 20 for ever?

# --- 5c. Before and after ------------------------------------------------------------
# Compare the ratings in the 5 days before the move with the ratings from
# day 15 on (when mood has settled). Did anything else about Albert change?
before <- albert$Mood[albert$time < shockTimes[1]]
after  <- albert$Mood[albert$time >= 15]
round(c(meanBefore = mean(before), meanAfter = mean(after),
        sdBefore = sd(before), sdAfter = sd(after)), 1)

periods <- rbind(data.frame(Mood = before, period = "before the move"),
                 data.frame(Mood = after,  period = "from day 15 on"))
p5c <- ggplot(periods, aes(x = Mood, fill = period)) +
  geom_histogram(binwidth = 1.5, colour = "white", position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("#2a78d6", "#eb6834")) +
  theme_bw(base_size = 16) +
  labs(x = "Mood", y = "Number of ratings", fill = NULL) +
  theme(legend.position = "bottom")
p5c
if (savePlots) ggsave(file.path(plotFolder, "5c_before_after.png"), p5c, width = 9, height = 5, dpi = 150)

# Q: The mean changed. Did the ups and downs (the SD) change? Did A change?

# =============================================================================
#  PART 6: THINGS TO TRY
# =============================================================================
# 1. Make Albert slower (A <- -0.1, and keep B <- -A * restMood). Run
#    Part 2 again. Where does mood end up now? Why does a slow Albert get a
#    BIGGER lasting effect from the same move?
#
# 2. Make the shock process fade very slowly (Ainput <- -0.02). Over 30
#    days, can you tell it apart from a persistent shock? Extend the times
#    to 200 days. Now?
#
# 3. Two moves: shockTimes <- c(5, 15). Where does Albert end up?
#
# 4. A persistent shock with negative coupling (C <- -1): Albert loses his
#    job on day 5. Where does mood settle? Is "Ainput = 0" realistic here?
