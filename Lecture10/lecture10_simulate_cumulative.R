# =============================================================================
#  LECTURE 10 -- SIMULATE DATA: A CUMULATIVE SHOCK
#  (repeated shocks that build up in a process of their own)
#  Dynamic Systems Thinking in Psychology
#
#  Example: Albert goes to therapy once a week, 8 sessions. Each session adds
#  to a process (what he has learned and practises), which fades slowly
#  between sessions. If a session comes before the last one has faded, the
#  process builds up:
#
#     dMood  = (A * Mood + B + C * Input) dt + G dW
#     dInput = (Ainput * Input) dt              (+ dose, at each session)
#
#  In matrix form (the coupled model from Lecture 7):
#
#     d/dt (Mood, Input) = | A  C      | (Mood, Input) + (B, 0)
#                          | 0  Ainput |
#
#  This is the dissipative shock of lecture10_simulate_dissipative.R, given
#  again and again. Whether it builds up depends on how fast it fades
#  (Ainput) compared with how often it comes (the gap between sessions).
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

times <- seq(from = 0, to = 84, by = 0.25)  # 4 ratings a day for 12 weeks (time in days)
Nsteps <- 20                       # small steps between two ratings

A <- -0.5                          # Albert's own state dependence (per day)
restMood <- 50                     # where Albert's mood rests without therapy
B <- -A * restMood                 # the intercept that makes him rest there (= 25)
C <- 1                             # how strongly the therapy process pushes mood
G <- 3                             # kick strength (set to 0 to see the clean response)

shockTimes <- seq(from = 7, to = 56, by = 7)   # sessions on days 7, 14, ..., 56 (rating times)
dose <- 3                          # each session adds 3 to the therapy process ...
Ainput <- -0.1                     # ... which fades slowly: half-life log(2)/0.1 = 6.9 days

seed <- 2026                       # same random kicks every time you run this
savePlots <- TRUE                  # TRUE: save every plot in Part 5 as a png file
plotFolder <- "plots_cumulative"   # folder for the png files (made in your working directory)


# =============================================================================
#  PART 2: WHAT TO EXPECT
# =============================================================================
# Between two sessions the process loses -Ainput * Input per day; at each
# session it gains 'dose'. It stops building up when, on average, the loss
# over one gap equals the dose:
#
#     -Ainput * averageInput * gap = dose   ->   averageInput = dose / (-Ainput * gap)
#
# Mood then moves around the target that goes with it:
#
#     restMood + C * averageInput / (-A)

gap <- shockTimes[2] - shockTimes[1]                     # days between two sessions
keptPerGap <- exp(Ainput * gap)                          # fraction of the process left after one gap
averageInput <- dose / (-Ainput * gap)
cat("Of each session,", round(100 * keptPerGap), "% is still there at the next one\n")
cat("The therapy process levels off around:", round(averageInput, 1), "\n")
cat("So during therapy, mood levels off around:", round(restMood + C * averageInput / (-A), 1), "\n")
cat("Half of the process is lost", round(log(2) / -Ainput, 1), "days after the last session\n")

# Q: Sketch the therapy process and Albert's mood over the 12 weeks before
#    you run anything else. What happens after the last session?


# =============================================================================
#  PART 3: THE SIMULATION FUNCTION
# =============================================================================
# The same function as in lecture10_simulate_dissipative.R. With direct = TRUE,
# each session hits mood directly (transient shocks), for comparison.

simulateAlbert <- function(Ainput, direct = FALSE, G = 3, times, shockTimes, dose){
  set.seed(seed)                               #same kicks for every version we compare
  Nobs <- length(times)
  Mood <- Input <- rep(NA, Nobs)
  MoodState <- restMood                        #start at the resting level
  InputState <- 0                              #no therapy yet
  for(i in 1:Nobs){
    if(i > 1){
      dt <- (times[i] - times[i-1]) / Nsteps   #size of each small step
      for(stepi in 1:Nsteps){
        dMood <- A*MoodState + B + C*InputState          #mood: own pull plus the push of the therapy process
        dInput <- Ainput*InputState                      #the therapy process fades by its own rule
        kick <- G * sqrt(dt) * rnorm(1)                  #random kick, variance G^2 * dt
        MoodState <- MoodState + dMood * dt + kick
        InputState <- InputState + dInput * dt
      }
    }
    if(any(abs(times[i] - shockTimes) < 1e-8)){          #is there a session at this moment?
      if(direct) MoodState <- MoodState + dose           #transient: the session hits mood directly
      else InputState <- InputState + dose               #cumulative: the session adds to the process
    }
    Mood[i] <- MoodState
    Input[i] <- InputState
  }
  data.frame(time = times, Mood = Mood, Input = Input)
}


# =============================================================================
#  PART 4: SIMULATE ALBERT
# =============================================================================

albert <- simulateAlbert(Ainput = Ainput, G = G, times = times, shockTimes = shockTimes, dose = dose)
albertDirect <- simulateAlbert(Ainput = Ainput, direct = TRUE, G = G, times = times,
                               shockTimes = shockTimes, dose = dose)
albert$target <- restMood + C * albert$Input / (-A)
head(albert)

# write.csv(albert, "lecture10_data_cumulative.csv", row.names = FALSE)


# =============================================================================
#  PART 5: LOOK AT THE DATA
# =============================================================================
if (savePlots) dir.create(plotFolder, showWarnings = FALSE)

# --- 5a. Albert and the therapy process over time ------------------------------
panels <- c("Mood (measured)", "Therapy process (not measured)")
long <- rbind(data.frame(time = albert$time, value = albert$Mood,   panel = panels[1], line = "mood"),
              data.frame(time = albert$time, value = albert$target, panel = panels[1], line = "target"),
              data.frame(time = albert$time, value = albert$Input,  panel = panels[2], line = "process"))
long$panel <- factor(long$panel, levels = panels)

p5a <- ggplot(long, aes(x = time, y = value, colour = line, linetype = line)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(mood = "#2a78d6", target = "#eb6834", process = "#2f9e6e")) +
  scale_linetype_manual(values = c(mood = "solid", target = "dashed", process = "solid")) +
  facet_grid(rows = vars(panel), scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "Time (days)", y = NULL,
       title = "Each session adds to a process that has not faded yet")
p5a
if (savePlots) ggsave(file.path(plotFolder, "5a_mood_and_process.png"), p5a, width = 11, height = 7, dpi = 150)

# Q: After how many sessions has the process (almost) stopped building up?
#    Does that agree with Part 2?

# --- 5b. Cumulative versus transient ---------------------------------------------
both <- rbind(data.frame(time = albert$time, Mood = albert$Mood, version = "cumulative: sessions add to a process"),
              data.frame(time = albertDirect$time, Mood = albertDirect$Mood, version = "transient: sessions hit mood directly"))

p5b <- ggplot(both, aes(x = time, y = Mood, colour = version)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#2f9e6e", "#b0306a")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5b
if (savePlots) ggsave(file.path(plotFolder, "5b_cumulative_vs_transient.png"), p5b, width = 11, height = 5, dpi = 150)

# Q: Why do the transient sessions not build up, even though they have the
#    same dose and come just as often?

# --- 5c. How fast does the therapy process fade? -----------------------------------
# The same 8 sessions, four rules for the process, without kicks.
speeds <- c(-0.5, -0.1, -0.03, 0)
fade <- NULL
for (s in speeds) {
  sim <- simulateAlbert(Ainput = s, G = 0, times = times, shockTimes = shockTimes, dose = dose)
  fade <- rbind(fade, data.frame(sim, Ainput = paste("Ainput =", s)))
}
fade$Ainput <- factor(fade$Ainput, levels = paste("Ainput =", speeds))

p5c <- ggplot(fade, aes(x = time, y = Mood, colour = Ainput)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("#b0306a", "#2f9e6e", "#2a78d6", "#eb6834")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5c
if (savePlots) ggsave(file.path(plotFolder, "5c_fading_speeds.png"), p5c, width = 11, height = 5, dpi = 150)

# Q: Which processes build up, and which do not? Which ones has the therapy
#    "worked" for 4 weeks after the last session?


# =============================================================================
#  PART 6: THINGS TO TRY
# =============================================================================
# 1. Therapy schedule: give 16 sessions twice a week
#    (shockTimes <- seq(from = 7, to = 59.5, by = 3.5)) with half the dose
#    (dose <- 1.5). Same total dose. Does the level that mood reaches change?
#    What does change? Check with Part 2.
#
# 2. Booster sessions: after the 8 weekly sessions, add 3 monthly sessions
#    (on days 84, 112, 140; extend times to 180 days). How much of the gain
#    do they keep?
#
# 3. Stopping early: stop after 4 sessions. How much of the gain is left 4
#    weeks later?
#
# 4. People differ: simulate six people who differ only in Ainput (from
#    -0.5 to 0) and plot them together. If you only measured their mood,
#    would you blame the therapy, or the people, for the differences?
