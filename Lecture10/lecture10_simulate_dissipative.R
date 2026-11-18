# =============================================================================
#  LECTURE 10 -- SIMULATE DATA: A DISSIPATIVE SHOCK
#  (a shock that starts a process of its own, which fades)
#  Dynamic Systems Thinking in Psychology
#
#  Example: on day 5, Albert has a therapy session. The session does not
#  change his mood in one instant. It starts something (new insight, hope)
#  that pushes his mood for a while, and that slowly fades. That "something"
#  is a process of its own, with its own rate of change:
#
#     dMood  = (A * Mood + B + C * Input) dt + G dW
#     dInput = (Ainput * Input) dt              (+ dose, at the shock)
#
#     Input   = the shock process. It starts at 0, jumps by 'dose' when the
#               shock arrives, and then fades by its own rule.
#     Ainput  = the shock's own state dependence (< 0: it fades)
#     C       = how strongly the shock process pushes mood (the coupling)
#
#  In matrix form this is the coupled model from Lecture 7 (Albert and his
#  rabbit), with the shock process in the place of the rabbit:
#
#     d/dt (Mood, Input) = | A  C      | (Mood, Input) + (B, 0)
#                          | 0  Ainput |
#
#  The 0 in the bottom left: Albert's mood does not change the shock process.
#  We only measure Albert's mood. The shock process is latent.
#
#  Compare with lecture10_simulate_transient.R, where the shock hits mood
#  directly and has no process of its own.
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

shockTimes <- 5                    # the session is on day 5 (must be one of the rating times)
dose <- 10                         # the session starts a process of size 10 ...
Ainput <- -0.5                     # ... which fades: half of it is gone after log(2)/0.5 = 1.4 days

seed <- 2026                       # same random kicks every time you run this
savePlots <- TRUE                  # TRUE: save every plot in Part 5 as a png file
plotFolder <- "plots_dissipative"  # folder for the png files (made in your working directory)


# =============================================================================
#  PART 2: WHAT TO EXPECT
# =============================================================================
# At any moment, the shock process gives mood a temporary target: the mood at
# which Albert's own pull (A * Mood + B) exactly cancels the push (C * Input):
#
#     target = restMood + C * Input / (-A)
#
# Mood always moves towards this target. The shock process fades, so the
# target moves back down, and mood chases it.

cat("Without any shock, mood rests at:", restMood, "\n")
cat("Right after the shock, the target jumps to:", restMood + C * dose / (-A), "\n")
cat("Half of the shock process is gone after:", round(log(2) / -Ainput, 2), "days\n")
cat("In the end, the process is gone and mood returns to:", restMood, "\n")
cat("A transient shock with the same total effect on mood would need a dose of:", C * dose / -Ainput, "\n")

# Q: Sketch the shock process and Albert's mood before you run anything else.
#    When will mood be highest: at the shock, or later?

# =============================================================================
#  PART 3: THE SIMULATION FUNCTION
# =============================================================================
# The function from Lecture 5, with a second state (the shock process) that is
# updated in the same small steps, as with the rabbit in Lecture 7.
# With direct = TRUE, the same shock instead hits mood directly (a transient
# shock), so that we can compare the two.

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
        dInput <- Ainput*InputState                      #NEW: the shock process has its own rule for change
        kick <- G * sqrt(dt) * rnorm(1)                  #random kick, variance G^2 * dt
        MoodState <- MoodState + dMood * dt + kick
        InputState <- InputState + dInput * dt
      }
    }
    if(any(abs(times[i] - shockTimes) < 1e-8)){          #does a shock arrive at this moment?
      if(direct) MoodState <- MoodState + dose           #transient: the shock hits mood directly
      else InputState <- InputState + dose               #dissipative: the shock starts a process
    }
    Mood[i] <- MoodState                       #store the states (just after a shock, if there was one)
    Input[i] <- InputState
  }
  data.frame(time = times, Mood = Mood, Input = Input)
}


# =============================================================================
#  PART 4: SIMULATE ALBERT
# =============================================================================

albert <- simulateAlbert(Ainput = Ainput, G = G, times = times)           #the shock starts a process
albertDirect <- simulateAlbert(Ainput = Ainput, direct = TRUE, G = G, times = times)  #the same shock, transient
albert$target <- restMood + C * albert$Input / (-A)                       #the target from Part 2
head(albert)

# write.csv(albert, "lecture10_data_dissipative.csv", row.names = FALSE)


# =============================================================================
#  PART 5: LOOK AT THE DATA
# =============================================================================
if (savePlots) dir.create(plotFolder, showWarnings = FALSE)

# --- 5a. Albert and the shock process over time --------------------------------
# Two panels: the mood we measure, and the shock process that nobody measures.
panels <- c("Mood (measured)", "Shock process (not measured)")
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
       title = "Albert's mood chases a target (dashed) that the shock process moves")
p5a
if (savePlots) ggsave(file.path(plotFolder, "5a_mood_and_process.png"), p5a, width = 11, height = 7, dpi = 150)

# Q: Does mood jump at the shock? When is mood highest, and where is the
#    target at that moment?
# Q: Mood keeps rising after the shock, even while the shock process is
#    already fading. Why?

# --- 5b. Dissipative versus transient ---------------------------------------------
both <- rbind(data.frame(time = albert$time, Mood = albert$Mood, version = "dissipative: shock starts a process"),
              data.frame(time = albertDirect$time, Mood = albertDirect$Mood, version = "transient: shock hits mood directly"))

p5b <- ggplot(both, aes(x = time, y = Mood, colour = version)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#2a78d6", "#b0306a")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5b
if (savePlots) ggsave(file.path(plotFolder, "5b_dissipative_vs_transient.png"), p5b, width = 11, height = 5, dpi = 150)

# Q: Both versions get the same random kicks and the same dose. Where do they
#    differ? Could you tell them apart from noisy ratings?

# --- 5c. How fast does the shock process fade? ----------------------------------
# The same shock, with three different rules for the shock process, without
# kicks so that you see the pure response.
speeds <- c(-2, -0.5, -0.1)                    #Ainput values: fades fast ... slowly
fade <- NULL
for (s in speeds) {
  sim <- simulateAlbert(Ainput = s, G = 0, times = times)
  fade <- rbind(fade, data.frame(sim, Ainput = paste("Ainput =", s)))
}
fade$Ainput <- factor(fade$Ainput, levels = paste("Ainput =", speeds))

p5c <- ggplot(fade, aes(x = time, y = Mood, colour = Ainput)) +
  geom_vline(xintercept = shockTimes, linetype = "dotted", colour = "grey40") +
  geom_hline(yintercept = restMood, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("#b0306a", "#2a78d6", "#2f9e6e")) +
  theme_bw(base_size = 16) +
  labs(x = "Time (days)", y = "Mood", colour = NULL) +
  theme(legend.position = "bottom")
p5c
if (savePlots) ggsave(file.path(plotFolder, "5c_fading_speeds.png"), p5c, width = 11, height = 5, dpi = 150)

# Q: Which shock process makes mood look most like a transient shock?
# Q: The slower the process fades, the bigger and later the peak. Why?


# =============================================================================
#  PART 6: THINGS TO TRY
# =============================================================================
# 1. When does a process look like a transient shock? Set Ainput to -5 and
#    dose to 50. Compare 5b: what transient dose would give the same picture?
#    (Hint: the total push of the process is C * dose / -Ainput.)
#
# 2. Who is fast? Set A <- -0.1 and Ainput <- -0.5 (Albert slow, shock fast),
#    and then A <- -0.5 and Ainput <- -0.1 (Albert fast, shock slow). Use
#    G <- 0. Compare the mood curves. What do you notice? (Remember to keep
#    B <- -A * restMood.)
#
# 3. Make the coupling negative (C <- -1): the session first brings up
#    painful memories. What does mood do now?
#
# 4. Give Albert a second session (shockTimes <- c(5, 8)). Does the second
#    session add to the first? Compare with the same two shocks in
#    lecture10_simulate_transient.R.
