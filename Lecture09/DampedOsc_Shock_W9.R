#######################################
# Adding a dampening oscillator shock
# Michael E. Aristodemou
# Week 9: 13.11.2025
#######################################

# install package if it is missing
if (!requireNamespace(c("ggplot2","dplyr","tidyr"), quietly = TRUE)) {
  install.packages(c("ggplot2","dplyr","tidyr"))
}

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# --- parameters ---
NSubjects <- 20
times <- seq(0, 100, 1)
Nobs <- length(times)
Nsteps <- 200            
dt <- 1 / Nsteps

# Affect model
A <- -0.1               # continuous-time state dependence
B <- 1                  # continuous intercept
G <- 0.2                # system noise coefficient on Affect

# Damped oscillator for InputState u (with velocity w)
omega0 <- 0.5           # natural frequency (rad / time unit)
zeta   <- 0.25          # damping ratio (>0). Larger = faster decay
M      <- -0.4          # drive applied ONLY in the interval that lands on inputTime

initialAffect <- rnorm(NSubjects, mean = 5, sd = 2)
inputTime <- times[ceiling(length(times)/2)]   # mid-window

# --- storage ---
data <- data.frame(
  Subject = rep(NA, NSubjects * Nobs),
  Time = rep(NA, NSubjects * Nobs),
  Affect = rep(NA, NSubjects * Nobs),
  InputState = rep(NA, NSubjects * Nobs)
)

row <- 0
for (subi in 1:NSubjects) {
  # initial states
  AffectState <- initialAffect[subi]
  u <- 0  # InputState position (latent input)
  w <- 0  # InputState velocity
  
  for (obsi in 1:Nobs) {
    
    if (obsi > 1) {
      # integrate over the unit interval to the next observation
      for (stepi in 1:Nsteps) {
        # ----- input oscillator dynamics -----
        du <- w
        # external drive M acts during the interval landing on inputTime
        drive <- if (times[obsi] == inputTime) M else 0
        dw <- -2 * zeta * omega0 * w - (omega0^2) * u + drive
        
        # Euler updates for the oscillator (shock)
        u <- u + du * dt
        w <- w + dw * dt
        
        # ----- affect dynamics -----
        dAffect <- A * AffectState + B + u # u is the state of the shock at the current step
        AffectState <- AffectState + dAffect * dt +
          G * rnorm(1, mean = 0, sd = sqrt(dt))
      }
    }
    
    row <- row + 1
    data$Subject[row] <- subi
    data$Time[row] <- times[obsi]
    data$Affect[row] <- AffectState + rnorm(1, 0, 0.05)  # small measurement noise
    data$InputState[row] <- u
  }
}

# Indicator column for plotting the intervention marker
data$Input <- as.integer(data$Time == inputTime)

# --- plots ---
plot_data <- data %>%
  select(Subject, Time, Affect, InputState) %>%
  pivot_longer(c(Affect, InputState), names_to = "Series", values_to = "Value")

p1=ggplot(plot_data, aes(x = Time, y = Value, color = as.factor(Subject))) +
  geom_line() +
  geom_point(size = 0.6) +
  facet_grid(rows = vars(Series),
             scales = "free_y",
             labeller = as_labeller(c(Affect = "Affect",
                                      InputState = "Latent input state"))) +
  geom_vline(xintercept = inputTime, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Time (weeks)", y = NULL)
