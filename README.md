# Dynamic Systems Thinking for Psychologists

Course materials for **Dynamic Systems Thinking for Psychologists** (DSTP), a master's course in psychology at the University of Zurich.

Psychological processes such as mood, motivation or stress do not stand still: they change over time, recover from setbacks, and influence each other. This course teaches you to think about these processes as *dynamic systems* and to describe them with models you can simulate and fit to data. We start from regression models you already know and build up, step by step, to continuous-time models of how people change, how they influence each other, and how they respond to events.

Each week, the slides are uploaded after the lecture, together with the scripts and exercise sheets used in class.

## Course overview

| Week | Topic | What you will learn |
|---|---|---|
| [1](Lecture01) | Introduction | Why thinking in systems and simulations helps (warm-up: the Monty Hall problem) |
| [2](Lecture02) | Linear change | Describing change over time with the general linear model |
| [3](Lecture03) | Individual differences | People change in different ways: mixed models and growth curves |
| [4](Lecture04) | Continuous time | Change as a rule ($dy/dt$), simulated in small steps (Euler method) |
| [5](Lecture05) | State dependence | Equilibria, baselines and the pull back to them; a model of therapy |
| [6](Lecture06) | Noise | Process noise versus measurement noise; fitting models with `ctsem` |
| [7](Lecture07) | Coupling | When one person's state drives another's change; shared shocks; moderators |
| [8](Lecture08) | Time-dependent predictors | Switching influences on and off over time; thresholds |
| [9](Lecture09) | Shocks | Transient, persistent, dissipative and cumulative shocks; damped oscillations |
| [10](Lecture10) | Case studies | Applying everything to a real decision problem |

## What is in each folder

Every `LectureXX` folder contains:

- **Slides** (`.pptx`), uploaded after the lecture
- **R scripts** (`.R`) used in the lecture, to simulate and plot the models
- **Exercise sheets** (`.Rmd` or `.qmd`) for the in-class exercises
- A **ReadMe** listing which file is which

The [Assessment](Assessment) folder contains information about how the course is assessed.

## Getting started

1. **Download the materials.** Click the green **Code** button above and choose **Download ZIP**, or clone the repository:
   ```
   git clone https://github.com/mearistodemou/dynamic-systems-uzh.git
   ```
2. **Install R and RStudio** (free): [R](https://cran.r-project.org/) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/).
3. **Install the packages** used in the course. Run this once in R:
   ```r
   install.packages(c("ggplot2", "dplyr", "tidyr", "lme4", "data.table", "shiny", "rmarkdown"))
   ```
   Most scripts only need `ggplot2` and install it themselves if it is missing.
4. **Install `ctsem`** (needed from week 4 onwards for fitting continuous-time models):
   ```r
   install.packages("ctsem")
   ```
   `ctsem` uses Stan in the background. Installing it can take a while, and the first model fit is slow because the model has to be compiled. Do this *before* class.
5. **Open a script or exercise sheet** in RStudio. Run scripts line by line to see what each part does. For exercise sheets (`.Rmd`, `.qmd`), run the code chunks one at a time, or click **Knit** / **Render** to create the full document. Quarto (`.qmd`) files need [Quarto](https://quarto.org/docs/get-started/), which is included in recent RStudio versions.

## How to work with the scripts

The scripts are written to be read, not only run. Most of them simulate data from a model whose parameters you set yourself. The best way to learn from them is to:

- **Predict first:** before you run the code, sketch what you expect to happen.
- **Change one thing at a time:** change a single parameter, run it again, and explain the difference.
- **Break it:** try extreme values and find out where the model stops making sense.

## Questions

If you have questions about the course or the materials, send me an email: Michael E. Aristodemou, michael.aristodemou@psychologie.uzh.ch.

## License

The materials are shared under the [GNU General Public License v3.0](LICENSE).
