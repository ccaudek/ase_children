# Project: ASE with dynamic displays.
# Participants: 1st, 5th and 9th grade children.
#
# This code generates reads the necessary packages and sets some options.
# 
# Corrado Caudek
# Last updated: "Sat Mar 16 13:40:58 2019"


# Prelims -----------------------------------------------------------------

# This line of code installs the pacman page if you do not have it 
# installed - if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

# devtools::install_github("thomasp85/patchwork")

pacman::p_load(
  "tidyverse", "magrittr", "rstan", "brms", "brmstools", "bayesplot", 
  "colorblindr", "gridExtra", "ggthemes", "ggrepel", 
  "tidybayes", "LaplacesDemon", "tictoc", "future", 
  "mice", "patchwork", "tibble",
  "bayesboot", "forcats", "ggmcmc", "R.utils"
)

plan(multiprocess)

bayesplot_theme_set(theme_default(base_size = 13, base_family = "sans"))
theme_set(theme_default(base_size = 13, base_family = "sans"))
# color_scheme_set("brightblue")
# theme_set(theme_default())
