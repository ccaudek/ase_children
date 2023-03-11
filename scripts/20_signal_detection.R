# Project: ASE with children (dynamic displays).
# Participants: 1st, 5th and 9th grade children.
#
# This code performs the Bayesian SDT analysis.
#
# Corrado Caudek



# read data  --------------------------------------------------------

library("here")
library("tidyverse")
library("rstan")
library("ggmcmc")
library("R.utils")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sourceDirectory(here("libraries"))

# read data
children <- read_csv(
  here("data", "input", "children_data.csv")
)

# remove accuracy responses for invalid RTs
children$correct <- ifelse(
  children$rt < 200, NA, children$correct
)


# select age group
mydata <- subset(children, grade == 9)

mydata$id <- as.numeric(factor(mydata$id))

NITER <- 10000
NBURNIN <- 2000

A_list <- list()
H_list <- list()

# Runs four chains in separate runs, with different seeds.
# The `i` index selects the seeds from a vector in sdt_func.R.
for (i in 1:4) {
  posterior <- stan_sdt(i, NITER, NBURNIN, mydata)
  
  a_samples <- posterior[[1]]
  A <- ggs(a_samples)
  A$Chain <- i
  A_list[[i]] <- A
  rm(A)
 
  h_samples <- posterior[[2]]
  H <- ggs(h_samples)
  H$Chain <- i
  H_list[[i]] <- H
  rm(H)
}

# angry face target
A <- do.call(
  "rbind", lapply(A_list, as.data.frame)
)
ggmcmc::ci(A)

A_wide <- tidyr::spread(A, Parameter, value)
apply(A_wide[, 3:6], 2, compute_hpdi)

# happy face target
H <- do.call(
  "rbind", lapply(H_list, as.data.frame)
)
ggmcmc::ci(H)

# saveRDS(A, here("data", "output", "posterior_samples", "A_5_9_age_grps.rds"))

draw_figure_sdt(NITER, a_samples, h_samples)


eff_size(2.87, 2.32, 1.06, 1.01)
eff_size(-0.0212, 0.253, 0.540, 0.555)

# 1st grade ---------------------------------------------------------
# ci(A)
# [1] "Sat Mar 23 07:58:19 2019"
# Parameter    low    Low  median   High  high
# 1 muc       -0.153 -0.132 -0.0212 0.0936 0.117
# 2 mud        2.60   2.65   2.87   3.11   3.15 
# 3 sigmac     0.440  0.454  0.540  0.644  0.668
# 4 sigmad     0.857  0.887  1.06   1.28   1.33 

# ci(H)
# [1] "Sat Mar 23 07:58:19 2019"
# Parameter   low   Low median  High  high
# 1 muc       0.121 0.143  0.253 0.368 0.391
# 2 mud       2.07  2.11   2.32  2.54  2.58 
# 3 sigmac    0.460 0.474  0.555 0.657 0.678
# 4 sigmad    0.823 0.848  1.01  1.20  1.24 

# 0.127 / 0.415
# [1] 0.3060241

# 5th grade ---------------------------------------------------------
# ci(A)
# [1] "Sat Mar 23 08:36:13 2019"
# Parameter    low    Low median    High    high
# 1 muc       -0.230 -0.213 -0.127 -0.0388 -0.0209
# 2 mud        2.65   2.69   2.91   3.14    3.19  
# 3 sigmac     0.334  0.346  0.415  0.498   0.515 
# 4 sigmad     0.923  0.952  1.12   1.33    1.38  
#
# ci(H)
# [1] "Sat Mar 23 08:36:13 2019"
# Parameter     low     Low median  High  high
# 1 muc       -0.0480 -0.0308 0.0633 0.158 0.176
# 2 mud        2.29    2.32   2.51   2.71  2.75 
# 3 sigmac     0.387   0.399  0.471  0.557 0.576
# 4 sigmad     0.778   0.803  0.952  1.14  1.17 


# 9th grade ---------------------------------------------------------
# ci(A)
# [1] "Sat Mar 23 09:01:40 2019"
# Parameter     low     Low median   High   high
# 1 muc       -0.0545 -0.0437 0.0131 0.0692 0.0810
# 2 mud        2.90    2.93   3.10   3.27   3.31  
# 3 sigmac     0.135   0.147  0.206  0.270  0.283 
# 4 sigmad     0.648   0.673  0.804  0.961  0.996 

# ci(H)
# [1] "Sat Mar 23 09:01:40 2019"
# Parameter   low   Low median  High  high
# 1 muc       0.124 0.135  0.187 0.241 0.251
# 2 mud       2.55  2.58   2.74  2.90  2.94 
# 3 sigmac    0.130 0.140  0.194 0.253 0.264
# 4 sigmad    0.646 0.669  0.794 0.946 0.981


# 5th and 9th grade -------------------------------------------------
# [1] "Fri Mar 22 14:51:23 2019"
# ci(A)
# Parameter    low    Low  median     High    high
# 1 muc       -0.123 -0.113 -0.0595 -0.00580 0.00505
# 2 mud        2.85   2.88   3.02    3.16    3.19   
# 3 sigmac     0.283  0.291  0.335   0.385   0.395  
# 4 sigmad     0.856  0.876  0.987   1.11    1.14   

# ci(H)
# [1] "Fri Mar 22 14:51:23 2019"
# Parameter    low    Low median  High  high
# 1 muc       0.0580 0.0691  0.124 0.179 0.190
# 2 mud       2.49   2.51    2.64  2.77  2.80 
# 3 sigmac    0.313  0.321   0.364 0.413 0.423
# 4 sigmad    0.777  0.794   0.896 1.01  1.03 

eff_size(3.02, 2.64, 0.987, 0.896)
eff_size(-0.0595, 0.124, 0.335, 0.364)



# eof


