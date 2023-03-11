# Project: ASE with children (dynamic displays).
# Participants: 1st, 5th and 9th grade children.
#
# This code performs the hierarchical ex-gaussian analysis.
#
# Corrado Caudek


library("here")
library("tidyverse")
library("rstan")
library("ggmcmc")
library("R.utils")
library("tidybayes")
library("coda")
library("ggthemes")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sourceDirectory(here::here("libraries"))

# read data
children <- rio::import(
  here("data", "input", "children_data.csv")
)

 # Stan model
stan_model <- here("scripts", "stan_model_exgauss.stan")


# Select age, condition, and correct responses 
mydata <- children %>% 
  dplyr::filter(
    grade == 9 & 
      target == "Happy",
      correct == 1 & 
      !is.na(rt)
  ) %>% 
  dplyr::select(id, rt = rt)

mydata$id <- as.numeric(factor(mydata$id))


# data list in the format required by Stan
stan_data = list(
  rt = mydata$rt_tukey / 1000, # vector containing all RTs of all subjects
  N = length(mydata$rt_tukey), # total number of trials
  J = length(unique(mydata$id)),  # number of subjects
  id = mydata$id
)


# Fit Stan model
niter <- 16000
SEED <- 7348597

fit_hier <- rstan::stan(
  file = stan_model,
  data = stan_data,
  chains = 4,
  iter = niter,
  warmup = 1000,
  cores = getOption("mc.cores", 1L),
  control = list(adapt_delta = 0.9999995, max_treedepth = 15),
  seed = SEED
)

# Posterior estimates
print(
  fit_hier, 
  c("mu_m", "mu_s", "tau"), 
  probs = c(0.025, 0.5, 0.975),
  digits = 4
)

# Convert stan to coda 
coda_samples <- stan2coda(fit_hier, param = c("mu_m", "mu_s", "tau"))

# Save results
saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade1_angry_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 3.2063  0.0010 0.1091 2.9957 3.2048 3.4242 11719 1.0003
# mu_s 0.9838  0.0007 0.0599 0.8695 0.9831 1.1046  7733 1.0008
# tau  1.5197  0.0012 0.1093 1.3086 1.5191 1.7385  8302 1.0005

saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade1_happy_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 3.3452  0.0010 0.1171 3.1183 3.3440 3.5770 14241 1.0003
# mu_s 1.0486  0.0007 0.0657 0.9238 1.0474 1.1818  9275 1.0003
# tau  1.4138  0.0012 0.1120 1.1994 1.4123 1.6377  8508 1.0003

saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade1_absent_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 3.3322  0.0008 0.1259 3.0858 3.3320 3.5804 22645 1.0002
# mu_s 0.9291  0.0006 0.0533 0.8282 0.9278 1.0375  8734 1.0006
# tau  1.5530  0.0010 0.1103 1.3416 1.5507 1.7742 12279 1.0003

saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade5_absent_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 2.7158  0.0009 0.0877 2.5487 2.7143 2.8931  9672 1.0006
# mu_s 0.9024  0.0006 0.0480 0.8122 0.9011 0.9998  6038 1.0010
# tau  1.2478  0.0010 0.1111 1.0405 1.2441 1.4744 12050 1.0005

saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade5_angry_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 2.6527  0.0008 0.0768 2.5062 2.6511 2.8082  8674 1.0004
# mu_s 0.8620  0.0006 0.0413 0.7839 0.8609 0.9457  4740 1.0008
# tau  1.2593  0.0010 0.1103 1.0549 1.2553 1.4877 12932 1.0003

saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade5_happy_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 2.7089  0.0006 0.0699 2.5743 2.7078 2.8485 11764 1.0002
# mu_s 0.8934  0.0004 0.0388 0.8192 0.8927 0.9719  7908 1.0003
# tau  1.2145  0.0009 0.1068 1.0141 1.2108 1.4330 13946 1.0001

saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade9_absent_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 2.7964  0.0003 0.0569 2.6850 2.7963 2.9080 35598 1.0000
# mu_s 0.4223  0.0002 0.0263 0.3718 0.4219 0.4749 27027 1.0000
# tau  0.4120  0.0003 0.0260 0.3608 0.4120 0.4627 10753 1.0004


saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade9_angry_coda_samples.rds")
)
#        mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 1.5830  0.0006 0.0423 1.5005 1.5827 1.6660  5391 1.0006
# mu_s 0.5057  0.0004 0.0265 0.4538 0.5057 0.5580  4639 1.0008
# tau  0.5753  0.0007 0.0416 0.4928 0.5753 0.6565  3947 1.0008


saveRDS(
  coda_samples, 
  file = here("data", "output", "posterior_samples", 
              "grade9_happy_coda_samples.rds")
)
#       mean se_mean     sd   2.5%    50%  97.5% n_eff   Rhat
# mu_m 1.8098  0.0005 0.0481 1.7141 1.8109 1.9044  8057 1.0001
# mu_s 0.5689  0.0003 0.0210 0.5281 0.5680 0.6111  3708 1.0043
# tau  0.4964  0.0006 0.0414 0.4143 0.4962 0.5783  4782 1.0003

#--------------------------------------------------------------------
# Figure of the posterior distribution for each condition

# Convert coda-object codaSamples to matrix object for easier handling.
# Note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]

# grade 1 absent ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
          "grade1_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_mu_m_no <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
          "grade1_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_mu_s_no <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
          "grade1_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_tau_no <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 1 angry ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade1_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_mu_m_a <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade1_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_mu_s_a <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade1_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_tau_a <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 1 happy ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade1_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_mu_m_h <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade1_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_mu_s_h <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade1_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g1_tau_h <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 5 absent ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_mu_m_no <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_mu_s_no <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_tau_no <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 5 angry ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_mu_m_a <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_mu_s_a <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_tau_a <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 5 happy ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_mu_m_h <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_mu_s_h <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade5_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g5_tau_h <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 9 absent ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_mu_m_no <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_mu_s_no <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_absent_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_tau_no <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 9angry ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_mu_m_a <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_mu_s_a <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_angry_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_tau_a <- mcmcChain[, "tau"] # or mu.he or mu.hn

# grade 9 happy ----------------------------------------------------
rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_mu_m_h <- mcmcChain[, "mu_m"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_mu_s_h <- mcmcChain[, "mu_s"] # or mu.he or mu.hn

rm(coda_samples)
coda_samples <- readRDS(here("data", "output", "posterior_samples", 
                             "grade9_happy_coda_samples.rds"))
mcmcChain = as.matrix(coda_samples)
g9_tau_h <- mcmcChain[, "tau"] # or mu.he or mu.hn


# data.frame for plot -----------------------------------------------

n <- length(g9_mu_m_h)
df <- data.frame(
  y = c(
    g1_mu_m_no, g1_mu_s_no, g1_tau_no,
    g1_mu_m_a,  g1_mu_s_a,  g1_tau_a,
    g1_mu_m_h,  g1_mu_s_h,  g1_tau_h,
    g5_mu_m_no, g5_mu_s_no, g5_tau_no,
    g5_mu_m_a,  g5_mu_s_a,  g5_tau_a,
    g5_mu_m_h,  g5_mu_s_h,  g5_tau_h,
    g9_mu_m_no, g9_mu_s_no, g9_tau_no,
    g9_mu_m_a,  g9_mu_s_a,  g9_tau_a,
    g9_mu_m_h,  g9_mu_s_h,  g9_tau_h),
  Grade = rep(c('1st Grade', '5th Grade', '9th Grade'), each = 9*n),
  Target = rep(rep(c('N', 'A', 'H'), each=3*n), 3),
  parameter = rep(rep(c('mu', 'sigma', 'tau'), each=n), 9)
)


# plot --------------------------------------------------------------

dodge <- position_dodge(width = 0.7)

P1 <- filter(df, parameter == "mu") %>%
  ggplot(aes(x = Target, y = y)) +
  facet_wrap(~ Grade) +
  geom_violin(position = dodge, fill = "grey80") +
  geom_boxplot(width=.33, outlier.colour=NA, position = dodge) +
  labs(x = "Target") +
  theme_tufte(base_size = 14, base_family = "Helvetica", ticks = TRUE) +
  ylab( expression(Group~Mean~mu))

P2 <- filter(df, parameter == "sigma") %>%
  ggplot(aes(x = Target, y = y)) +
  facet_wrap(~ Grade) +
  geom_violin(position = dodge, fill = "grey80") +
  geom_boxplot(width=.33, outlier.colour=NA, position = dodge) +
  labs(x = "Target") +
  theme_tufte(base_size = 14, base_family = "Helvetica", ticks = TRUE) +
  ylab( expression(Group~Mean~sigma))

P3 <- filter(df, parameter == "tau") %>%
  ggplot(aes(x = Target, y = y)) +
  facet_wrap(~ Grade) +
  geom_violin(position = dodge, fill = "grey80") +
  geom_boxplot(width=.33, outlier.colour=NA, position = dodge) +
  labs(x = "Target") +
  theme_tufte(base_size = 14, base_family = "Helvetica", ticks = TRUE) +
  ylab( expression(Group~Mean~tau))

multiplot(P1, P2, P3)



# Multiple plot function --------------------------------------------
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# eof
