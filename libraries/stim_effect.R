
stim_effect <- function(df, GROUP) {
  
  if (GROUP == "HTA") { # set reference level for HTA group
    df$group <- factor(
      df$group, levels = c("HTA", "ATA")
    )
    contrasts(df$group)
    destfile <-
      here("data", "output", "posterior_samples", "fit_hta")
  } else if (GROUP == "ATA") {
    df$group <- factor(
      df$group, levels = c("ATA", "HTA")
    )
    contrasts(df$group)
    destfile <-
      here("data", "output", "posterior_samples", "fit_ata")
  } else {
    stop("ERROR: parameter GROUP must be HTA or ATA.")
  }
  
  # scale RTs by MAD
  df$y <- df$rt / mad(df$rt)
  # model's formula
  bform1 <- bf(
    y ~ target * group + (1 + target | id)
  )
  
  # # get_prior(
  #   y ~ target * group + (1 + target | id),
  #   family = shifted_lognormal,
  #   data = younger_grps_df
  # )
  
  # # plot prior
  # x_values <- seq(-15, 15, length.out = 1000)
  # data.frame(x_values) %>%
  #   ggplot(aes(x_values)) +
  #   stat_function(fun = dst, args = list(nu = 3, mu = 0, sigma = 2.5))
  
  # mildly informative priors
  priors <- c(
    set_prior("student_t(3, 0,  2.5)", class = "b"),
    set_prior("student_t(3, 1, 10.0)", class = "Intercept"),
    set_prior("student_t(3, 0, 10.0)", class = "sigma")
  )
  
  if (!file.exists(destfile)) {
    fit <- brm(
      bform1, 
      family = shifted_lognormal,
      prior = priors,
      data = df,
      control = list(adapt_delta = 0.94,  max_treedepth = 15),
      future = TRUE, 
      file = destfile,
      seed = 345654
    )
  } else {
    fit <- readRDS(destfile)
  }
  
  fit
}




