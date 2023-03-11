
stan_sdt <- function(SEED_INDEX, NITER, NBURNIN, df) {

  df$signal <-
    ifelse(as.character(df$target) != "Absent", 1, 0)

  df$say_signal <- ifelse(
    (df$signal == 1 & df$correct == 1) |
      (df$signal == 0 & df$correct == 0), 1, 0
  )

  df$std <- as.factor(
    ifelse(
      df$signal == 1 & df$say_signal == 1, "HI",
      ifelse(df$signal == 0 & df$say_signal == 1, "FA",
        ifelse(df$signal == 1 & df$say_signal == 0, "MI", "CR")
      )
    )
  )

  absent <- subset(df, target == "Absent")
  happy <- subset(df, target == "Happy")
  angry <- subset(df, target == "Angry")

  # df with target absent and target happy
  df_happy <- rbind(absent, happy)
  # df with target absent and target angry
  df_angry <- rbind(absent, angry)

  # target absent and target happy trials
  temp <- dplyr::select(df_happy, id, grade, gender, std)

  aa <- table(temp$id, temp$std)
  aaa <- data.frame(
    id = factor(rownames(aa)),
    CR = aa[, 1], FA = aa[, 2], HI = aa[, 3], MI = aa[, 4]
  )

  bb <- df_happy %>%
    group_by(id, grade, gender) %>%
    summarise(
      m = mean(rt, na.rm = TRUE)
    )
  bb$m <- NULL
  bb$id <- factor(bb$id)

  df_h <- merge(aaa, bb, by = "id")
  rm(temp, aa, aaa, bb)

  # target absent and target angry trials
  temp <- dplyr::select(df_angry, id, grade, gender, std)

  aa <- table(temp$id, temp$std)
  aaa <- data.frame(
    id = factor(rownames(aa)),
    CR = aa[, 1], FA = aa[, 2], HI = aa[, 3], MI = aa[, 4]
  )

  bb <- df_angry %>%
    group_by(id, grade, gender) %>%
    summarise(
      m = mean(rt, na.rm = TRUE)
    )
  bb$m <- NULL
  bb$id <- factor(bb$id)

  df_a <- merge(aaa, bb, by = "id")

  # prepare data for stan analysis

  # df_h <- subset(tot_happy, grade == '9th Grade')
  # df_a <- subset(tot_angry, grade == '9th Grade')

  # write.csv(df_h, "females1happy.csv", row.names=FALSE)
  # write.csv(df_a, "females1angry.csv", row.names=FALSE)
  # df_h <- read.csv('females1happy.csv')
  # df_a <- read.csv('females1angry.csv')
  
  NCHAINS <- 1 # must be 1!
  THIN <- 1
  
  niter <- NITER
  chains <- NCHAINS
  thin <- THIN
  nburnin <- NBURNIN
  
  SEED <- c(134523, 2436423, 2462466, 2462462)

  for (dataset in 1:2) { # analyze both conditions
    if (dataset == 1) {
      data <- df_a[complete.cases(df_a), ]
    } # the induction data
    if (dataset == 2) {
      data <- df_h[complete.cases(df_h), ]
    } # the deduction data

    h <- data[, 4]
    f <- data[, 3]
    MI <- data[, 5]
    CR <- data[, 2]
    s <- h + MI
    n <- f + CR
    s <- s # s[1]
    n <- n # n[1] # Each subject gets same number of signal and noise trials
    k <- nrow(data)

    data <- list(h = h, f = f, s = s, n = n, k = k) # To be passed on to Stan

    myinits <- list(
      list(
        deltac = rep(0, k), deltad = rep(0, k), xic = .5, xid = .5,
        muc = 0, mud = 0, lambdac = 1, lambdad = 1
      )
    )
    # Parameters to be monitored
    parameters <- c("mud", "muc", "sigmad", "sigmac")
    
    stan_model <- here("scripts", "stan_model.stan")
    
    if (dataset == 1) {
      isamples <- stan(
        file = stan_model,
        data = data,
        init = myinits, 
        pars = parameters,
        iter = niter,
        chains = chains,
        thin = thin,
        warmup = nburnin, 
        control = list(adapt_delta = 0.99, max_treedepth = 15),
        seed = SEED[SEED_INDEX] 
      )
    }
    if (dataset == 2) {
      dsamples <- stan(
        fit = isamples,
        data = data,
        init = myinits, 
        pars = parameters,
        iter = niter,
        chains = chains,
        thin = thin,
        warmup = nburnin, 
        control = list(adapt_delta = 0.99, max_treedepth = 15),
        seed = SEED[SEED_INDEX] 
      )
    }
  }
  # Now the values for the monitored parameters are in the "isamples" and
  # "dsamples "objects, ready for inspection.
  post <- list()
  post[[1]] <- isamples
  post[[2]] <- dsamples
  
  post
}



draw_figure_sdt <- function(NITER, isamples, dsamples) {
  niter <- NITER
  keepi <- 10000
  keep <- sample(niter, keepi)
  
  imud <- rstan::extract(isamples)$mud
  imuc <- rstan::extract(isamples)$muc
  d.imuc <- density(imuc, adjust = 5)
  
  dmud <- rstan::extract(dsamples)$mud
  dmuc <- rstan::extract(dsamples)$muc
  d.dmuc <- density(dmuc, adjust = 5)
  
  layout(
    matrix(
      c(1, 2, 3, 0), 2, 2,
      byrow = TRUE
    ),
    width = c(2 / 3, 1 / 3),
    heights = c(2 / 3, 1 / 3)
  )
  # layout.show()
  
  # points
  # mar – A numeric vector of length 4, which sets the margin sizes in 
  # the following order: bottom, left, top, and right. 
  # The default is c(5.1, 4.1, 4.1, 2.1).
  par(mar = c(2, 2, 1, 0))
  plot(imud[keep], imuc[keep],
       pch = 2, cex = 0.2, 
       xlab = "", ylab = "", axes = F, xlim = c(1, 4),
       ylim = c(-1.0, 1.0), bty = "n"
  )
  points(dmud[keep], dmuc[keep], col = "gray", pch = 2, cex = 0.2)
  # box(lty = 1)
  legend("topleft",
         legend = c("Target happy", "Target angry"), bty = "n",
         cex = 1.3, col = c("gray", "black"), pch = c(19, 19)
  )
  
  # right plot
  par(mar = c(2, 0, 1, 4))
  plot(d.imuc$y, d.imuc$x,
       xlim = rev(c(0, 13)), type = "l", axes = FALSE, xlab = "",
       ylab = "", ylim = c(-1.0, 1.0), lwd = 2
  )
  lines(d.dmuc$y, d.dmuc$x, col = "gray", lwd = 2)
  axis(4)
  mtext(expression(paste(mu)["c"]), side = 4, line = 2.3, cex = 1.3)
  # box(lty = 1)
  
  # bottom plot
  par(mar = c(4, 2, 0, 0))
  plot(density(imud, adjust = 5),
       zero.line = FALSE, main = "", ylab = "", xlab = "",
       cex.lab = 1.3, axes = FALSE, xlim = c(1, 4), ylim = c(0, 3.95), lwd = 2
  )
  lines(density(dmud, adjust = 2), col = "gray", lwd = 2)
  axis(1, at = c(1, 2, 3, 4))
  mtext(expression(paste(mu)["d"]), side = 1.2, line = 2, cex = 1.3)
  # box(lty = 1)
}



compute_hpdi <- function(xs, prob = 0.95) {
  x_sorted <- sort(xs)
  n <- length(xs)
  
  num_to_keep <- ceiling(prob * n)
  num_to_drop <- n - num_to_keep
  
  possible_starts <- seq(1, num_to_drop + 1, by = 1)
  # Just count down from the other end
  possible_ends <- rev(seq(from = n, length = num_to_drop + 1, by = -1))
  
  # Find smallest interval
  span <- x_sorted[possible_ends] - x_sorted[possible_starts]
  edge <- which.min(span)
  edges <- c(possible_starts[edge], possible_ends[edge])
  
  # My requirement: length of span interval must be same as number to keep.
  # Other methods produce intervals that are 1 longer.
  stopifnot(length(edges[1]:edges[2]) == num_to_keep)
  
  x_sorted[edges]
}



eff_size <- function(mu1, mu2, sigma1, sigma2) {
  (mu1 - mu2) / sqrt((sigma1^2 + sigma2^2) / 2)
}

