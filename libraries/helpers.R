

# Responses outside the interval of the individual median +/- 3 × the
# interquartile range are considered as outliers (Tukey 1977)
trim_iqr <- function(x) {
  nIQR = 3
  iqr <- quantile(x, 3/4, na.rm = TRUE) - quantile(x, 1/4, na.rm = TRUE)
  x1 <- ifelse (
    (x > (quantile(x, 3/4, na.rm = TRUE) + nIQR * iqr)) |
      (x < (quantile(x, 1/4, na.rm = TRUE) - nIQR * iqr)),
    NA, x
  )
  return(x1)
}



scale_this <- function(x) {
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}



hist_var <- function(df, name) {
  
  x <- eval(substitute(name), df)
  
  #' # Percent correct
  mu <- df %>% 
    group_by(id, grade_f) %>% 
    summarise(
      bysub_x = mean(x, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    group_by(grade_f) %>% 
    summarise(
      grp_mean = mean(bysub_x, na.rm = TRUE)
    )
  
  out <- df %>% 
    group_by(id, grade_f) %>% 
    summarise(
      pct_corr = mean(pct_corr, na.rm = TRUE)
    ) %>% 
    ungroup()
  
  p <- ggplot(out, aes(x=pct_corr, fill=grade_f)) +
    geom_density(alpha=.5)+
    geom_vline(data=mu, aes(xintercept=grp_mean, color=grade_f),
               linetype="dashed") +
    labs(title = "Percent correct") +
    theme(legend.position = "top", legend.title = element_blank())
  
  print(p)
}


## fit -- stan fit object
## param -- name of parameter *required*
## K -- number of components, set to 0 for level2 parameters
stan2coda <- function (fit, param, K=0) {
  tryCatch({
    mclist <- mcmc.list(lapply(1:ncol(fit),
                               function (chain) {
                                 extract <- as.array(fit,pars=param)[,chain,]
                                 if (is.null(dim(extract))) {
                                   ## Scalar parameter
                                   extract <- matrix(extract,ncol=1,
                                                     dimnames=list(NULL,param))
                                 }
                                 mcmc(extract)}))
    ## The Stan as.array function takes care of names
    ## except for scalars, which are handled above, so we should be good.
    mclist},
    error=function (e) {
      ## Will get here if Stan run stopped.
      print(e)
      NULL
    })
}






