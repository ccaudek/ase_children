
diagnostics <- function(m) {
  pp_check(m)  # shows dens_overlay plot by default
  pp_check(m, type = "error_hist", nsamples = 20)
  pp_check(m, type = "scatter_avg", nsamples = 100)
  pp_check(m, type = "stat_2d")
}




