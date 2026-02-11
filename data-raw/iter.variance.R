# script to determine the variance in optimizsed sample size given the number of
# iterations for calculating the mse and the number of posterior samples.


# Setup Config ------------------------------------------------------------


# sys settings
Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  BLAS_NUM_THREADS = "1"
)

# cran repo
options(repos=c(CRAN="https://ftp.belnet.be/mirror/CRAN/"))

# setup for HPC or local
HPC = TRUE

if (HPC) {
  # set root path
  root.dir <- "/home4/p310779/sspLNIRT/"

  # set save path
  save.dir <- "/home4/p310779/sspLNIRT/data-raw/results/"
  dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)

} else {
  # set root path
  root.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/"

  # set save path
  save.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/results/"
  #dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)


}

# required functions
fct.names <- list(
  "R/fct_comp_rmse.R",
  "R/fct_item_par.R",
  "R/fct_optim_sample.R",
  "R/fct_person_par.R",
  "R/fct_rhat_LNIRT.R",
  "R/fct_scale_M.R",
  "R/fct_sim_jhm_data.R"
  )

# load to environment
invisible (
  lapply(fct.names, FUN = function(x) {
    source(paste0(root.dir, x))
  }))

# cores
if (HPC ) {
  # set cores
  n.cores <- future::availableCores() - 5
  cat("running with ", n.cores, "cores! \n\n")
  future::plan(future::multisession, workers = n.cores)
} else {
  n.cores <- 6
  future::plan(future::multisession, workers = n.cores)
}



# Run the Job -------------------------------------------------------------

# batches
batches <- split(1:100, cut(seq_len(100), breaks = 5, labels = FALSE))

# compute MSE
start.time = Sys.time()
for (i in 1:length(batches)) {

  result <- list()
  batch <- batches[[i]]
  for (k in 1:length(batch)) {
    res <- optim_sample(
      FUN = comp_rmse,
      thresh = .1,
      range = c(100,ub = 1000),
      out.par = 'alpha',
      iter = 100,
      K = 30,
      mu.person = c(0,0),
      mu.item = c(1,0,.5,1),
      meanlog.sigma2 = log(.6),
      cov.m.person = matrix(c(1,0.4,
                              0.4,1), ncol = 2, byrow = TRUE),
      cov.m.item = matrix(c(1, 0, 0, 0,
                            0, 1, 0, 0.4,
                            0, 0, 1, 0,
                            0, 0.4, 0, 1), ncol =  4, byrow = TRUE),
      sd.item         = c(.2, 1, .2, .5),
      cor2cov.item    = TRUE,
      sdlog.sigma2 = 0,
      XG = 6000,
      seed = NULL,
      rhat = 1.05)
    result[[k]] <- res
    rm(res)
  }
  saveRDS(result, paste0(save.dir, "ssp.variance", i))

  cat("Batch", i, "of", length(batches), "done!!!! \n\n")
  rm(result)
  gc()
}

end.time = Sys.time()
time.taken = end.time-start.time
print(time.taken)


# Results -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# load results
ssp.variance1 <- readRDS(paste0(save.dir, "ssp.variance1"))
ssp.variance2 <- readRDS(paste0(save.dir, "ssp.variance2"))
ssp.variance3 <- readRDS(paste0(save.dir, "ssp.variance3"))
ssp.variance4 <- readRDS(paste0(save.dir, "ssp.variance4"))
ssp.variance5 <- readRDS(paste0(save.dir, "ssp.variance5"))

## check convergence
res.names <- c(ssp.variance1, ssp.variance2, ssp.variance3, ssp.variance4, ssp.variance5
               )

summary(sapply(res.names, FUN = function(x) {
  x$comp.rmse$conv.rate
}))

# conv rates: > .83


# get ssp data
ssp.res <- as.data.frame(
  t(sapply(res.names, FUN = function(x) {
  as.numeric(c(x$N.best,
               x$N.curve,
               x$res.best,
               x$trace$time.taken))
})))
colnames(ssp.res) <- c("N.min", "N.curve", "res.best", "time.taken")

# summary stats
sum.stats <- ssp.res %>%
 summarise(
    across(
      c(N.min, N.curve, res.best, time.taken),
      list(
        mean  = ~ mean(.x, na.rm = TRUE),
        sd    = ~ sd(.x,   na.rm = TRUE),
        min   = ~ min(.x,  na.rm = TRUE),
        max   = ~ max(.x,  na.rm = TRUE),
        lb.sd = ~ mean(.x, na.rm = TRUE) - sd(.x, na.rm = TRUE),
        ub.sd = ~ mean(.x, na.rm = TRUE) + sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to  = c("variable", ".value"),
    names_pattern = "^(.*)_(mean|sd|min|max|lb\\.sd|ub\\.sd)$"
  ) %>%
  as.data.frame()


# histogram N.min
lines_N.min <- sum.stats %>%
  filter(variable == "N.min") %>%
  pivot_longer(
    cols = c(lb.sd, mean, ub.sd),
    names_to = "bound",
    values_to = "xint"
  )

gg_N.min <- ggplot(ssp.res, aes(x = N.min)) +
  geom_histogram(alpha = .5) +
  geom_vline(
    data = lines_N.min,
    aes(xintercept = xint, linetype = bound)
  ) +
  scale_linetype_manual(values = c(lb.sd = "dashed", mean = "solid", ub.sd = "dashed"))

# histogram N.curve
lines_N.curve <- sum.stats %>%
  filter(variable == "N.curve") %>%
  pivot_longer(
    cols = c(lb.sd, mean, ub.sd),
    names_to = "bound",
    values_to = "xint"
  )

gg_N.curve <- ggplot(ssp.res, aes(x = N.curve)) +
  geom_histogram(alpha = .5) +
  geom_vline(
    data = lines_N.curve,
    aes(xintercept = xint, linetype = bound)
  ) +
  scale_linetype_manual(values = c(lb.sd = "dashed", mean = "solid", ub.sd = "dashed"))

# compare the plots
shared_xlim <- range(c(ssp.res$N.min, ssp.res$N.curve), na.rm = TRUE)
wrap_plots(gg_N.min, gg_N.curve, ncol = 1) &
  scale_x_continuous(limits = shared_xlim)


# combined density plot
plot_df <- ssp.res %>%
  pivot_longer(cols = c(N.min, N.curve), names_to = "method", values_to = "N")

lines_df <- sum.stats %>%
  filter(variable %in% c("N.min", "N.curve")) %>%
  pivot_longer(cols = c(lb.sd, mean, ub.sd), names_to = "bound", values_to = "xint") %>%
  rename(method = variable)

ggplot(plot_df, aes(x = N, fill = method, color = method)) +
  geom_density(alpha = .3) +
  geom_vline(
    data = lines_df,
    aes(xintercept = xint, color = method, linetype = bound)
  ) +
  scale_linetype_manual(
    values = c(lb.sd = "dashed", mean = "solid", ub.sd = "dashed")
  ) +
  scale_fill_grey(name = "N Method", start = 0.6, end = 0.2) +
  scale_color_grey(name = "N Method", start = 0.6, end = 0.2) +
  labs(x = "Sample Size", y = "Density") +
  theme_minimal()


# density plot res.best
lines_res.best <- sum.stats %>%
  filter(variable == "res.best") %>%
  pivot_longer(
    cols = c(min, max),
    names_to = "bound",
    values_to = "xint"
  )

ggplot(ssp.res, aes(x = res.best)) +
  geom_density(alpha = .5) +
  geom_vline(
    data = lines_res.best,
    aes(xintercept = xint, linetype = bound)
  ) +
  scale_linetype_manual(values = c(min = "dashed", max = "dashed"))



##### check log-log curves
# extract power curves into a named list
power_curves <- lapply(res.names, function(x) {
  # raw tracking data
  N_vec   <- x$trace$track.N$N.temp
  res_vec <- x$trace$track.res$res.temp

  # fit log-log model
  fit <- lm(log(res_vec) ~ log(N_vec))
  a <- coef(fit)[1]
  b <- coef(fit)[2]

  # derive N.curve
  N_curve <- ceiling(exp((log(.1) - a) / b))

  list(
    N_vec   = N_vec,
    res_vec = res_vec,
    fit     = fit,
    a       = as.numeric(a),
    b       = as.numeric(b),
    N_curve = as.numeric(N_curve),
    r_sq    = summary(fit)$r.squared
  )
})

# tabular overview
do.call(rbind, lapply(power_curves, function(pc) {
  data.frame(a = pc$a, b = pc$b, N_curve = pc$N_curve, r_sq = pc$r_sq)
}))

# plot a single log-log relation
plot(log(power_curves[[1]]$N_vec), log(power_curves[[1]]$res_vec))
abline(power_curves[[1]]$fit, col = "red")

plot_power_curve(res.names, thresh = .05)
