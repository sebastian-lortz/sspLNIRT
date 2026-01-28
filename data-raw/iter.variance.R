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
  "R/fct_geweke_LNIRT.R",
  "R/fct_item_par.R",
  "R/fct_optim_sample.R",
  "R/fct_person_par.R",
  "R/fct_rhat_LNIRT.R",
  "R/fct_scale_M.R",
  "R/fct_sim_jhm_data.R",
  "R/utils_helpers.R"
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


# generate the design conditions for low and high N
design <- expand.grid(
  XG = c(6000),
  iter = c(100)
)

# storage
result.list <- list()

# compute MSE
start.time = Sys.time()
for (i in 1:nrow(design)) {

  result <- list()
  iter <- design$iter[i]
  XG <- design$XG[i]

  for (k in 1:100) {
    res <- optim_sample(
      FUN = comp_rmse,
      thresh = .01,
      range = c(100,ub = 1000),
      out.par = 'alpha',
      iter = iter,
      K = 30,
      mu.person = c(0,0),
      mu.item = c(1,0,1,1),
      meanlog.sigma2 = log(.6),
      cov.m.person = matrix(c(1,0.4,
                              0.4,1), ncol = 2, byrow = TRUE),
      cov.m.item = matrix(c(1, 0, 0, 0,
                            0, 1, 0, 0.4,
                            0, 0, 1, 0,
                            0, 0.4, 0, 1), ncol =  4, byrow = TRUE),
      sd.item         = c(.2, 1, .2, .5),
      cor2cov.item    = TRUE,
      sdlog.sigma2 = 0.2,
      XG = XG,
      ssp.seed = NULL,
      rhat = 1.05)
    result[[k]] <- res
    cat("iteration", k, "of", 100, "done!!!! \n\n")
    rm(res)
  }
  saveRDS(result, paste0(save.dir, "ssp.variance.no.seed.", 2))

  result.list[[i]] <- result
  cat("Design row", i, "done!!!! \n\n")
  gc()
}
saveRDS(result.list, paste0(save.dir, "ssp.variance.no.seed.list"))

end.time = Sys.time()
time.taken = end.time-start.time
print(time.taken)


# Results -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)

# load results
ssp.variance.no.seed.1 <- readRDS(paste0(save.dir, "ssp.variance.no.seed.1"))

## check convergence
res.names <- list(ssp.variance.no.seed.1)

lapply(res.names, FUN = function(z) {
  summary(
    unlist(lapply(z, FUN = function(x) {
      lapply(x[[6]], FUN = function(y) nrow(y))
    }))
  )
})

# conv rates: > .69


# get ssp data
ssp.res <- lapply(res.names, FUN = function(x) {
  t(sapply(x, FUN = function(y) y[c(1, 2, 3, 7)]))
})

ssp.data <- do.call(rbind, lapply(ssp.res, FUN = function(x) {
  as.data.frame(cbind(
    N.best     = as.numeric(x[, 1]),
    res.best   = as.numeric(x[, 2]),
    reps       = as.numeric(x[, 3]),
    time.taken = as.numeric(x[, 4])
  ))
}))

# summary stats
sum.stats <- ssp.data %>%
  summarise(
    across(
      c(N.best, res.best, reps, time.taken),
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


# density plot N.best
lines_N.best <- sum.stats %>%
  filter(variable == "N.best") %>%
  pivot_longer(
    cols = c(lb.sd, mean, ub.sd),
    names_to = "bound",
    values_to = "xint"
  )

ggplot(ssp.data, aes(x = N.best)) +
  geom_histogram(alpha = .5) +
  geom_vline(
    data = lines_N.best,
    aes(xintercept = xint, linetype = bound)
  ) +
  scale_linetype_manual(values = c(lb.sd = "dashed", mean = "solid", ub.sd = "dashed"))


# density plot res.best
lines_res.best <- sum.stats %>%
  filter(variable == "res.best") %>%
  pivot_longer(
    cols = c(min, max),
    names_to = "bound",
    values_to = "xint"
  )

ggplot(ssp.data, aes(x = res.best)) +
  geom_density(alpha = .5) +
  geom_vline(
    data = lines_res.best,
    aes(xintercept = xint, linetype = bound)
  ) +
  scale_linetype_manual(values = c(min = "dashed", max = "dashed"))




### OLD Design with tolerance stopping criterion ####
# check if variance is dependent on number of reps
ssp.data %>%
  group_by(condition) %>%
  filter(reps >=11) %>%
  summarise(sd = sd(N.best))
ssp.data %>%
  group_by(condition) %>%
  filter(reps >=10) %>%
  summarise(sd = sd(N.best))
ssp.data %>%
  group_by(condition) %>%
  filter(reps >=9) %>%
  summarise(sd = sd(N.best))
ssp.data %>%
  group_by(condition) %>%
  filter(reps >=8) %>%
  summarise(sd = sd(N.best))
ssp.data %>%
  group_by(condition) %>%
  filter(reps >=0) %>%
  summarise(sd = sd(N.best))
# -> variance is indeed smaller if algorithm runs until the end

ggplot(ssp.data, aes(x = N.best, fill = condition)) +
  geom_density(alpha = .5) +
  geom_density(
    data = dplyr::filter(ssp.data, reps >= 11),
    aes(x = N.best, color = condition),
    fill = NA,
    linetype = "dotdash"  )

ggplot(ssp.data, aes(x = N.best)) +
  geom_density(alpha = .5) +
  geom_density(
    data = filter(ssp.data, reps >= 10),
    fill = NA,
    linetype = "dotted",
    linewidth = 0.9
  ) +
  geom_density(
    data = filter(ssp.data, reps >= 11),
    fill = NA,
    linetype = "dotdash",
    linewidth = 0.9
  ) +
  facet_wrap(~ condition, ncol = 1)

# -> tolerance stopping criterion removed
