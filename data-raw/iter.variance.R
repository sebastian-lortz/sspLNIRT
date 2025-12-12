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
  "R/fct_comp_mse.R",
  "R/fct_design_conditions.R",
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
  future::plan(future::multicore, workers = n.cores)
} else {
  n.cores <- 6
  future::plan(future::multisession, workers = n.cores)
}



# Run the Job -------------------------------------------------------------


# generate the design conditions for low and high N
design <- expand.grid(
  XG = c(1000, 3000, 6000, 9000),
  iter = c(100, 500)
)

# storage
result.list <- list()

# compute MSE
start.time = Sys.time()
for (i in 2:nrow(design)) {

  result <- list()
  iter <- design$iter[i]
  XG <- design$XG[i]

  for (k in 1:100) {
    res <- optim_sample(
      FUN = comp_mse,
      thresh = .01,
      lb = 100,
      ub = 500,
      tol = .0001,
      out.par = 'mse.alpha',
      iter = iter,
      I = 15,
      mu.person = c(0,0),
      mu.item = c(1,0,1,0),
      meanlog.sigma2 = log(.3),
      cov.m.person = matrix(c(1,0.5,
                              0.5,1), ncol = 2, byrow = TRUE),
      cov.m.item = matrix(c(1, 0, 0, 0,
                            0, 1, 0, 0.3,
                            0, 0, 1, 0,
                            0, 0.3, 0, 1), ncol =  4, byrow = TRUE),
      sd.item         = c(.2, .5, .2, .5),
      cor2cov.item    = TRUE,
      sdlog.sigma2 = 0.2,
      person.seed = NULL,
      item.seed = NULL,
      XG = XG)
    result[[k]] <- res
    cat("iteration", k, "of", 100, "done!!!! \n\n")
    rm(res)
  }
  saveRDS(result, paste0(save.dir, "ssp.variance.no.seed.", i))

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
#ssp.variance.no.seed.2 <- readRDS(paste0(save.dir, "mse.variance.no.seed.2"))

res.names <- list(
  ssp.variance.no.seed.1 #, ssp.variance.no.seed.2
)
ssp.variance.no.seed.1[[1]][[6]]

## check convergence
# optim_sample didnt return conv rates for [[64]]
lengths(ssp.variance.no.seed.1)
which(lengths(ssp.variance.no.seed.1) < 6)
subset.list <- ssp.variance.no.seed.1[-64]

summary(
  unlist(sapply(subset.list, FUN = function(x) {
  #x[[6]]
  sapply(x[[6]], FUN = function(y) {
    colMeans(y)
  })
  })))
# conv rates min = 99.85%

# get ssp data
ssp.res <- lapply(res.names, FUN = function(x) {
t(sapply(x, FUN = function(y) {
    (y[c(1,2,3,7)])
  }))
})

# drop the ub violation iteration
ssp.res[[1]] <- ssp.res[[1]][-64,]

ssp.data <- lapply(ssp.res, FUN = function(x) {
  as.data.frame(cbind(
    N.best = as.numeric(x[,1]),
    res.best = as.numeric(x[,2]),
    reps = as.numeric(x[,3]),
    time.taken = as.numeric(x[,4])))
})


sum.stats <- ssp.data[[1]] %>%
  slice(-64) %>%  # drop row 64
  summarise(
    across(
      1:4,
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x,   na.rm = TRUE),
        min  = ~ min(.x,  na.rm = TRUE),
        max  = ~ max(.x,  na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to  = c("variable", ".value"),
    names_sep = "_"
  ) %>%
  data.frame()




# plot ssp variance
cond_labels <- apply(design, 1, function(x) {
  paste(sprintf("%s = %s", names(x), x), collapse = " & ")
})

# density plot
N.mu = sum.stats$mean[1]
N.sd = sum.stats$sd[1]

ggplot(ssp.data[[1]], aes(x = N.best)) +
  geom_density(alpha = .5) +
  geom_vline(xintercept = c(N.mu-N.sd, N.mu, N.mu + N.sd), color = "red") +
  geom_vline(xintercept = sum.stats$mean[1]-sum.stats$sd1, linetype = "dotted")

# histogram
ggplot(ssp.data[[1]], aes(x = N.best)) +
  geom_histogram() +
  geom_vline(xintercept = c(N.mu-N.sd, N.mu, N.mu + N.sd), color = "red") +
  geom_vline(xintercept = sum.stats$mean[1]-sum.stats$sd1, linetype = "dotted")


  facet_wrap(
    ~ condition,
    ncol = 1,
    labeller = labeller(condition = cond_labels[1:2])
  )

# violin plot
ggplot(data  = mse.data, mapping = aes(x = parameter, y = mse, fill = condition)) +
  geom_violin()


