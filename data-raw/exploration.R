# Exploration for large rho

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
HPC = FALSE

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

exp0 <- comp_rmse(
  iter = 200,
  N = 100,
  K = 10,
  mu.person = c(0,0),
  mu.item = c(1,0,1,1),
  meanlog.sigma2 = log(.2),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, 0,
                        0, 0, 1, 0,
                        0, 0, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .1, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  XG = 6000,
  mse.seed = 123)

exp1 <- comp_rmse(
  iter = 200,
  N = 100,
  K = 10,
  mu.person = c(0,0),
  mu.item = c(1,0,1,1),
  meanlog.sigma2 = log(.2),
  cov.m.person = matrix(c(1,0.9,
                          0.9,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, 0.4,
                        0, 0, 1, 0,
                        0, 0.4, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .1, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  XG = 6000,
  mse.seed = 123)


save.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/"
saveRDS(exp0, paste0(save.dir, "exp0"))
saveRDS(exp1, paste0(save.dir, "exp1"))


# Results -----------------------------------------------------------------

exp0 <- readRDS(paste0(save.dir,"exp0"))
exp1 <- readRDS(paste0(save.dir,"exp1"))


exp0$conv.rate
exp1$conv.rate

exp0$mse.person
exp1$mse.person

exp0$mse.items
exp1$mse.items

# diff item pars
diff.items <- c(exp0$mse.items - exp1$mse.items)
names(diff.items) <- c("alpha", "beta", "phi", "lambda", "sigma2")
plot(diff.items,
     ylab = "Difference")

plot_estimation(exp0)
plot_estimation(exp1)


# no measurable difference on accuracy of item parameters



