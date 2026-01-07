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
  "R/fct_comp_mse.R",
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

exp0 <- comp_mse(
  iter = 100,
  N = 100,
  I = 30,
  mu.person = c(0,0),
  mu.item = c(1,0,1,1),
  meanlog.sigma2 = log(.2),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, .4,
                        0, 0, 1, 0,
                        0, .4, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, .5),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  XG = 6000,
  mse.seed = 12)

exp1 <- comp_mse(
  iter = 100,
  N = 100,
  I = 30,
  mu.person = c(0,0),
  mu.item = c(1,0,1,1),
  meanlog.sigma2 = log(.2),
  cov.m.person = matrix(c(1,.9,
                          .9,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, .4,
                        0, 0, 1, 0,
                        0, .4, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, .5),
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

exp0$mse.theta
exp1$mse.theta

exp0$mse.zeta
exp1$mse.zeta

exp0$mse.alpha
exp1$mse.alpha

exp0$mse.beta
exp1$mse.beta

exp0$mse.phi
exp1$mse.phi

exp0$mse.lambda
exp1$mse.lambda




