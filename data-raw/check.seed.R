# Script to examine the functionality of the seeds


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

parallelly::supportsMulticore()



# Run the Job -------------------------------------------------------------

design <- expand.grid(
  seed1 = c(NA, NA, 123, 123)
)
res.mse.seeds <- res.ssp.seeds <- list()

for (i in 1:nrow(design)) {

if (is.na(design$seed1[i])) {
  seed = NULL
} else {
  seed = design$seed1[i]
}
  print(seed)
res.mse.seeds[[i]] <- comp_mse(
                                N = 250,
                                iter = 6,
                                I = 15,
                                mu.person = c(0,0),
                                mu.item = c(1,0,1,0),
                                meanlog.sigma2 = log(.2),
                                cov.m.person = matrix(c(1,0.4,
                                                        0.4,1), ncol = 2, byrow = TRUE),
                                cov.m.item = matrix(c(1, 0, 0, 0,
                                                      0, 1, 0, 0.4,
                                                      0, 0, 1, 0,
                                                      0, 0.4, 0, 1), ncol =  4, byrow = TRUE),
                                sd.item         = c(.2, 1, .2, 1),
                                cor2cov.item    = TRUE,
                                sdlog.sigma2 = 0.2,
                                XG = 3000,
                                mse.seed = seed)

res.ssp.seeds[[i]] <- optim_sample(
  FUN = comp_mse,
  thresh = .02,
  lb = 100,
  ub = 400,
  out.par = 'mse.alpha',
  iter = 6,
  I = 15,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.2),
  cov.m.person = matrix(c(1,0.4,
                          0.4,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, 0.4,
                        0, 0, 1, 0,
                        0, 0.4, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  XG = 3000,
  ssp.seed = seed)
}


# Results -----------------------------------------------------------------

sapply(res.mse.seeds, function(x) {
  x[1:7]
})

sapply(res.ssp.seeds, function(x) {
  x[1:3]
})

