# script to determine the variance in MSE given the number of
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
  #save.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/results/"
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
  n.cores <- future::availableCores() - 1
  cat("running with ", n.cores, "cores! \n\n")
  future::plan(future::multisession, workers = n.cores)
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
for (i in seq_len(nrow(design))) {

  result <- list()
  iter <- design$iter[i]
  XG <- design$XG[i]

  for (k in 1:100) {
    res <- comp_mse(
      N = 250,
      iter = iter,
      I = 15,
      mu.person = c(0,0),
      mu.item = c(1,0,1,4),
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
  saveRDS(result, paste0(save.dir, "mse.variance.no.seed.", i))

  result.list[[i]] <- result
  cat("Design row", i, "done!!!! \n\n")
  gc()
}
saveRDS(result.list, paste0(save.dir, "mse.variance.no.seed.list"))

end.time = Sys.time()
time.taken = end.time-start.time
print(time.taken)
