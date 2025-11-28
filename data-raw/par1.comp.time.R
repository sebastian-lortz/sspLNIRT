# script to determine if the parametrization has an impact on running time


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

# benchmark
# par1 = TRUE
start.time = Sys.time()
for (i in 1:20) {
comp_mse(
  iter = 1,
  N = 100,
  I = 10,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.3),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, 0,
                        0, 0, 1, 0,
                        0, 0, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, .5, .2, .5),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  person.seed = NULL,
  item.seed = NULL,
  n.cores = 1,
  par1 = TRUE)
}
# track time
end.time = Sys.time()
time.taken = end.time - start.time
print(time.taken)


# par 1 = FALSE
start.time = Sys.time()
for (i in 1:20) {
  comp_mse(
    iter = 1,
    N = 100,
    I = 10,
    mu.person = c(0,0),
    mu.item = c(1,0,1,0),
    meanlog.sigma2 = log(.3),
    cov.m.person = matrix(c(1,0,
                            0,1), ncol = 2, byrow = TRUE),
    cov.m.item = matrix(c(1, 0, 0, 0,
                          0, 1, 0, 0,
                          0, 0, 1, 0,
                          0, 0, 0, 1), ncol =  4, byrow = TRUE),
    sd.item         = c(.2, .5, .2, .5),
    cor2cov.item    = TRUE,
    sdlog.sigma2 = 0.2,
    person.seed = NULL,
    item.seed = NULL,
    n.cores = 1,
    par1 = FALSE)
}
# track time
end.time = Sys.time()
time.taken = end.time - start.time
print(time.taken)

#### conclusion
# par1 = FALSE is not substantially slower compared to par1 = TRUE
