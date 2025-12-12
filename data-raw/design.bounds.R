# script to determine the lower and upper bound of N for the sspLNIRT.data script

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
  I = c(15,45),
  N = c(100, 2000),
  mu.alpha = c(.7, 1.3),
  meanlog.sigma2 = c(log(.3), log(1)),
  rho = c(0, .6),
  cor.item = c(0, 0.6),
  sd.factor = c(1,2)
  )

# storage
bounds.res <- list()

# compute MSE
for (i in seq_len(nrow(design))) {

# get pars
N = design$N[i]
I = design$I[i]
meanlog.sigma2 = design$meanlog.sigma2[i]
rho = design$rho[i]
cor.item = design$cor.item[i]
sd.factor = design$sd.factor[i]
mu.alpha = design$mu.alpha[i]
# comp MSE
res <- comp_mse(
          iter = 1000,
          N = N,
          I = I,
          mu.person = c(0,0),
          mu.item = c(mu.alpha,0,1,4),
          meanlog.sigma2 = meanlog.sigma2,
          cov.m.person = matrix(c(1,rho,
                                  rho,1), ncol = 2, byrow = TRUE),
          cov.m.item = matrix(c(1, 0, 0, 0,
                                 0, 1, 0, cor.item,
                                 0, 0, 1, 0,
                                 0, cor.item, 0, 1), ncol =  4, byrow = TRUE),
          sd.item         = sd.factor*c(.2, .5, .2, .5),
          cor2cov.item    = TRUE,
          sdlog.sigma2 = 0.2,
          person.seed = NULL,
          item.seed = NULL,
          XG = 3000)
bounds.res[[i]] <- res
cat("design row", i, "of", nrow(design), "done!! \n\n")
rm(res)
gc()
}

saveRDS(bounds.res, paste0(save.dir, "design.bounds"))
