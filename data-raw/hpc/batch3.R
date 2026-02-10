## Script to compute the minimum sample sizes and save the results in batches.
# This script can be executed on the Habrok high performance computing cluster

# job batch number
job = 3
n.jobs = 20

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

ssp.seed0 <- 310779

# generate the design conditions
design <- expand.grid(
  out.par         = c("alpha", "beta", "phi", "lambda"),
  thresh          = c(.2, .1, .05),
  K               = c(10, 30, 50),
  rho             = c(.2, .4, .6),
  mu.alpha        = c(.6, 1.0, 1.4),
  meanlog.sigma2  = c(log(.2), log(.6), log(1)),
  stringsAsFactors = FALSE
)


# incorporate a unique seed per design row
design$ssp.seed <- seq(from = ssp.seed0, length.out = nrow(design))

# Randomly shuffle rows then split into batches
L <- nrow(design)
set.seed(ssp.seed0)
design <- design[sample(L), ]
batches <- split(design, cut(seq_len(L), breaks = n.jobs, labels = FALSE))
set.seed(NULL)


start.time <- Sys.time()

  batch <- batches[[job]]

  # preallocate
  res.batch <- vector("list", nrow(batch))

  for (i in seq_len(nrow(batch))) {

    arg <- list(
      thresh = batch$thresh[i],
      range = c(50, 2000),
      out.par = as.character(batch$out.par[i]),
      iter = 100,
      K = batch$K[i],
      mu.person = c(0, 0),
      mu.item = c(batch$mu.alpha[i], 0, 0.5, 1),
      meanlog.sigma2 = batch$meanlog.sigma2[i],
      cov.m.person = matrix(c(1, batch$rho[i],
                              batch$rho[i], 1), ncol = 2, byrow = TRUE),
      cov.m.item = matrix(c(1, 0, 0, 0,
                            0, 1, 0, 0.4,
                            0, 0, 1, 0,
                            0, 0.4, 0, 1), ncol = 4, byrow = TRUE),
      sd.item = c(.2, 1, .2, .5),
      cor2cov.item = TRUE,
      sdlog.sigma2 = 0,
      item.pars.m = NULL,
      XG = 6000,
      burnin = 20,
      seed = batch$ssp.seed[i],
      rhat = 1.05,
      keep.err.dat = FALSE
    )
    res <- tryCatch(
      do.call(optim_sample, args = arg),
      error = function(e) e
    )
    res.batch[[i]] <- list(res = res, args = arg)
    cat("Row", i, "of", nrow(batch), "complete.\n")
  }

  file <- file.path(save.dir, sprintf("batch_%03d.rds", job))
  saveRDS(res.batch, file)

end.time <- Sys.time()
print(end.time - start.time)



