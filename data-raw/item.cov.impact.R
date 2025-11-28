# script to determine item cov impact

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
  n.cores <- future::availableCores() - 1
  cat("running with ", n.cores, "cores! \n\n")
  future::plan(future::multisession, workers = n.cores)
} else {
  n.cores <- 6
  future::plan(future::multisession, workers = n.cores)
}




# Run the Job -------------------------------------------------------------




# no cor between alpha and beta
res.no.cor <- comp_mse(
    iter = 1000,
    N = 250,
    I = 15,
    mu.person = c(0,0),
    mu.item = c(1,0,1,4),
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
    par1 = TRUE,
    XG = 3000,
    burnin = 20)
saveRDS(res.no.cor, paste0(save.dir, "res.no.cor"))

# no cor between alpha and beta for large test length
res.no.cor2 <- comp_mse(
  iter = 1000,
  N = 250,
  I = 45,
  mu.person = c(0,0),
  mu.item = c(1,0,1,4),
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
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.no.cor2, paste0(save.dir, "res.no.cor.I45"))

# cor between alpha and beta
res.cor <- comp_mse(
    iter = 1000,
    N = 250,
    I = 15,
    mu.person = c(0,0),
    mu.item = c(1,0,1,4),
    meanlog.sigma2 = log(.3),
    cov.m.person = matrix(c(1,0,
                            0,1), ncol = 2, byrow = TRUE),
    cov.m.item = matrix(c(1, -.6, 0, 0,
                          -.6, 1, 0, 0,
                          0, 0, 1, 0,
                          0, 0, 0, 1), ncol =  4, byrow = TRUE),
    sd.item         = c(.2, .5, .2, .5),
    cor2cov.item    = TRUE,
    sdlog.sigma2 = 0.2,
    person.seed = NULL,
    item.seed = NULL,
    par1 = TRUE,
    XG = 3000,
    burnin = 20)
saveRDS(res.cor, paste0(save.dir, "res.cor.alpha.beta"))

# cor between beta and lambda
res.cor2 <- comp_mse(
  iter = 1000,
  N = 250,
  I = 15,
  mu.person = c(0,0),
  mu.item = c(1,0,1,4),
  meanlog.sigma2 = log(.3),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, .5, .2, .5),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  person.seed = NULL,
  item.seed = NULL,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor2, paste0(save.dir, "res.cor.beta.lambda"))

# cor between beta and lambda for large test length
res.cor3 <- comp_mse(
  iter = 1000,
  N = 250,
  I = 45,
  mu.person = c(0,0),
  mu.item = c(1,0,1,4),
  meanlog.sigma2 = log(.3),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, .5, .2, .5),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  person.seed = NULL,
  item.seed = NULL,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor3, paste0(save.dir, "res.cor.beta.lambda.I45"))

# cor between alpha and beta for long tests
res.cor4 <- comp_mse(
  iter = 1000,
  N = 250,
  I = 45,
  mu.person = c(0,0),
  mu.item = c(1,0,1,4),
  meanlog.sigma2 = log(.3),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, -.6, 0, 0,
                        -.6, 1, 0, 0,
                        0, 0, 1, 0,
                        0, 0, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, .5, .2, .5),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  person.seed = NULL,
  item.seed = NULL,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor4, paste0(save.dir, "res.cor.alpha.beta.I45"))

# cor between alpha, beta and lambda
res.cor5 <- comp_mse(
  iter = 1000,
  N = 250,
  I = 15,
  mu.person = c(0,0),
  mu.item = c(1,0,1,4),
  meanlog.sigma2 = log(.3),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, -.6, 0, 0,
                        -.6, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, .5, .2, .5),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  person.seed = NULL,
  item.seed = NULL,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor5, paste0(save.dir, "res.cor.alpha.beta.lambda"))

# cor between alpha, beta and lambda for long test lengths
res.cor6 <- comp_mse(
  iter = 1000,
  N = 250,
  I = 45,
  mu.person = c(0,0),
  mu.item = c(1,0,1,4),
  meanlog.sigma2 = log(.3),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, -.6, 0, 0,
                        -.6, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, .5, .2, .5),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  person.seed = NULL,
  item.seed = NULL,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor6, paste0(save.dir, "res.cor.alpha.beta.lambda.I45"))

# load results
res.no.cor <- readRDS(paste0(save.dir, "res.no.cor"))
res.cor.alpha.beta <- readRDS(paste0(save.dir, "res.cor.alpha.beta"))
res.cor.beta.lambda <- readRDS(paste0(save.dir, "res.cor.beta.lambda"))
res.cor.alpha.beta.I45 <- readRDS(paste0(save.dir, "res.cor.alpha.beta.I45"))
res.cor.alpha.beta.lambda <- readRDS(paste0(save.dir, "res.cor.alpha.beta.lambda"))
res.cor.alpha.beta.lambda.I45 <- readRDS(paste0(save.dir, "res.cor.alpha.beta.lambda.I45"))


# check convergence
nrow(res.no.cor$conv.rate)
nrow(res.cor.alpha.beta$conv.rate)
nrow(res.cor.beta.lambda$conv.rate)
nrow(res.cor.alpha.beta.I45$conv.rate)
nrow(res.cor.alpha.beta.lambda$conv.rate)
nrow(res.cor.alpha.beta.lambda.I45$conv.rate)
# similar convergence 99.7% - 100%

# compare results
as.data.frame(res.no.cor[c(1:7)])
as.data.frame(res.cor.alpha.beta[c(1:7)])
as.data.frame(res.cor.beta.lambda[c(1:7)])
as.data.frame(res.cor.alpha.beta.I45[c(1:7)])
as.data.frame(res.cor.alpha.beta.lambda[c(1:7)])
as.data.frame(res.cor.alpha.beta.lambda.I45[c(1:7)])

# approximately similar for I = 15
