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


# no cor between alpha and beta
res.no.cor <- comp_rmse(
    iter = 1000,
    N = 250,
    K = 30,
    mu.person = c(0,0),
    mu.item = c(1,0,1,0),
    meanlog.sigma2 = log(.6),
    cov.m.person = matrix(c(1,0,
                            0,1), ncol = 2, byrow = TRUE),
    cov.m.item = matrix(c(1, 0, 0, 0,
                          0, 1, 0, 0,
                          0, 0, 1, 0,
                          0, 0, 0, 1), ncol =  4, byrow = TRUE),
    sd.item         = c(.2, 1, .2, 1),
    cor2cov.item    = TRUE,
    sdlog.sigma2 = 0.2,
    par1 = TRUE,
    XG = 3000,
    burnin = 20)
saveRDS(res.no.cor, paste0(save.dir, "res.no.cor"))

# no cor between alpha and beta for large test length
res.no.cor2 <- comp_rmse(
  iter = 1000,
  N = 250,
  K = 50,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.6),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, 0,
                        0, 0, 1, 0,
                        0, 0, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.no.cor2, paste0(save.dir, "res.no.cor.I50"))

# cor between alpha and beta
res.cor <- comp_rmse(
    iter = 1000,
    N = 250,
    K = 30,
    mu.person = c(0,0),
    mu.item = c(1,0,1,0),
    meanlog.sigma2 = log(.6),
    cov.m.person = matrix(c(1,0,
                            0,1), ncol = 2, byrow = TRUE),
    cov.m.item = matrix(c(1, -.6, 0, 0,
                          -.6, 1, 0, 0,
                          0, 0, 1, 0,
                          0, 0, 0, 1), ncol =  4, byrow = TRUE),
    sd.item         = c(.2, 1, .2, 1),
    cor2cov.item    = TRUE,
    sdlog.sigma2 = 0.2,
    par1 = TRUE,
    XG = 3000,
    burnin = 20)
saveRDS(res.cor, paste0(save.dir, "res.cor.alpha.beta"))

# cor between beta and lambda
res.cor2 <- comp_rmse(
  iter = 1000,
  N = 250,
  K = 30,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.6),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor2, paste0(save.dir, "res.cor.beta.lambda"))

# cor between beta and lambda for large test length
res.cor3 <- comp_rmse(
  iter = 1000,
  N = 250,
  K = 50,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.6),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor3, paste0(save.dir, "res.cor.beta.lambda.I50"))

# cor between alpha and beta for long tests
res.cor4 <- comp_rmse(
  iter = 1000,
  N = 250,
  K = 50,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.6),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, -.6, 0, 0,
                        -.6, 1, 0, 0,
                        0, 0, 1, 0,
                        0, 0, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor4, paste0(save.dir, "res.cor.alpha.beta.I50"))

# cor between alpha, beta and lambda
res.cor5 <- comp_rmse(
  iter = 1000,
  N = 250,
  K = 30,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.6),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, -.6, 0, 0,
                        -.6, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor5, paste0(save.dir, "res.cor.alpha.beta.lambda"))

# cor between alpha, beta and lambda for long test lengths
res.cor6 <- comp_rmse(
  iter = 1000,
  N = 250,
  K = 50,
  mu.person = c(0,0),
  mu.item = c(1,0,1,0),
  meanlog.sigma2 = log(.6),
  cov.m.person = matrix(c(1,0,
                          0,1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, -.6, 0, 0,
                        -.6, 1, 0, .6,
                        0, 0, 1, 0,
                        0, .6, 0, 1), ncol =  4, byrow = TRUE),
  sd.item         = c(.2, 1, .2, 1),
  cor2cov.item    = TRUE,
  sdlog.sigma2 = 0.2,
  par1 = TRUE,
  XG = 3000,
  burnin = 20)
saveRDS(res.cor6, paste0(save.dir, "res.cor.alpha.beta.lambda.I50"))



# Results -----------------------------------------------------------------

library(dplyr)
library(ggplot2)

# load results
res.no.cor <- readRDS(paste0(save.dir, "res.no.cor"))
res.cor.alpha.beta <- readRDS(paste0(save.dir, "res.cor.alpha.beta"))
res.cor.beta.lambda <- readRDS(paste0(save.dir, "res.cor.beta.lambda"))
res.cor.alpha.beta.lambda <- readRDS(paste0(save.dir, "res.cor.alpha.beta.lambda"))
res.no.cor.I50 <- readRDS(paste0(save.dir, "res.no.cor.I50"))
res.cor.alpha.beta.I50 <- readRDS(paste0(save.dir, "res.cor.alpha.beta.I50"))
res.cor.beta.lambda.I50 <- readRDS(paste0(save.dir, "res.cor.beta.lambda.I50"))
res.cor.alpha.beta.lambda.I50 <- readRDS(paste0(save.dir, "res.cor.alpha.beta.lambda.I50"))
res.names <- list(
  res.no.cor, res.cor.alpha.beta, res.cor.beta.lambda, res.cor.alpha.beta.lambda,
  res.no.cor.I50, res.cor.alpha.beta.I50, res.cor.beta.lambda.I50, res.cor.alpha.beta.lambda.I50
)

# check convergence
lapply(res.names, FUN = function(x) {
  nrow(x$conv.rate)/1000
})
# similar convergence 77% - 86%

# compare results
person.par <- t(sapply(res.names, FUN = function(x) {
  as.data.frame(x[c(1:2)])
}))
item.par <- t(sapply(res.names, FUN = function(x) {
  x[c(3:7)]
}))
rownames(person.par) <- rownames(item.par) <- c("no.cor", "cor.alpha.beta", "cor.beta.lambda", "cor.alpha.beta.lambda",
                        "no.cor.I50", "cor.alpha.beta.I50", "cor.beta.lambda.I50", "cor.alpha.beta.lambda.I50")

# prep ggplot format
ggdat <- data.frame(
  condition = rep(rownames(item.par), 5),
  parameter = rep(colnames(item.par), each = 8),
  mse = unlist(c(item.par)),
  mc.se = c(do.call(rbind, sapply(res.names, FUN = function(x) {
    x[8]
  })))
)
ggdat$condition <- factor(ggdat$condition, levels = unique(ggdat$condition))

# split data
ggdat.I50 <- ggdat[grepl("I50", ggdat[,1]),]
ggdat.I30 <- ggdat[!grepl("I50", ggdat[,1]),]

# test length 30
ggplot(data = ggdat.I30, aes(x = condition, y = mse, group = parameter)) +
  geom_line(aes(color = parameter)) +
  geom_point()  +
  geom_errorbar(aes(ymin=mse-2*mc.se, ymax=mse+2*mc.se, color = parameter))

# test length 50
ggplot(data = ggdat.I50, aes(x = condition, y = mse, group = parameter)) +
  geom_line(aes(color = parameter)) +
  geom_point()  +
  geom_errorbar(aes(ymin=mse-2*mc.se, ymax=mse+2*mc.se, color = parameter), position=position_dodge(0))

# compare test lengths
ggplot(data = ggdat, aes(x = condition, y = mse, group = parameter)) +
  geom_line(aes(color = parameter)) +
  geom_point()
# approximately similar for K = 30
