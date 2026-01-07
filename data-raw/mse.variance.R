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


# generate the design conditions for low and high N
design <- expand.grid(
  XG = c(1000, 3000, 6000),
  iter = c(100, 250, 500)
)

# storage
result.list <- list()

# compute MSE
start.time = Sys.time()
for (i in 1:nrow(design)) {

  result <- list()
  iter <- design$iter[i]
  XG <- design$XG[i]

  for (k in 1:100) {
    res <- comp_mse(
      N = 250,
      iter = iter,
      I = 30,
      mu.person = c(0,0),
      mu.item = c(1,0,1,1),
      meanlog.sigma2 = log(.6),
      cov.m.person = matrix(c(1,0.4,
                              0.4,1), ncol = 2, byrow = TRUE),
      cov.m.item = matrix(c(1, 0, 0, 0,
                            0, 1, 0, 0.4,
                            0, 0, 1, 0,
                            0, 0.4, 0, 1), ncol =  4, byrow = TRUE),
      sd.item         = c(.2, 1, .2, .5),
      cor2cov.item    = TRUE,
      sdlog.sigma2 = 0.2,
      XG = XG,
      mse.seed = NULL)
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


# Results -----------------------------------------------------------------

library(dplyr)
library(ggplot2)

L = nrow(design)

# load results
mse.variance.no.seed.1 <- readRDS(paste0(save.dir, "mse.variance.no.seed.1"))
mse.variance.no.seed.2 <- readRDS(paste0(save.dir, "mse.variance.no.seed.2"))
mse.variance.no.seed.3 <- readRDS(paste0(save.dir, "mse.variance.no.seed.3"))
mse.variance.no.seed.4 <- readRDS(paste0(save.dir, "mse.variance.no.seed.4"))
mse.variance.no.seed.5 <- readRDS(paste0(save.dir, "mse.variance.no.seed.5"))
mse.variance.no.seed.6 <- readRDS(paste0(save.dir, "mse.variance.no.seed.6"))
mse.variance.no.seed.7 <- readRDS(paste0(save.dir, "mse.variance.no.seed.7"))
mse.variance.no.seed.8 <- readRDS(paste0(save.dir, "mse.variance.no.seed.8"))
mse.variance.no.seed.9 <- readRDS(paste0(save.dir, "mse.variance.no.seed.9"))

res.names <- list(
  mse.variance.no.seed.1, mse.variance.no.seed.2, mse.variance.no.seed.3,
  mse.variance.no.seed.4, mse.variance.no.seed.5, mse.variance.no.seed.6,
  mse.variance.no.seed.7, mse.variance.no.seed.8, mse.variance.no.seed.9
)

# check convergence
conv.data = as.data.frame(sapply(res.names, FUN = function(x) {
  sapply(x, FUN = function(y) {
    nrow(y$conv.rate)
  })
}))

sum_conv <- cbind(design, data.frame(
  min  = apply(conv.data, 2, min),
  median = apply(conv.data, 2, median),
  mean = apply(conv.data, 2, mean),
  max  = apply(conv.data, 2, max),
  sd   = apply(conv.data, 2, sd),
  row.names = NULL
)) %>%
  mutate(prop = mean / iter)


# get mse data
list.mse <- lapply(res.names, FUN = function(x) {
  as.data.frame(t(sapply(x, FUN = function(y) {
    cbind(y$mse.alpha,
      y$mse.beta,
      y$mse.phi,
      y$mse.lambda,
      y$mse.sigma2)
  })))
})

mse.data <- data.frame(
  condition = factor(rep(rep(1:L, each = 100), 5)),
  parameter = factor(rep(c("alpha", "beta", "phi", "lambda", "sigma2"), each = L*100)),
  mse = unlist(dplyr::bind_rows(list.mse)), row.names = NULL)


sum.stats <- mse.data %>%
  summarise(
    mu  = mean(mse),
    sd  = sd(mse),
    min = min(mse),
    max = max(mse),
    rel.sd = sd(mse)/mean(mse),
    .by = c(condition, parameter)
  )

#round(res$mc.se.items,5)
saveRDS(sum.stats, paste0(save.dir, "mse.variance.sum.stats"))

# plot mse variance
cond_labels <- apply(design, 1, function(x) {
  paste(sprintf("%s = %s", names(x), x), collapse = " & ")
})
cond_labs <- setNames(cond_labels, levels(mse.data$condition))

# density plot
mse.data <- mse.data %>%
  filter(condition == 1 |
         condition == 2 |
         condition == 3)

ggplot(mse.data , aes(x = mse, fill = condition)) +
  geom_density(alpha = .5) +
  facet_wrap(
    ~ parameter,
    scales = "free",
    ncol = 1  )
design

# violin plot
ggplot(data  = mse.data, mapping = aes(x = condition, y = mse, fill = condition)) +
  geom_violin() +
  facet_wrap(
    ~ parameter,
    scales = "free_y",
    ncol = 1,
    labeller = labeller(condition = cond_labels)
  )


# sd plots
ggplot(data  = sum.stats %>%
         filter(condition %in% c(1, 2, 3)) %>%
         mutate(across(c(sd, rel.sd), ~round(.x, 5))),
       mapping = aes(x = condition, y = sd, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~ parameter,
    scales = "free_y",
    ncol = 1,
    labeller = labeller(condition = cond_labels[1:5])
  ) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  coord_cartesian(ylim = c(0, NA))

# rel.sd plots
ggplot(data  = sum.stats %>%
         filter(condition %in% c(3, 6, 9)) %>%
         mutate(across(c(sd, rel.sd), ~round(.x, 5))),
       mapping = aes(x = condition, y = rel.sd, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~ parameter,
    scales = "free_y",
    ncol = 1,
    labeller = labeller(condition = cond_labels[1:5])
  ) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  coord_cartesian(ylim = c(0, NA))


### Paper Figure and numbers

sum.stats %>%
  filter(condition == 3,
         parameter != "sigma2") %>%
  select(sd, rel.sd) %>%
  round(., 5) %>%
  mutate()

gg.stats <- sum.stats %>%
  filter(condition %in% c(1,2,3),
         parameter != "sigma2") %>%
  mutate(condition = dplyr::recode(condition, `1` = 1000, `2` = 3000, `3` = 6000)) %>%
  mutate(across(c(sd, rel.sd), ~round(.x, 5)))

gg.conv <- conv.data %>%
            select(V1, V2, V3) %>%
            pivot_longer(
              cols      = c(V1, V2, V3),
              names_to  = "condition",
              values_to = "conv"
            ) %>%
            mutate(condition = factor(recode(condition, `V1` = 1000, `V2` = 3000, `V3` = 6000)),
                   conv = conv/100)


abs.sd.mse.plot <- ggplot(
  data = gg.stats,
  aes(x = condition, y = sd, linetype = parameter, shape = parameter, group = parameter)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(breaks = c(1000, 3000, 6000)) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    y = "Absolute SD of Estimated MSE",
    x = "Posterior Samples (XG)",
    colour = "Parameter",
    linetype = "Parameter",
    shape = "Parameter"
  )

rel.sd.mse.plot <- ggplot(
    data = gg.stats,
    aes(x = condition, y = rel.sd, linetype = parameter, shape = parameter, group = parameter)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_x_continuous(breaks = c(1000, 3000, 6000)) +
    coord_cartesian(ylim = c(0, NA)) +
  labs(
    y = "Relative SD of Estimated MSE",
    x = "Posterior Samples (XG)",
    colour = "Parameter",
    linetype = "Parameter",
    shape = "Parameter"
  )

mse.conv.plot <- ggplot(
  data = gg.conv,
  aes(x = condition, y = conv)) +
  geom_violin() +
  labs(
    y = "Convergence Rate",
    x = "Number of Posterior Samples (XG)"
  )


library(patchwork)

sd.mse.plot <- (abs.sd.mse.plot + rel.sd.mse.plot) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  ) &
  theme(legend.position = "right")


ggsave(
  filename = "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/plots/sd.mse.plot.pdf",
  plot     = sd.mse.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white"
)

ggsave(
  filename = "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/plots/conv.plot.pdf",
  plot     = mse.conv.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white"
)


