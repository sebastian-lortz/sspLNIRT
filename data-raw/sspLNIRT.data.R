# Script to create the dataset `sspLNIRT.data` from the precomputed results

# libraries
library(tibble)

# load the batches
save.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/results/"

files <- list.files(
  path = save.dir,
  pattern = "^batch_[0-9]{3}\\.rds$",
  full.names = TRUE
)

#numeric batch id
batch_id <- as.integer(sub("^batch_([0-9]{3})\\.rds$", "\\1", basename(files)))


# read all batches into a named list
batch.list <- setNames(lapply(files, readRDS), sprintf("batch_%03d", batch_id))



rows <- lapply(names(batch.list), function(bn) {
  batch <- batch.list[[bn]]

  # one tibble row per element inside this batch
  lapply(seq_along(batch), function(i) {

    res <- batch[[i]][[1]]  # results
    cfg <- batch[[i]][[2]]  # config

    tibble(
      batch   = bn,
      element = i,

      N    = res[[1]],
      mse  = res[[2]],
      conv = nrow(res[[6]][[res[[3]]]]),

      thresh = cfg$thresh,
      lb     = cfg$lb,
      ub     = cfg$ub,
      out.par = cfg$out.par,
      iter   = cfg$iter,
      I      = cfg$I,

      mu.person = list(cfg$mu.person),
      mu.item   = list(cfg$mu.item),
      meanlog.sigma2 = cfg$meanlog.sigma2,

      cov.m.person = list(cfg$cov.m.person),
      cov.m.item   = list(cfg$cov.m.item),

      sd.item      = list(cfg$sd.item),
      cor2cov.item = cfg$cor2cov.item,
      sdlog.sigma2 = cfg$sdlog.sigma2,
      XG           = cfg$XG,
      ssp.seed     = cfg$ssp.seed
    )
  })
})

# flatten into a single tibble
all_tbl <- do.call(rbind, unlist(rows, recursive = FALSE))

all_tbl$conv
batch.list$batch_005[[47]][[1]]


# combine the batches into one data frame


# usethis::use_data(res, overwrite = TRUE)
