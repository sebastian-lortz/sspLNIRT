# Script to create the tibble `sspLNIRT.data` from the precomputed results

# libraries
library(tibble)
library(dplyr)

# load the batches
save.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/results/"

files <- list.files(
  path = save.dir,
  pattern = "^batch_[0-9]{3}\\.rds$",
  full.names = TRUE
)

# numeric batch id
batch_id <- as.integer(sub("^batch_([0-9]{3})\\.rds$", "\\1", basename(files)))

# read all batches into a named list
batch.list <- setNames(lapply(files, readRDS), sprintf("batch_%03d", batch_id))

# combine the batches into one data frame
rows <- lapply(names(batch.list), function(bn) {
  batch <- batch.list[[bn]]
  lapply(seq_along(batch), function(i) {
    res <- batch[[i]][[1]]
    cfg <- batch[[i]][[2]]

    # pre bin the err data
    n.bins = 30
    item.bin.means <- res$comp.rmse$err.dat$item %>%
      dplyr::filter(par != "sigma2") %>%
      group_by(par) %>%
      mutate(bin = ntile(sim.val, n.bins)) %>%
      group_by(par, bin) %>%
      summarise(
        mean_sim = mean(sim.val, na.rm = TRUE),
        mean_err = mean(err, na.rm = TRUE),
        mean_rmse = sqrt(mean(err^2, na.rm = TRUE)),
        .groups = "drop"
      )
    person.bin.means <- res$comp.rmse$err.dat$person %>%
      group_by(par) %>%
      mutate(bin = ntile(sim.val, n.bins)) %>%
      group_by(par, bin) %>%
      summarise(
        mean_sim = mean(sim.val, na.rm = TRUE),
        mean_err = mean(err, na.rm = TRUE),
        mean_rmse = sqrt(mean(err^2, na.rm = TRUE)),
        .groups = "drop"
      )

    # fit power curve
    if (res$trace$steps > 2) {
      fit <- lm(log(res$trace$track.res$res.temp) ~ log(res$trace$track.N$N.temp))
      a <- coef(fit)[1]
      b <- coef(fit)[2]
      N.curve <- as.numeric(ceiling(exp((log(cfg$thresh) - a) / b)))
    } else {
      N.curve <- NA
    }

    # replace err.dat with binned version, add N.curve
    res$comp.rmse$err.dat <- list(item = item.bin.means, person = person.bin.means)
    res$N.curve <- N.curve
    class(cfg) <- "sspLNIRT.design"

    tibble(
      batch   = bn,
      element = i,
      res = list(res),
      cfg = list(cfg)
    )
  })
})

# flatten into a single tibble
sspLNIRT.data <- do.call(rbind, unlist(rows, recursive = FALSE))

#### write data in file
usethis::use_data(sspLNIRT.data, overwrite = TRUE)


sspLNIRT.data[[3]][[1]]
