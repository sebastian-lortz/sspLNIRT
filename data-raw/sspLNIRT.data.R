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
    res <- batch[[i]]$res
    cfg <- batch[[i]]$args
    class(cfg) <- "sspLNIRT.design"

    names(res)[1] <- "N.min"

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


####### Inspect the data #########
conv <- sapply(seq_len(972), function(i) {
  sspLNIRT.data[[3]][[i]]$comp.rmse$conv.rate
})
summary(conv)
hist(conv)

Ns <- t(sapply(seq_len(972), function(i) {
  if(grepl("thresh", sspLNIRT.data[[3]][[i]]$N.min)) {
    cbind(NA_real_, NA_real_)
  } else {
    cbind(sspLNIRT.data[[3]][[i]]$N.min,
          sspLNIRT.data[[3]][[i]]$N.curve)
  }
}))

dif.n <- Ns[,1] - Ns[,2]
summary(dif.n)
plot(dif.n)

 Reduce("+", lapply(seq_len(972), \(x)
grepl("<", sspLNIRT.data[[3]][[x]]$N.min)))
