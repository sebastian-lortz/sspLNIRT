## Script to compute the minimum sample sizes and save the results in batches.
# This script can be executed on the Habrok high performance computing cluster

# set to Habrok cluster
HPC = FALSE
if (HPC) {

} else {
# set root path
root.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/"

# set save path
save.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/results/"
dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)
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

# generate the design conditions
design <- design_conditions()

# storage

# create batches of 100
L = length(design)
batches <- split(design, ceiling(seq_along(design) / 2))

# loop over batches
for (b in seq_along(batches)) {

  # get batch
  batch <- batches[[b]]

    # storage
    res.out <- list()

      # loop over batch
      for (i in seq_along(batch)) {

        # function arguments
        fct.args <- list(
                thresh          = batch[[i]]$thresh,
                lb              = 100,
                ub              = 2000,
                tol             = .01,
                out.par         = batch[[i]]$out.par,
                iter            = 6,
                I               = batch[[i]]$I,
                mu.person       = c(0,0),
                mu.item         = batch[[i]]$mu.item,
                meanlog.sigma2  = log(.3),
                cov.m.person    = batch[[i]]$cov.m.person,
                cov.m.item      = batch[[i]]$cov.m.item,
                sd.item         = c(.2, .5, .2, .5),
                cor2cov.item    = TRUE,
                sdlog.sigma2 = 0.2,
                scale = TRUE,
                random.item = TRUE,
                n.cores = 6)

        # estimate sample size
        res <- do.call(optim_sample, fct.args)

        # assemble output
        res.out[[i]] <- append(
                            list(
                                N.best = res$N.best,
                                res.best = res$res.best,
                                reps = res$reps),
                            fct.args)
      }

    # save batch result
    name <- paste0("batch",b)
    saveRDS(res.out, paste0(save.dir, name))

    # progress
    cat("Batch", b, "of", length(batches), "batches complete. \n")

    # empty memory
      rm(res.out)
      invisible(
      gc()
    )

}


