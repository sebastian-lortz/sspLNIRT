# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# check dependencies
# shows every file + line where each package is referenced
renv::dependencies("R/")
rsconnect::appDependencies()
#spelling::update_wordlist()

# read me info
system("clang --version")   # macOS C/C++ compiler
system("gfortran --version")# Fortran compiler
R.version.string
# Test your app
devtools::document()

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv()
## If you want to deploy to ShinyProxy
golem::add_dockerfile_with_renv_shinyproxy()

## Posit ----
## If you want to deploy on Posit related platforms
#golem::add_positconnect_file()
golem::add_shinyappsio_file()
#golem::add_shinyserver_file()

## Deploy to Posit Connect or ShinyApps.io ----

# install own package from Git and snapshot
# FIRST find and delete all .o .so files
# find . -type f -name '*.so'
# find . -type f -name '*.o'
# ./src/discourse.so
# rm ./src/discourse.so

# THEN commit and push!

remotes::install_github("sebastian-lortz/sspLNIRT")
renv::snapshot(prompt = FALSE)

## Add/update manifest file (optional; for Git backed deployment on Posit )
rsconnect::writeManifest()

# one-time: install rsconnect if needed
#install.packages("rsconnect")

rsconnect::setAccountInfo(name='sebastian-lortz',
                          token='E49AD05B5F9E8CDCE2462D3590F39BC1',
                          secret='Z31qmjL4VXZYShJHwbNhBQMoLlHGGLycUcjz8m+r')

# deploy

#options(rsconnect.dependency.lock = FALSE)
# update to latest CRAN release (not dev version)
# install.packages("rsconnect")

# restart R session, then:
rsconnect::deployApp(
  appName  = "sspLNIRT",
  appTitle = "sspLNIRT",
  appFiles = c(
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  lint = FALSE,
  forceUpdate = TRUE
)


### pkg down
install.packages("pkgdown")
usethis::use_pkgdown()

pkgdown::build_site()
