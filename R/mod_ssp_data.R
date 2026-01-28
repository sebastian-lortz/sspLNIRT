#' ssp_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' ssp_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ssp_data_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    bslib::page_fluid(
      bslib::layout_columns(
        col_widths = c(4, 8),

        # =========================
        # LEFT SIDE: INPUTS (as-is)
        # =========================
        bslib::accordion(
          id   = ns("acc"),
          open = NULL,  # start collapsed

          # --- Person Parameters ---
          bslib::accordion_panel(
            title = "Person Parameters",
            icon  = bsicons::bs_icon("menu-app"),

            shiny::helpText("Person parameters are assumed Standard Normal with SD = 1."),

            bslib::layout_columns(
              col_widths = c(4, 4, 4),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_theta"), "Mean Ability", value = 0, step = 0.01)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_zeta"), "Mean Speed", value = 0, step = 0.01)
              ),

              shiny::sliderInput(
                ns("corr_person"),
                "Correlation",
                min   = 0.2,
                max   = 0.6,
                value = 0.2,
                step  = 0.2
              )
            )
          ),

          # --- Item Parameters ---
          bslib::accordion_panel(
            title = "Item Parameters",
            icon  = bsicons::bs_icon("sliders"),

            shiny::helpText("The item parameters follow a truncated multivariate normal distribution."),

            bslib::layout_columns(
              col_widths = c(3, 9),
              shiny::selectInput(
                inputId  = ns("I"),
                label    = "Test Length",
                choices  = c(30, 50),
                selected = 30
              )
            ),

            shiny::tags$hr(),

            shiny::tags$h5("Means"),
            bslib::layout_columns(
              col_widths = c(3, 3, 3, 3),

              shiny::selectInput(
                inputId  = ns("mu_alpha"),
                label    = "alpha",
                choices  = c("0.6" = 0.6, "1.0" = 1.0, "1.4" = 1.4),
                selected = 0.6
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_beta"), "beta", value = 0, step = 0.01)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_phi"), "phi", value = 1, step = 0.01)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_lambda"), "lambda", value = 1, step = 0.01)
              )
            ),

            shiny::tags$hr(),

            shiny::tags$fieldset(
              `disabled` = NA,
              shinyMatrix::matrixInput(
                inputId = ns("corr_sd_item"),
                label   = "Item Correlations and SD",
                value   = matrix(
                  c(
                    1, 0, 0, 0,
                    0, 1, 0, 0.4,
                    0, 0, 1, 0,
                    0, 0.4, 0, 1,
                    0.2, 1.0, 0.2, 0.5
                  ),
                  nrow = 5,
                  byrow = TRUE,
                  dimnames = list(
                    c("alpha", "beta", "phi", "lambda", "SD"),
                    c("alpha", "beta", "phi", "lambda")
                  )
                ),
                rows = list(names = TRUE),
                cols = list(names = TRUE)
              )
            ),

            shiny::tags$hr(),

            shiny::tags$h5("Residual variance (sigma2) of the RT model"),
            shiny::tags$h6("The residual variance is assumed to follow a log-normal distribution."),

            bslib::layout_columns(
              col_widths = c(6, 6),

              shiny::selectInput(
                inputId  = ns("meanlog_sigma2"),
                label    = "Log-normal Mean",
                choices  = c(
                  "log(0.2)" = log(0.2),
                  "log(0.6)" = log(0.6),
                  "log(1)"   = log(1)
                ),
                selected = log(0.2)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("sdlog_sigma2"), "Log-normal SD", value = 0.2, min = 0, step = 0.01)
              )
            )
          ),

          # --- Targets ---
          bslib::accordion_panel(
            title = "Targets",
            icon  = bsicons::bs_icon("calendar-date"),

            bslib::layout_columns(
              col_widths = c(6, 6),

              shiny::selectInput(
                inputId  = ns("out_par"),
                label    = "Target parameter",
                choices  = c(
                  "alpha" = "alpha",
                  "beta"       = "beta",
                  "phi"   = "phi",
                  "lambda"      = "lambda"
                ),
                selected = "alpha"
              ),

              shiny::sliderInput(
                inputId = ns("thresh_idx"),
                label   = "Expected RMSE Threshold",
                min     = 0.05,
                max     = 0.20,
                value   = 0.05,
                step    = 0.05
              )
            )
          ),

          # --- Optimizer Settings (disabled) ---
          bslib::accordion_panel(
            title = "Optimizer Settings",
            icon  = bsicons::bs_icon("bar-chart"),

            shiny::tags$fieldset(
              `disabled` = NA,

              bslib::layout_columns(
                col_widths = c(3, 3, 3, 3),
                shiny::numericInput(ns("iter"), "iter", value = 100, min = 1, step = 1),
                shiny::numericInput(ns("XG"),   "XG",   value = 6000, min = 1, step = 100),
                shiny::numericInput(ns("lb"),   "lb",   value = 100,  min = 1, step = 1),
                shiny::numericInput(ns("ub"),   "ub",   value = 2000, min = 1, step = 1)
              ),

              shiny::textInput(ns("ssp_seed"), "Reproducibility Seed", value = "")
            )
          )
        ),

        # =========================
        # RIGHT SIDE: OUTPUT WINDOW
        # =========================
        shiny::tagList(
          bslib::card(
            bslib::card_header("Output"),
            bslib::card_body(
              style = "height: 25vh; overflow-y: auto;",
              shiny::verbatimTextOutput(ns("out_text"))
            )
          ),

          bslib::card(
            bslib::card_header("Plots"),
            bslib::card_body(
              style = "height: 65vh; overflow-y: auto;",
              bslib::navset_tab(
                bslib::nav_panel("Plot 1", shiny::plotOutput(ns("plot1"), height = "55vh")),
                bslib::nav_panel("Plot 2", shiny::plotOutput(ns("plot2"), height = "55vh"))
              )
            )
          )
        )
      )
    )
  )
}










#' ssp_data Server Functions
#'
#' @noRd
mod_ssp_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Helper: disabled inputs can sometimes come through as NULL
    nz <- function(x, default) if (is.null(x) || length(x) == 0) default else x

    # ONLY collect inputs and package them as optim.sample-style arguments
    optim_inputs <- shiny::reactive({

      # --- scalars / controls ---
      thresh <- as.numeric(input$thresh_idx)
      lb     <- as.integer(nz(input$lb, 100))
      ub     <- as.integer(nz(input$ub, 2000))
      out.par <- as.character(input$out_par)
      iter   <- as.integer(nz(input$iter, 100))
      I      <- as.integer(as.numeric(input$I))
      XG     <- as.integer(nz(input$XG, 6000))
      seed_raw <- trimws(nz(input$ssp_seed, ""))
      ssp.seed <- if (identical(seed_raw, "")) NULL else as.integer(seed_raw)

      # --- person: mu.person + cov.m.person from (mu_theta, mu_zeta, rho) ---
      mu_theta <- as.numeric(nz(input$mu_theta, 0))
      mu_zeta  <- as.numeric(nz(input$mu_zeta,  0))
      rho      <- as.numeric(input$corr_person)

      mu.person <- c(mu_theta, mu_zeta)
      cov.m.person <- matrix(c(1, rho,
                               rho, 1),
                             nrow = 2, byrow = TRUE)

      # --- items: mu.item ---
      mu_alpha  <- as.numeric(input$mu_alpha)
      mu_beta   <- as.numeric(nz(input$mu_beta, 0))
      mu_phi    <- as.numeric(nz(input$mu_phi, 1))
      mu_lambda <- as.numeric(nz(input$mu_lambda, 1))
      mu.item <- c(mu_alpha, mu_beta, mu_phi, mu_lambda)

      # --- item correlation + SD from matrixInput; derive cov.m.item and sd.item ---
      M <- nz(
        input$corr_sd_item,
        matrix(
          c(
            1, 0, 0, 0,
            0, 1, 0, 0.4,
            0, 0, 1, 0,
            0, 0.4, 0, 1,
            0.2, 1.0, 0.2, 0.5
          ),
          nrow = 5, byrow = TRUE
        )
      )
      M <- apply(M, c(1, 2), as.numeric)

      cov.m.item <- as.matrix(M[1:4, 1:4])
      sd.item <- as.numeric(M[5, 1:4])
      cor2cov.item <- TRUE

      # --- residual variance ---
      meanlog.sigma2 <- as.numeric(input$meanlog_sigma2)
      sdlog.sigma2   <- as.numeric(nz(input$sdlog_sigma2, 0.2))

      # Return ONLY the requested parameters
      list(
        thresh         = thresh,
        lb             = lb,
        ub             = ub,
        out.par        = out.par,
        iter           = iter,
        I              = I,
        mu.person      = mu.person,
        mu.item        = mu.item,
        meanlog.sigma2 = meanlog.sigma2,
        cov.m.person   = cov.m.person,
        cov.m.item     = cov.m.item,
        sd.item        = sd.item,
        cor2cov.item   = cor2cov.item,
        sdlog.sigma2   = sdlog.sigma2,
        XG             = XG,
        ssp.seed       = ssp.seed
      )
    })

    # expose only the collected inputs for downstream use
    list(optim_inputs = optim_inputs)


    output$out_text <- shiny::renderPrint({
      args <- optim_inputs()
      dat  <- get("all_tbl", envir = asNamespace("sspLNIRT"))

      dat.f <- dat %>%
        dplyr::filter(
          #out.par == args$out.par,
          out.par == "mse.alpha", # need to change with new batch
          I == args$I,
          thresh == args$thresh,
          dplyr::near(meanlog.sigma2, args$meanlog.sigma2),

          purrr::map_lgl(mu.item,       ~ isTRUE(all.equal(.x, args$mu.item))),
          purrr::map_lgl(cov.m.person,  ~ isTRUE(all.equal(.x, args$cov.m.person))),
        )

    list.out <- list(
     N.out = dat.f$N,
     res.out = dat.f$mse,
     conv.out = dat.f$conv,
     seed.out = dat.f$ssp.seed)

    list.out
    })
  })
}



## To be copied in the UI
# mod_ssp_data_ui("ssp_data_1")

## To be copied in the server
# mod_ssp_data_server("ssp_data_1")
