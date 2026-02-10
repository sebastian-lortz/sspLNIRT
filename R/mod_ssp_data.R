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
        # LEFT SIDE: INPUTS
        # =========================
        bslib::accordion(
          id   = ns("acc"),
          open = FALSE,

          # --- Person Parameters ---
          bslib::accordion_panel(
            title = "Person Parameters",
            icon  = bsicons::bs_icon("people"),

            shiny::helpText("Person parameters are drawn from a bivariate normal distribution with unit variance."),

            bslib::layout_columns(
              col_widths = c(4, 4, 4),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_theta"),
                                    label = name_with_info("\u03B8 (theta)", "Latent ability parameter. Higher theta = higher probability of correct response."),
                                    value = 0, step = 0.01)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_zeta"),
                                    label = name_with_info("\u03B6 (zeta)", "Latent speed parameter. Higher zeta = faster responses."),
                                    value = 0, step = 0.01)
              ),

              shiny::sliderInput(
                ns("corr_person"),
                label = name_with_info("\u03C1 (rho)", "Correlation between ability (\u03B8) and speed (\u03B6). Positive = faster respondents tend to be more accurate."),
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
            icon  = bsicons::bs_icon("list-check"),

            shiny::helpText("Item parameters are drawn from a truncated multivariate normal distribution."),

            bslib::layout_columns(
              col_widths = c(4, 8),
              shiny::selectInput(
                inputId  = ns("K"),
                label    = name_with_info("K (Test Length)", "Number of items in the test."),
                choices  = c(10, 30, 50),
                selected = 30
              )
            ),

            shiny::tags$hr(),

            shiny::tags$h6("Item Parameter Means", class = "text-muted"),
            bslib::layout_columns(
              col_widths = c(3, 3, 3, 3),

              shiny::selectInput(
                inputId  = ns("mu_alpha"),
                label    = name_with_info("\u03B1 (alpha)", "Item discrimination. Higher \u03B1 = item better differentiates between high/low ability."),
                choices  = c("0.6" = 0.6, "1.0" = 1.0, "1.4" = 1.4),
                selected = 0.6
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_beta"),
                                    label = name_with_info("\u03B2 (beta)", "Item difficulty. Higher \u03B2 = more difficult item."),
                                    value = 0, step = 0.01)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_phi"),
                                    label = name_with_info("\u03C6 (phi)", "Time discrimination. Higher \u03C6 = RT more sensitive to speed differences."),
                                    value = .5, step = 0.01)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("mu_lambda"),
                                    label = name_with_info("\u03BB (lambda)", "Time intensity. Higher \u03BB = item takes longer on average."),
                                    value = 1, step = 0.01)
              )
            ),

            shiny::tags$hr(),

            shiny::tags$fieldset(
              `disabled` = NA,
              shinyMatrix::matrixInput(
                inputId = ns("corr_sd_item"),
                label   = name_with_info("Correlations & SDs", "Upper 4\u00D74: correlation matrix. Bottom row: standard deviations of item parameters."),
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
                    c("\u03B1", "\u03B2", "\u03C6", "\u03BB", "SD"),
                    c("\u03B1", "\u03B2", "\u03C6", "\u03BB")
                  )
                ),
                rows = list(names = TRUE),
                cols = list(names = TRUE)
              )
            ),

            shiny::tags$hr(),

            shiny::tags$h6("Residual Variance (\u03C3\u00B2)", class = "text-muted"),
            shiny::helpText("Log-normal distribution for RT model residual variance."),

            bslib::layout_columns(
              col_widths = c(6, 6),

              shiny::selectInput(
                inputId  = ns("meanlog_sigma2"),
                label    = name_with_info("Mean (log scale)", "Expected value of log(\u03C3\u00B2). Lower = less residual noise in RT."),
                choices  = c(
                  "log(0.2)" = log(0.2),
                  "log(0.6)" = log(0.6),
                  "log(1)"   = log(1)
                ),
                selected = log(0.2)
              ),

              shiny::tags$fieldset(
                `disabled` = NA,
                shiny::numericInput(ns("sdlog_sigma2"),
                                    label = name_with_info("SD (log scale)", "Variability of log(\u03C3\u00B2) across items."),
                                    value = 0, min = 0, step = 0.01)
              )
            )
          ),

          # --- Targets ---
          bslib::accordion_panel(
            title = "Targets",
            icon  = bsicons::bs_icon("bullseye"),

            shiny::helpText("Define the precision goal for sample size planning."),

            bslib::layout_columns(
              col_widths = c(6, 6),

              shiny::selectInput(
                inputId  = ns("out_par"),
                label    = name_with_info("Target Parameter", "Which item parameter should meet the RMSE threshold?"),
                choices  = c(
                  "\u03B1 (alpha)" = "alpha",
                  "\u03B2 (beta)"  = "beta",
                  "\u03C6 (phi)"   = "phi",
                  "\u03BB (lambda)" = "lambda"
                ),
                selected = "alpha"
              ),

              shiny::selectInput(
                inputId = ns("thresh"),
                label   = name_with_info("RMSE Threshold", "Target root mean square error. Lower = more precision required = larger N."),
                choices  = c(
                  "0.20"   = .2,
                  "0.10"  = .1,
                  "0.05"    = .05
                ),
                selected = .1
              )
            )
          ),

          # --- Optimizer Settings (disabled) ---
          bslib::accordion_panel(
            title = "Optimizer Settings",
            icon  = bsicons::bs_icon("gear"),

            shiny::helpText("Settings for model estimation and the bisection search algorithm."),

            shiny::tags$fieldset(
              `disabled` = NA,
              bslib::layout_columns(
                col_widths = c(6, 6),
                shiny::numericInput(ns("iter"),
                                    label = name_with_info("Iterations", "Monte Carlo replications per N evaluation."),
                                    value = 100, min = 1, step = 1),
                shiny::numericInput(ns("XG"),
                                    label = name_with_info("Gibbs Samples", "MCMC iterations for parameter estimation."),
                                    value = 6000, min = 1, step = 100)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                shiny::numericInput(ns("lb"),
                                    label = name_with_info("N min", "Lower bound of sample size search range."),
                                    value = 50, min = 10, step = 10),
                shiny::numericInput(ns("ub"),
                                    label = name_with_info("N max", "Upper bound of sample size search range."),
                                    value = 2000, min = 10, step = 10)
              ),
              bslib::layout_columns(
                col_widths = c(6),
                shiny::textInput(
                  ns("seed_display"),
                  label = name_with_info("Seed", "Random seed used for reproducibility of the sample size optimization."),
                  value = "\u2014",
                  width = "100%"
                )
              )
            )
          )
        ),

        # =========================
        # RIGHT SIDE: OUTPUT WINDOW
        # =========================
        bslib::accordion(
          id   = ns("acc_output"),
          open = FALSE,

          # --- Output Panel ---
          bslib::accordion_panel(
            title = "Output",
            icon  = bsicons::bs_icon("terminal"),

            shiny::div(
              style = "height: 40vh; overflow-y: auto;",
              shiny::verbatimTextOutput(ns("out_text"))
            ),
            # --- Downloads collapsible section ---
            shiny::tags$details(
              class = "mt-3",
              shiny::tags$summary(
                style = "cursor: pointer; font-size: 0.85em; color: #6c757d;",
                bsicons::bs_icon("download", class = "me-1"),
                "Downloads"
              ),
              shiny::div(
                class = "pt-2",
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shiny::downloadButton(
                    ns("download_object"),
                    label = "Result (.rds)",
                    class = "btn-outline-secondary btn-sm w-100"
                  ),
                  shiny::downloadButton(
                    ns("download_design"),
                    label = "Design (.rds)",
                    class = "btn-outline-secondary btn-sm w-100"
                  )
                )
              )
            )
          ),


          # --- Plots Panel ---
          bslib::accordion_panel(
            title = "Plots",
            icon  = bsicons::bs_icon("graph-up"),

            shiny::div(
              style = "height: 62vh; overflow-y: auto;",
              bslib::navset_pill(

                # === Precision panel (FIRST) ===
                bslib::nav_panel(
                  title = "Precision",
                  icon  = bsicons::bs_icon("speedometer2"),

                  bslib::card(
                    bslib::card_header("Parameter Precision at the Minimum N", class = "bg-light"),
                    bslib::card_body(
                      bslib::layout_columns(
                        col_widths = c(6, 6),
                        shiny::selectInput(
                          inputId = ns("precision_pars"),
                          label = name_with_info("Parameters", "Show precision for item or person parameters."),
                          choices = c("Item" = "item", "Person" = "person"),
                          selected = "item"
                        ),
                        shiny::selectInput(
                          inputId = ns("precision_yval"),
                          label = name_with_info("Metric", "RMSE = estimation error; Bias = systematic over/underestimation."),
                          choices = c("RMSE" = "rmse", "Bias" = "bias"),
                          selected = "rmse"
                        )
                      ),
                      shiny::plotOutput(ns("plot3"), height = "35vh")
                    )
                  )
                ),

                # === Power Curve panel ===
                bslib::nav_panel(
                  title = "Power Curve",
                  icon  = bsicons::bs_icon("lightning"),

                  bslib::card(
                    bslib::card_header("Power Curve of Optimization Steps", class = "bg-light"),
                    bslib::card_body(
                      shiny::plotOutput(ns("plot4"), height = "45vh")
                    )
                  )
                ),

                # === Response Time panel ===
                bslib::nav_panel(
                  title = "Response Time",
                  icon  = bsicons::bs_icon("clock"),

                  bslib::card(
                    bslib::card_header("Response Time Distribution at Optimal N", class = "bg-light"),
                    bslib::card_body(
                      bslib::layout_columns(
                        col_widths = c(4, 4, 4),
                        shiny::selectInput(
                          inputId = ns("rt_level"),
                          label = name_with_info("Level", "Person: average RT per person. Item: RT distribution per item."),
                          choices = c("Person" = "person", "Item" = "item"),
                          selected = "person"
                        ),
                        shiny::selectInput(
                          inputId = ns("rt_logRT"),
                          label = name_with_info("Scale", "Seconds or log-seconds. Log scale often more symmetric."),
                          choices = c("Seconds" = "FALSE", "Log" = "TRUE"),
                          selected = "FALSE"
                        ),
                        shiny::div(
                          style = "padding-top: 1.9em;",
                          shiny::actionButton(
                            ns("draw_rt"),
                            label = "Draw Sample",
                            icon  = shiny::icon("play"),
                            class = "btn-primary w-100"
                          )
                        )
                      ),
                      shiny::plotOutput(ns("plot1"), height = "35vh")
                    )
                  )
                ),

                # === Response Accuracy panel ===
                bslib::nav_panel(
                  title = "Response Accuracy",
                  icon  = bsicons::bs_icon("check-circle"),

                  bslib::card(
                    bslib::card_header("Response Accuracy Distribution at Optimal N", class = "bg-light"),
                    bslib::card_body(
                      bslib::layout_columns(
                        col_widths = c(4, 4, 4),
                        shiny::selectInput(
                          inputId = ns("ra_level"),
                          label = name_with_info("Level", "Person: total score. Item: probability correct per item."),
                          choices = c("Person" = "person", "Item" = "item"),
                          selected = "person"
                        ),
                        shiny::selectInput(
                          inputId = ns("ra_by_theta"),
                          label = name_with_info("X-axis", "By \u03B8: show accuracy vs ability. By Item: show distributions."),
                          choices = c("By \u03B8" = "TRUE", "Distribution" = "FALSE"),
                          selected = "TRUE"
                        ),
                        shiny::div(
                          style = "padding-top: 1.9em;",
                          shiny::actionButton(
                            ns("draw_ra"),
                            label = "Draw Sample",
                            icon  = shiny::icon("play"),
                            class = "btn-primary w-100"
                          )
                        )
                      ),
                      shiny::plotOutput(ns("plot2"), height = "35vh")
                    )
                  )
                )
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

    # Reactive to get the sspLNIRT result based on UI inputs
    ssp_result <- shiny::reactive({

      # Extract design parameters from UI
      thresh <- as.numeric(input$thresh)
      out.par <- as.character(input$out_par)
      K <- as.integer(as.numeric(input$K))
      mu.alpha <- as.numeric(input$mu_alpha)
      meanlog.sigma2 <- as.numeric(input$meanlog_sigma2)
      rho <- as.numeric(input$corr_person)

      # Get matching result from precomputed data
      tryCatch({
        get_sspLNIRT(
          thresh = thresh,
          out.par = out.par,
          K = K,
          mu.alpha = mu.alpha,
          meanlog.sigma2 = meanlog.sigma2,
          rho = rho
        )
      }, error = function(e) {
        list(
          object = NULL,
          design = NULL,
          error = e$message
        )
      })
    })

    # Helper function to extract optimal N safely
    get_optimal_N <- function(obj) {
      if (is.null(obj)) return(NULL)

      N <- obj$N.min

      if (!is.null(N)) {
        N <- as.integer(N[1])
        if (is.na(N) || N <= 0) N <- NULL
      }

      return(N)
    }

    # Update seed display when result changes
    shiny::observe({
      result <- ssp_result()
      if (!is.null(result$design) && !is.null(result$design$seed)) {
        shiny::updateTextInput(session, "seed_display", value = as.character(result$design$seed))
      } else {
        shiny::updateTextInput(session, "seed_display", value = "\u2014")
      }
    })

    # Render summary output
    output$out_text <- shiny::renderPrint({
      result <- ssp_result()

      if (!is.null(result$error)) {
        cat("Error:\n", result$error)
      } else if (!is.null(result$object)) {
        cat("=== sspLNIRT Result ===\n\n")
        summary(result$object)
      } else {
        cat("No matching configuration found.")
      }
    })

    # ============================================
    # Download handlers
    # ============================================
    output$download_object <- shiny::downloadHandler(
      filename = function() {
        paste0("ssp_lnirt_result_", format(Sys.Date(), "%Y%m%d"), ".rds")
      },
      content = function(file) {
        result <- ssp_result()
        if (!is.null(result$object)) {
          saveRDS(result$object, file)
        }
      }
    )

    output$download_design <- shiny::downloadHandler(
      filename = function() {
        paste0("ssp_lnirt_design_", format(Sys.Date(), "%Y%m%d"), ".rds")
      },
      content = function(file) {
        result <- ssp_result()
        if (!is.null(result$design)) {
          saveRDS(result$design, file)
        }
      }
    )

    # ============================================
    # Plot 1: Response Time distribution (on button click)
    # ============================================
    rt_plot_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$draw_rt, {
      # Increment counter to force redraw
      rt_plot_trigger(rt_plot_trigger() + 1)
    })

    output$plot1 <- shiny::renderPlot({
      # Depend on the trigger
      trigger <- rt_plot_trigger()

      if (trigger == 0) {
        plot.new()
        text(0.5, 0.5, "Click 'Draw Sample' to generate plot", cex = 1.2, col = "grey50")
        return()
      }

      result <- ssp_result()
      N_opt <- get_optimal_N(result$object)

      if (is.null(result$object) || is.null(N_opt)) {
        plot.new()
        text(0.5, 0.5, "No minimum N available", cex = 1.2, col = "grey50")
        return()
      }

      cfg <- result$design

      plot_RT(
        level = input$rt_level,
        logRT = as.logical(input$rt_logRT),
        N = N_opt,
        K = cfg$K,
        mu.person = cfg$mu.person,
        mu.item = cfg$mu.item,
        meanlog.sigma2 = cfg$meanlog.sigma2,
        cov.m.person = cfg$cov.m.person,
        cov.m.item = cfg$cov.m.item,
        sd.item = cfg$sd.item,
        sdlog.sigma2 = cfg$sdlog.sigma2,
        cor2cov.item = cfg$cor2cov.item
      )
    }, res = 120)

    # ============================================
    # Plot 2: Response Accuracy (on button click)
    # ============================================
    ra_plot_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$draw_ra, {
      # Increment counter to force redraw
      ra_plot_trigger(ra_plot_trigger() + 1)
    })

    output$plot2 <- shiny::renderPlot({
      # Depend on the trigger
      trigger <- ra_plot_trigger()

      if (trigger == 0) {
        plot.new()
        text(0.5, 0.5, "Click 'Draw Sample' to generate plot", cex = 1.2, col = "grey50")
        return()
      }

      result <- ssp_result()
      N_opt <- get_optimal_N(result$object)

      if (is.null(result$object) || is.null(N_opt)) {
        plot.new()
        text(0.5, 0.5, "No minimum N available", cex = 1.2, col = "grey50")
        return()
      }

      cfg <- result$design

      plot_RA(
        level = input$ra_level,
        by.theta = as.logical(input$ra_by_theta),
        N = N_opt,
        K = cfg$K,
        mu.person = cfg$mu.person,
        mu.item = cfg$mu.item,
        meanlog.sigma2 = cfg$meanlog.sigma2,
        cov.m.person = cfg$cov.m.person,
        cov.m.item = cfg$cov.m.item,
        sd.item = cfg$sd.item,
        sdlog.sigma2 = cfg$sdlog.sigma2,
        cor2cov.item = cfg$cor2cov.item
      )
    }, res = 120)

    # ============================================
    # Plot 3: Precision plot (always reactive)
    # ============================================
    output$plot3 <- shiny::renderPlot({
      result <- ssp_result()
      N_opt <- get_optimal_N(result$object)

      if (is.null(result$object) || is.null(N_opt)) {
        plot.new()
        text(0.5, 0.5, "No minimum N data available", cex = 1.2, col = "grey50")
        return()
      }

      obj <- result$object

      plot_precision(
        object = obj,
        pars = input$precision_pars,
        y.val = input$precision_yval,
        n.bins = 30
      )
    }, res = 120)

    # ============================================
    # Plot 4: Power Curve
    # ============================================
    output$plot4 <- shiny::renderPlot({
      result <- ssp_result()
      N_opt <- get_optimal_N(result$object)

      if (is.null(result$object) || is.null(N_opt)) {
        plot.new()
        text(0.5, 0.5, "No minimum N available", cex = 1.2, col = "grey50")
        return()
      }

      obj <- result$object
      design <- result$design

      plot_power_curve(
        object = obj,
        thresh = design$thresh
      )
    }, res = 120)

  })
}

## To be copied in the UI
# mod_ssp_data_ui("ssp_data_1")

## To be copied in the server
# mod_ssp_data_server("ssp_data_1")
