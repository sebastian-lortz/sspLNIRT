#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    fluidPage(
      bslib::navset_tab(
        id = "tab",

        bslib::nav_panel(
          title = "Home",
          div(
            style = "margin-top: 20px;",
            includeMarkdown(app_sys("app/www/home.md"))
          )
        ),

        # precomputed data
        bslib::nav_panel(
          title = "Precomputations",
          mod_ssp_data_ui("ssp_data")
        ),

        # customizable functions
        bslib::nav_panel(
          title = "Custom Functions",
          mod_ssp_custom_ui("ssp_custom")
        ),

        # about the app
        bslib::nav_panel(
          title = "About",
          div(
            style = "margin-top: 20px;",
          includeMarkdown(app_sys("app/www/about.md"))
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sspLNIRT"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
