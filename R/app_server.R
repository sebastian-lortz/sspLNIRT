#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  observeEvent(input$go_to_tab, {
    updateNavbarPage(session, "main", selected = input$go_to_tab)
  })

  # Modules
  mod_ssp_custom_server("ssp_custom")
  mod_ssp_data_server("ssp_data")
}
