#' ssp_custom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ssp_custom_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ssp_custom Server Functions
#'
#' @noRd 
mod_ssp_custom_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ssp_custom_ui("ssp_custom_1")
    
## To be copied in the server
# mod_ssp_custom_server("ssp_custom_1")
