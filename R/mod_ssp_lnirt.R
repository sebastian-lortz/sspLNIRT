#' ssp_lnirt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ssp_lnirt_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ssp_lnirt Server Functions
#'
#' @noRd 
mod_ssp_lnirt_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ssp_lnirt_ui("ssp_lnirt_1")
    
## To be copied in the server
# mod_ssp_lnirt_server("ssp_lnirt_1")
