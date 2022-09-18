shapes <- c("dot", "square", "triangle", "star", "box", "circle",
            "ellipse", "database", "text", "diamond")


#' mod_shapepicker
#'
#' @description module of shapepicker.
#' 
#' @param id string. type of the network node.
#' @param shape string. "square", "dot", "triangle", "star", "box", "circle",
#'                      "ellipse", "database", "text", "diamond".
#' @return shapepickerUI returns a pickerInput of shape for different types.
#'         shapepickerServer returns the value of shape pickerInput.
#' @examples
#' \dontrun{
#' shapepickerUI("NLP", "square")
#' shapepickerServer("NLP")
#' }
#' @export
shapepickerUI <- function(id, shape) {
  ns <- NS(id)
  shinyWidgets::pickerInput(
    inputId = ns("shapepicker"),
    label = id, 
    choices = shapes,
    selected = shape,
    choicesOpt = list(
      icon = c("fa fa-circle", 
               "fa fa-square", 
               "fa fa-mountain",
               "fa fa-star", 
               "fa fa-square-plus", 
               "fa fa-circle-info", 
               "fa fa-circle-question", 
               "fa fa-database", 
               "fa fa-font", 
               "fa diamond")),
    options = list(
      `icon-base` = "")
  )
}

#' @rdname shapepickerUI
shapepickerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
   ns <- NS(id)
   input$shapepicker
})}




