#' @name numericInputs
#' @title Tabular Numeric Input Controls
#' 
#' @description Create an input control for entry of numeric values
#' 
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param value Initial value.
#' @param min Minimum allowed value.
#' @param max Maximum allowed value.
#' @param step Interval to use when stepping between min and max
#' @param width The width of the input, e.g. \code{'400px'} or \code{'100\%'}
#' @param leftLabel Should the label column be the left column or the right column.
#'   \code{TRUE} puts the label on the left column.  This only takes effect when
#'   \code{labelButton = FALSE}.
#' 
#' @return \code{numericInput_cell} returns a character vector the length of \code{inputId}
#' 
#' \code{numericInput_row} returns a data frame with two columns and a row for each element of
#'   \code{inputId}
#'   
#' @seealso \code{\link[shiny]{numericInput}}
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' \dontrun{
#' options(pixiedust_print_method = "html")
#' shinyApp(
#'   ui = shinyUI(
#'     wellPanel(uiOutput("sampleUI"))
#'   ),
#'   
#'   server = shinyServer(function(input, output){
#'     output$sampleUI <- 
#'       renderText({ 
#'         numericInput_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                          label = rownames(mtcars),
#'                          value = 50,
#'                          min = 0,
#'                          max = 100,
#'                          step = 10) %>%
#'                          dust() %>%
#'                          print(asis = FALSE)
#'       })
#'   })
#' )
#' }
#' 
#' @export

numericInput_cell <- function (inputId, label = "", value, 
                               min = NA, max = NA, step = NA, 
                               width = "", 
                               disabled = FALSE, hidden = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  mapply(checkmate::assertCharacter, 
         list(inputId, label, width), 
         .var.name = c("inputId", "label", "width"), 
         MoreArgs = list(add = coll))
  mapply(checkmate::assertNumeric, 
         list(min, max, step), 
         .var.name = c("min", "max", "step"), 
         MoreArgs = list(add = coll))
  lapply(list(disabled, hidden), 
         checkmate::assertLogical, 
         add = coll)
  checkmate::reportAssertions(coll)
  
  min <- rep(min, 
             length.out = length(inputId))
  max <- rep(max, 
             length.out = length(inputId))
  step <- rep(step, 
              length.out = length(inputId))
  value <- rep(value, 
               length.out = length(inputId))
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  paste0(label, "<input id='", inputId, "' ", "class='form-group shiny-input-container form-control", 
         ifelse(disabled, " shinyjs-disabled", ""), 
         ifelse(hidden, " shinyjs-hide", ""), 
         "' type='number' ", 
         ifelse(is.na(value), "", paste0("value='", value, "' ")), 
         ifelse(is.na(min), "", paste0("min='", min, "' ")), 
         ifelse(is.na(max), "", paste0("max='", max, "' ")), 
         ifelse(is.na(step), "", paste0("step='", step, "' ")), 
         ifelse(width == "", "", paste0("style='width:", width, ";' ")), "/>")
}

#' @rdname numericInputs
#' @export

numericInput_row <- function(inputId, label = "", value, min = NA, max = NA, 
                             step = NA,  width = "", leftLabel = TRUE, 
                             disabled = FALSE, hidden = FALSE) 
{
  controls <- numericInput_cell(inputId = inputId, 
                                label = "", 
                                value = value, 
                                min = min, 
                                max = max, 
                                step = step, 
                                width = width, 
                                disabled = disabled, 
                                hidden = hidden)
  if (leftLabel){
    data.frame(label, 
               controls, 
               stringsAsFactors = FALSE)
  } else {
    data.frame(controls, 
               label, 
               stringsAsFactors = FALSE)
  }
}