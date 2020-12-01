#' @name textInputs
#' @title Tabular Text Input Controls
#' 
#' @description Create a table of input controls for entry of unstructured text values
#' 
#' @param inputId the \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{""} for no label.
#' @param value Initial value
#' @param width The width of the input, e.g. \code{'400px'} or \code{'100\%'}
#' @param leftLabel Should the label column be the left column or the right column.
#'   \code{TRUE} puts the label on the left column.
#' 
#' @return \code{textInput_cell} returns a character vector the length of \code{inputId}.
#' 
#' \code{textInput_row} returns a data frame with two columns and a row for every 
#' element in \code{inputId}
#' 
#' @seealso \code{\link[shiny]{textInput}}
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
#'         textInput_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                         label = rownames(mtcars)) %>%
#'         dust() %>%
#'         print(asis = FALSE)
#'       })
#'   })
#' )
#' }
#' 
#' @export

textInput_cell <- function (inputId, label, value = "", 
                            width = "", 
                            disabled = FALSE, hidden = FALSE, 
                            placeholder = NA) 
{
  coll <- checkmate::makeAssertCollection()
  mapply(checkmate::assertCharacter, 
         list(inputId, label, width), 
         .var.name = c("inputId", "label", "width"), 
         MoreArgs = list(add = coll))
  mapply(checkmate::assertLogical, 
         list(disabled, hidden), 
         .var.name = c("disabled", "hidden"), 
         MoreArgs = list(add = coll))
  checkmate::reportAssertions(coll)
  label <- rep(label, 
               length.out = length(inputId))
  value <- rep(value, 
               length.out = length(inputId))
  width <- rep(width, 
               length.out = length(inputId))
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  placeholder <- rep(placeholder, 
                     length.out = length(inputId))
  value <- gsub("'", "&#39;", value)
  paste0(label, 
         "<input id='", inputId, "' ", 
         "type='text' class='form-group shiny-input-container form-control", 
         ifelse(disabled, " shinyjs-disabled", ""), 
         ifelse(hidden, " shinyjs-hide", ""), "' ", 
         "style='width:", width, "' ", 
         ifelse(!is.na(value), 
                paste0("value='", value, "'"), ""), 
         ifelse(is.na(placeholder), "", 
                paste0("placeholder='", placeholder, "'")), 
         "/>")
}

#' @rdname textInputs
#' @export

textInput_row <- function (inputId, label, 
                           value = "", width = "", 
                           leftLabel = TRUE, 
                           disabled = FALSE, hidden = FALSE, 
                           placeholder = NA) 
{
  controls <- textInput_cell(inputId = inputId, 
                             label = "", 
                             value = value, 
                             width = width, 
                             disabled = disabled, 
                             hidden = hidden, 
                             placeholder = placeholder)
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
