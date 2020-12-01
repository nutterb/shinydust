#' @name checkboxInputs
#' @title Tabluar Checkbox Control Elements
#' 
#' @description Produce the HTML code for a cell with a checkbox or a row of a two
#'   column table iwth a checkbox.
#'   
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control.  Defaults to \code{""} for no label.
#' @param value Initial value (\code{TRUE} or \code{FALSE})
#' @param width The width of the input, e.g. \code{'400px'} or '100\%'. Defaults to
#'   \code{""}. 
#' @param leftLabel Should the label column be the left column or the right column.
#'   \code{TRUE} puts the label on the left column.
#' 
#' @return \code{checkboxInput_cell} returns a character vector of HTML code, one 
#'   element for each \code{inputId}.
#'   
#' \code{checkboxInput_row} returns a data frame with two columns and one row for
#'   each \code{inputId}. 
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link[shiny]{checkboxInput}}
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
#'         checkboxInput_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                           label = rownames(mtcars)) %>%
#'                           dust() %>%
#'                           print(asis = FALSE)
#'       })
#'   })
#' )
#' }
#' 
#' @export


checkboxInput_cell <- function(inputId, label = "", value = FALSE, width = "", 
                               leftLabel = FALSE, 
                               disabled = FALSE, hidden = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  mapply(checkmate::assertCharacter, 
         list(inputId, label, width), 
         .var.name = c("inputId", "label", "width"), 
         MoreArgs = list(add = coll))
  mapply(checkmate::assertLogical, 
         list(value, leftLabel, disabled, hidden), 
         .var.name = c("value", "leftLabel", "disabled", "hidden"), 
         MoreArgs = list(add = coll))
  checkmate::reportAssertions(coll)
  
  leftLabel <- rep(leftLabel, 
                   length.out = length(inputId))
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  sprintf(paste0("%s<input id='%s' type='checkbox' class='form-group ", 
                 "shiny-input-containter checkbox%s%s'%s%s/>%s"), 
          ifelse(leftLabel, label, ""), 
          inputId, 
          ifelse(disabled, " shinyjs-disabled", ""), 
          ifelse(hidden, " shinyjs-hide", ""), 
          ifelse(value, "checked='checked' ", ""), 
          ifelse(width == "", "", 
                 paste0("style='width:", width, "'")), 
          ifelse(leftLabel, "", label))
}

#' @rdname checkboxInputs
#' @export

checkboxInput_row <- function(inputId, label = "", value = FALSE, width = "", 
                              leftLabel = FALSE, 
                              disabled = FALSE, hidden = FALSE) 
{
  controls <- checkboxInput_cell(inputId = inputId, 
                                 label = "", 
                                 value = value, 
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
