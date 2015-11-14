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


checkboxInput_cell <- function(inputId, label="", value=FALSE, width="", leftLabel = FALSE){
  
  if (length(leftLabel) == 1) leftLabel <- rep(leftLabel, length(inputId))
  
  paste0(ifelse(leftLabel, label, ""),
         "<input id='", inputId, "' ",
         "type='checkbox' ",
         "class='form-group shiny-input-container checkbox' ",
         ifelse(value, "checked='checked' ", ""),
         ifelse(width == "", "", paste0("style='width:", width, "'")),
         "/>", 
         ifelse(leftLabel, "", label))
}

#' @rdname checkboxInputs
#' @export

checkboxInput_row <- function(inputId, label="", value=FALSE, width="", leftLabel = FALSE){
  
  controls <- checkboxInput_cell(inputId = inputId, 
                                 label = "", 
                                 value = value,
                                 width = width)
  if (leftLabel) data.frame(label, controls, stringsAsFactors = FALSE)
  else data.frame(controls, label, stringsAsFactors = FALSE)
}