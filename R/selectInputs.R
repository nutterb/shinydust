#' @name selectInputs
#' @title Tabular Select List Input Controls
#' 
#' @description Create a select list that can be used to choose a single or multiple items
#'   from a list of values.
#'   
#' @param inputId The \code{input} slot that will be used to access the value
#' @param label Display label for the control, or \code{""} for no label.
#' @param choices List of values to select from. If the elements of the list are named, 
#'   then the name rather than the value is displayed to the user.
#' @param selected The initially selected value (or multiple values if \code{multiple = TRUE}). 
#'   If not specified then defaults to the first value for single-select lists and no 
#'   values for multiple select lists.
#' @param multiple Is selection of multiple items allowed?
#' @param selectize Whether to use selectize.js or not.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'}
#' @param size Number of items to show in the selection box; a larger number will result in
#'   a taller box.  Not compatible with \code{selectize=TRUE}. Normally, when 
#'   \code{multiple=FALSE}, a select input will be a drop-down list, but when \code{size}
#'   is set, it will be a box instead.
#' @param ... Arguments passed to \code{selectInput()}
#' @param leftLabel Should the label column be the left column or the right column.
#'   \code{TRUE} puts the label on the left column.
#'   
#' @return \code{selectInput_cell} returns a character vector the length of \code{inputId}.
#' 
#' \code{selectInput_row} returns a data frame with two columns and a row for every 
#' element in \code{inputId}
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
#'         selectInput_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                         label = rownames(mtcars),
#'                         choices = LETTERS[1:5]) %>%
#'         dust() %>%
#'         print(asis = FALSE)
#'       })
#'   })
#' )
#' }
#' 
#' @export

selectInput_cell <- function (inputId, label = "", 
                              choices, selected = NULL, 
                              multiple = FALSE, selectize = TRUE, 
                              width = "", size = NULL, 
                              disabled = FALSE, hidden = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  mapply(checkmate::assertCharacter, 
         list(inputId, label, width), 
         .var.name = c("inputId", "label", "width"), 
         MoreArgs = list(add = coll))
  mapply(checkmate::assertLogical, 
         list(multiple, selectize, disabled, hidden), 
         .var.name = c("multiple", "selectize", "disabled", "hidden"), 
         len = list(1, 1, NULL, NULL), 
         MoreArgs = list(add = coll))
  
  if (!is.null(size)) {
    checkmate::assertInteger(size, 
                             len = 1, 
                             add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  if (is.null(names(choices))) 
    names(choices) <- choices
  
  if (is.null(selected)) 
    selected = choices[1]
  
  label <- rep(label, 
               length.out = length(inputId))
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(hidden))
  paste0(label, 
         "<select id='", inputId, "' ", 
         if (multiple) "multiple='multiple' " else "", 
         "class='form-group shiny-input-container", 
         ifelse(disabled, " shinyjs-disabled", ""), 
         ifelse(hidden, " shinyjs-hide", ""),
         "'", 
         ifelse(width == "", "", 
                paste0(" style='width:", width, "'")), 
         ">", 
         paste0("<option value='", 
                choices, "' ", 
                ifelse(selected == choices, " selected", ""), 
                ">",
                names(choices), "</option> ", collapse = " "), 
         if (selectize) "<script type='application/json' data-for='x'>{}</script>" else "")
}

#' @rdname selectInputs
#' @export

selectInput_row <- function(inputId, label = "", 
                            choices, selected = NULL, 
                            multiple = FALSE, selectize = TRUE, 
                            width = "", size = NULL, 
                            leftLabel = TRUE, 
                            disabled = FALSE, hidden = FALSE) 
{
  controls <- selectInput_cell(inputId = inputId, 
                               label = "", 
                               choices = choices, 
                               selected = selected, 
                               multiple = multiple, 
                               selectize = selectize, 
                               width = width, 
                               size = NULL, 
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
