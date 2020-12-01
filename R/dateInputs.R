#' @rdname dateInputs
#' @title Create Tabular Date Inputs
#' 
#' @description Create tabular text inputs that, when clicked on, bring up a calendar
#'   that the user can click on to select dates.
#'   
#' @param inputId the input slot that will be used to acces the value
#' @param label Display label for the control, or \code{""} for no label.
#' @param value The starting date. Either a date object, or a string in 
#'   \code{yyyy-mm-dd} format.  If \code{NULL} (the default), will use the
#'   current date in the client's time zone.
#' @param min the minimum allowed date.  Either a date object, or a string in 
#'   \code{yyyy-mm-dd} format.
#' @param max the maximum allowed date.  Either a date object, or a string in 
#'   \code{yyyy-mm-dd} format.
#' @param format The format for the date to display in the browser.  Defaults
#'   to \code{"yyyy-mm-dd"}
#' @param startview The date range shown when the input object is first
#'   clicked.  Can be \code{"month"} (the default), \code{"year"}, or 
#'   \code{"decade"}.
#' @param weekstart Which day is the start of the week.  Should be an integer
#'   from 0 (Sunday) to 6 (Saturday)
#' @param The language used for month and day names.  Default is \code{"en"}.
#'   See \code{\link[shiny]{dateInput}} for other valid options.
#' @param width The width of the input, eg \code{'400px'} or \code{'100\%'}
#' 
#' @details For more about \code{format} strings, see the documentation for 
#'   \code{\link[shiny]{dateInput}}
#'   
#' @return \code{dateInput_cell} returns a character vector the length of \code{inputId}.
#' 
#' \code{dateInput_row} returns a data frame with two columns and a row for every 
#' element in \code{inputId}
#' 
#' @seealso \code{\link[shiny]{dateInput}}, \code{\link[shiny]{dateRangeInput}}
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' \dontrun{
#' options(pixiedust_print_method = "html")
#' shinyApp(
#'   ui = shinyUI(
#'     navbarPage(title = "Example",
#'       tabsetPanel(
#'         tabPanel(title = "Dates",
#'           uiOutput("sampleUI")
#'         )
#'       )
#'     )
#'   ),
#'   
#'   server = shinyServer(function(input, output){
#'     output$sampleUI <- 
#'       renderText({ 
#'         dateInput_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                       label = rownames(mtcars)) %>%
#'         dust() %>%
#'         print(asis = FALSE)
#'       })
#'       
#'   })
#' )
#' }
#' 
#' @export

dateInput_cell <- function(inputId, label = "", value = NULL, 
                           min = NULL, max = NULL, 
                           format = "yyyy-mm-dd", startview = "month", 
                           weekstart = 0, 
                           language = "en", width = "", 
                           emptyValueNoDefault = TRUE, 
                           disabled = FALSE, hidden = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  
  if (!is.null(value)) {
    if (!inherits(value, "Date") & 
        (any(!grepl("\\\\d{4}-\\\\d{2}-\\\\d{2}", value) & 
             as.character(value) != ""))) {
      coll$push("`value` must either be a date object of have the format 'yyyy-mm-dd'")
    }
  }
  if (!is.null(min)) {
    if (!inherits(min, "Date") & 
        (any(!grepl("\\\\d{4}-\\\\d{2}-\\\\d{2}", min) & 
             as.character(min) != ""))) {
      coll$push("`min` must either be a date object of have the format 'yyyy-mm-dd'")
    }
  }
  if (!is.null(max)) {
    if (!inherits(max, "Date") & 
        (any(!grepl("\\\\d{4}-\\\\d{2}-\\\\d{2}", max) & 
             as.character(max) != ""))) {
      coll$push("`max` must either be a date object of have the format 'yyyy-mm-dd'")
    }
  }
  checkmate::assertChoice(weekstart, 
                          choices = 0:6, 
                          add = coll)
  checkmate::assertLogical(disabled, 
                           add = coll)
  checkmate::assertLogical(hidden, 
                           add = coll)
  checkmate::reportAssertions(coll)
  
  if (!is.null(value) & all(grepl("\\\\d{4}-\\\\d{2}-\\\\d{2}", value))) 
    value <- as.Date(value)
  if (is.null(value)) 
    value <- ""
  if (inherits(value, "Date")) 
    value <- format(value, format = "%Y-%m-%d")
  if (is.null(min)) 
    min <- ""
  if (inherits(min, "Date")) 
    min <- format(min, format = "%Y-%m-%d")
  if (is.null(max)) 
    max <- ""
  if (inherits(max, "Date")) 
    max <- format(max, format = "%Y-%m-%d")
  
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  
  sprintf(paste0("<div id='%s' ", "class='shiny-date-input form-group shiny-input-container%s%s'>", 
                 "%s<input class='form-control datepicker' type='text' ", 
                 "data-date-language='%s' data-date-weekstart='%s' ", 
                 "data-date-format='%s' data-date-start-view='%s' ", "%s%s%s%s/></div>"), 
          inputId, 
          ifelse(disabled, " shinyjs-disabled", ""), 
          ifelse(hidden, " shinyjs-hide", ""), 
          label, 
          language, 
          weekstart, 
          format, 
          startview, 
          if (emptyValueNoDefault) {
            sprintf("data-initial-date='%s'", value)
          } else {
            ifelse(value != "", sprintf("data-initial-date='%s'", 
                                        value), "")
          }, 
          ifelse(min != "", sprintf("data-min-date='%s'", min), ""), 
          ifelse(max != "", sprintf("data-max-date='%s'", max), ""), 
          ifelse(width != "", sprintf("width='%s'", width), ""))
}

#' @rdname dateInputs
#' @export

dateInput_row <- function(inputId, label = "", value = NULL, 
                          min = NULL, max = NULL, 
                          format = "yyyy-mm-dd", startview = "month", 
                          weekstart = 0, language = "en", width = "", 
                          leftLabel = TRUE, 
                          disabled = FALSE, hidden = FALSE) 
{
  controls <- dateInput_cell(inputId = inputId, 
                             label = "", 
                             value = value, 
                             min = min, 
                             max = max, 
                             format = format, 
                             startview = startview, 
                             weekstart = weekstart, 
                             language = language, 
                             width = width, 
                             disabled = disabled, 
                             hidden = hidden)
  if (leftLabel){
    data.frame(label, 
               controls, 
               stringsAsFactors = FALSE)
  }
  else {
    data.frame(controls, 
               label, 
               stringsAsFactors = FALSE)
  }
}

