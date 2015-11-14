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
#'     wellPanel(uiOutput("sampleUI"))
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
                           weekstart = 0, language = "en", width = ""){
  Check <- ArgumentCheck::newArgCheck()
  
  if (!is.null(value)){
    if (!is.Date(value) & any(!grepl("\\d{4}-\\d{2}-\\d{2}", value))){
      ArgumentCheck::addError(
        msg = "'value' must either be a date object of have the format 'yyyy-mm-dd'",
        argcheck = Check)
    }
  }
  
  if (!is.null(min)){
    if (!is.Date(min) & any(!grepl("\\d{4}-\\d{2}-\\d{2}", min))){
      ArgumentCheck::addError(
        msg = "'min' must either be a date object of have the format 'yyyy-mm-dd'",
        argcheck = Check)
    }
  }
  
  if (!is.null(max)){
    if (!is.Date(max) & any(!grepl("\\d{4}-\\d{2}-\\d{2}", max))){
      ArgumentCheck::addError(
        msg = "'max' must either be a date object of have the format 'yyyy-mm-dd'",
        argcheck = Check)
    }
  }
  
  if (!weekstart %in% 0:6)
    ArgumentCheck::addError(
      msg = "'weekstart' but be an integer between 0 and 6, inclusive",
      argcheck = Check)
      
  ArgumentCheck::finishArgCheck(Check)
  
  if (!is.null(value) & all(grepl("\\d{4}-\\d{2}-\\d{2}", value))) value <- as.Date(value)
      
  if (is.null(value)) value <- ""
  if (inherits(value, "Date")) value <- format(value, format = "%Y-%m-%d")
  
  if (is.null(min)) min <- ""
  if (inherits(min, "Date")) min <- format(min, format = "%Y-%m-%d")
  
  if (is.null(max)) max <- ""
  if (inherits(max, "Date")) max <- format(max, format = "%Y-%m-%d")
  
  paste0(label, 
         "<input id='", inputId, "' ",
         "class='shiny-date-input form-group shiny-input-container form-control datepicker' ",
         "type='text' ",
         "data-date-language='", language, "' ",
         "data-date-weekstart='", weekstart, "' ",
         "data-date-format='", format, "' ",
         "data-date-start-view='", startview, "' ",
         ifelse(value != "", paste0("data-initial-date='", value, "' "), ""),
         ifelse(value != "", paste0("data-min-date='", min, "' "), ""),
         ifelse(value != "", paste0("data-max-date='", max, "' "), ""),
         ifelse(width != "", paste0("width='", width, "' "), ""),
         "/>")
      
}

#' @rdname dateInputs
#' @export

dateInput_row <- function(inputId, label = "", value = NULL, 
                          min = NULL, max = NULL, 
                          format = "yyyy-mm-dd", startview = "month", 
                          weekstart = 0, language = "en", width = "",
                          leftLabel = TRUE){
  controls <- dateInput_cell(inputId = inputId, 
                            label = "", 
                            value = value,
                            min = min, 
                            max = max,
                            format = format,
                            startview = startview,
                            weekstart = weekstart,
                            language = language,
                            width = width)
  if (leftLabel) data.frame(label, controls, stringsAsFactors = FALSE)
  else data.frame(controls, label, stringsAsFactors = FALSE)
}