#' @name passwordInputs
#' @title Tabular Password Input Controls
#' 
#' @description Create a table of input controls for entry of passwords.
#' 
#' @param inputId the \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or "" for no label.
#' @param value Initial value
#' @param width The width of the input, e.g. \code{'400px'} or \code{'100%'}
#' @param diabled A logical vector specifying which controls should be disabled
#'   using \code{shinyjs}. Will be recycled if necessary.
#' @param hidden A logical vector specifying which controls should be hidden 
#'   using \code{shinyjs}. Will be recycled if necessary.
#' @param leftlabel Should the label column be the left column or the right 
#'   column.  \code{TRUE} puts the label on the left column.

passwordInput_cell <- function(inputId, label = "", 
                               value = "", width = "", 
                               disabled = FALSE, hidden = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  mapply(checkmate::assertCharacter, 
         list(inputId, label, value, width), 
         .var.name = c("inputId", "label", "value", "width"), 
         MoreArgs = list(add = coll))
  mapply(checkmate::assertLogical, 
         list(disabled, hidden), 
         .var.name = c("disable", "hidden"), 
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
  paste0(label, 
         "<input id='", inputId, "' ", "type='password' class='form-group shiny-input-container form-control", 
         ifelse(disabled, " shinyjs-disabled", ""), 
         ifelse(hidden, " shinyjs-hide", ""), "' ", 
         "width='", width, "' ", 
         "value='", value, "'/>")
}

#' @rdname passwordInputs
#' @export

passwordInput_row <- function(inputId, label, 
                              value = "", width = "", 
                              leftLabel = TRUE, 
                              disabled = FALSE, hidden = FALSE) 
{
  controls <- passwordInput_cell(inputId = inputId, 
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