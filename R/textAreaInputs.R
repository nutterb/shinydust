#' @name textAreaInputs
#' @title Create Tabular Text Area Input Controls
#' 
#' @description Create text area inputs, similar to \code{textInput}, but may be 
#'   sized as a box to allow for larger (paragraph) inputs.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or \code{""} for no label.
#' @param value The initial text to be shown in the text box.
#' @param ... Additional named arguments to paste into the text area tag. Some recommended
#'   tags: \code{rows} (determines how many lines of text are displayed in the box);
#'   \code{cols} The width of the text box. Unnamed arguments will be ignored.
#' @param leftLabel Should the label column be the left column or the right column.
#'   \code{TRUE} puts the label on the left column.
#'   
#' @return \code{textArea_cell} returns a character vector the length of \code{inputId}.
#' 
#' \code{textArea_row} returns a data frame with two columns and a row for every 
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
#'         textArea_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                      label = rownames(mtcars),
#'                      rows = 3,
#'                      cols = 40) %>%
#'         dust() %>%
#'         print(asis = FALSE)
#'       })
#'   })
#' )
#' }
#' 
#' @export

textArea_cell <- function (inputId, label = "", value = "", 
                           leftLabel = TRUE, 
                           ..., 
                           disabled = FALSE, hidden = FALSE, readonly = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  mapply(checkmate::assertCharacter, 
         list(inputId, label, value), 
         .var.name = c("inputId", "label", "value"), 
         add = coll)
  mapply(checkmate::assertLogical, 
         list(leftLabel, disabled, hidden), 
         .var.name = c("leftLabel", "disabled", "hidden"), 
         MoreArgs = list(add = coll))
  checkmate::reportAssertions(coll)
  
  dots <- unlist(list(...))
  dots <- dots[names(dots) != ""]
  
  label <- rep(label, 
               length.out = length(inputId))
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  readonly <- rep(readonly, 
                  length.out = length(inputId))
  paste0(label, 
         "<textarea id='", inputId, "' ", 
         paste0(names(dots), "='", dots, "' ", collapse = ""), 
         "class='", 
         ifelse(hidden, " shinyjs-hide", ""), 
         ifelse(disabled, " shinyjs-disabled", ""), "'", 
         ifelse(readonly, " readonly", ""), ">", 
         value, 
         "</textarea>")
}

#' @rdname textAreaInputs
#' @export

textArea_row <- function (inputId, label = "", value = "", 
                          leftLabel = TRUE, 
                          ..., 
                          disabled = FALSE, hidden = FALSE) 
{
  controls <- textArea_cell(inputId = inputId, 
                            label = "", 
                            value = value, 
                            ..., 
                            disabled = disabled, 
                            hidden = hidden)
  if (leftLabel){
    data.frame(label, 
               controls, 
               stringsAsFactors = FALSE)
  } else{
    data.frame(controls, 
               label, 
               stringsAsFactors = FALSE)
  } 
}
