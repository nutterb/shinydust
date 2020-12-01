#' @name actionButtons
#' @title Action Buttons and Links
#' 
#' @description Create action buttons in tabular input panels.
#' 
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label The contents of the button or link-usually a text label, but
#'   you could also use any other HTML, like an image.
#' @param icon An option \code{\link[shiny]{icon}} to appear on the button. 
#'   Currently ignored.
#' @param width The width of the input, e.g. \code{'400px'} or \code{'100\%'}.
#' @param leftLabel Should the label column be the left column or the right column.
#'   \code{TRUE} puts the label on the left column.  This only takes effect when
#'   \code{labelButton = FALSE}.
#' @param labelButton Should the label be placed on the button? If \code{FALSE}, the 
#'   label text appears to the side of the button and the button itself is blank.
#' @param ... Named attributes to be applied to the button or link.  Not yet
#'   implemented.
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link[shiny]{actionButton}}
#' 
#' @examples 
#' \dontrun{
#' options(pixiedust_print_method = "html")
#' 
#' #* Example of Action Buttons
#' shinyApp(
#'   ui = shinyUI(
#'     wellPanel(uiOutput("sampleUI"))
#'   ),
#'   
#'   server = shinyServer(function(input, output){
#'     output$sampleUI <- 
#'       renderText({ 
#'         actionButton_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                           label = rownames(mtcars),
#'                           width = "100px",
#'                           labelButton = c(TRUE, FALSE)) %>%
#'                           dust() %>%
#'                           print(asis = FALSE)
#'       })
#'   })
#' )
#' 
#' #* Example of Action Links
#' shinyApp(
#'   ui = shinyUI(
#'     wellPanel(uiOutput("sampleUI"))
#'   ),
#'   
#'   server = shinyServer(function(input, output){
#'     output$sampleUI <- 
#'       renderText({ 
#'         data.frame(
#'           actionLink = 
#'             actionLink_cell(inputId = paste0("car", 1:nrow(mtcars)),
#'                             label = rownames(mtcars),
#'                             width = "100px",
#'                             labelButton = c(TRUE, FALSE)),
#'           stringsAsFactors = FALSE) %>%
#'         dust() %>%
#'         print(asis = FALSE)
#'       })
#'   })
#' )
#' }
#' @export

actionButton_cell <- function(inputId, label, icon = NULL, 
                              width = "", leftLabel = TRUE, 
                              labelButton = TRUE, disabled = FALSE, 
                              hidden = FALSE, style = "", 
                              ...) 
{
  coll <- checkmate::makeAssertCollection()

  mapply(checkmate::assertCharacter, 
         list(inputId, label, style), 
         .var.name = c("inputId", "label", "style"), 
         MoreArgs = list(add = coll))
  
  mapply(checkmate::assertLogical, 
         list(leftLabel, labelButton, disabled, hidden), 
         .var.name = c("leftLabel", "labelButton", 
                       "disabled", "hidden"), 
         MoreArgs = list(add = coll))
  
  checkmate::reportAssertions(coll)
  
  label <- rep(label, 
               length.out = length(inputId))
  leftLabel <- rep(leftLabel, 
                   length.out = length(inputId))
  labelButton <- rep(labelButton, 
                     length.out = length(inputId))
  width <- rep(width, 
               length.out = length(inputId))
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  style <- rep(style, 
               length.out = length(inputId))
  sprintf("%s<button id='%s' style='%s;%s' type='button' class='btn btn-default action-button%s%s'>%s</button>%s", 
          ifelse(leftLabel & !labelButton, 
                 label, 
                 ""), 
          inputId, 
          ifelse(width == "", 
                 "", 
                 paste0("width:", width)), 
          ifelse(style == "", 
                 "", 
                 paste0(style, ";")), 
          ifelse(disabled, 
                 " shinyjs-disabled ", 
                 ""), 
          ifelse(hidden, 
                 " shinyjs-hide", 
                 ""), 
          ifelse(labelButton, 
                 label, 
                 ""), 
          ifelse(!leftLabel & !labelButton, 
                 label, 
                 ""))
}
#' @rdname actionButtons
#' @export

actionButton_row <- function (inputId, label, icon = NULL, 
                              width = "", leftLabel = TRUE, 
                              labelButton = TRUE, disabled = FALSE, 
                              hidden = FALSE, ...) 
{
  controls <- 
    actionButton_cell(inputId = inputId, 
                      label = if (labelButton) label else "", 
                      width = width, 
                      leftLabel = leftLabel, 
                      labelButton = labelButton, 
                      disabled = disabled, 
                      hidden = hidden)
  if (labelButton) {
    if (leftLabel) 
      data.frame(label = "", 
                 controls, 
                 stringsAsFactors = FALSE)
    else data.frame(controls, 
                    label = "", 
                    stringsAsFactors = FALSE)
  }
  else {
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
}

#' @rdname actionButtons
#' @export

actionLink_cell <- function(inputId, label, icon = NULL, 
                            disabled = FALSE, hidden = FALSE, 
                             ...) 
{
  coll <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(inputId, 
                             add = coll)
  checkmate::assertCharacter(label, 
                             len = length(inputId), 
                             add = coll)
  checkmate::assertLogical(disabled, 
                           add = coll)
  checkmate::assertLogical(hidden, 
                           add = coll)
  checkmate::reportAssertions(coll)
  
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  sprintf("<a class='action-button%s%s'  href='#' id='%s'>%s</a>", 
          ifelse(disabled, 
                 " shinyjs-disabled", 
                 ""), 
          ifelse(hidden, 
                 " shinyjs-hide", 
                 ""), 
          inputId, 
          label)
}