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

actionButton_cell <- function(inputId, label, icon = NULL, width = "", 
                              leftLabel = TRUE, labelButton = TRUE, ...){
  
  if (length(leftLabel) == 1) leftLabel <- rep(leftLabel, length(inputId))
  if (length(labelButton) == 1) labelButton <- rep(labelButton, length(inputId))
  
  paste0(ifelse(leftLabel & !labelButton, label, ""),
         "<button id='", inputId, "' ",
         ifelse(width == "", "", paste0("style='width:", width, ";' ")), 
         "type='button' class='btn btn-default action-button'>",
         ifelse(labelButton, label, ""),
         "</button>",
         ifelse(!leftLabel & !labelButton, label, ""))
}

#' @rdname actionButtons
#' @export

actionButton_row <- function(inputId, label, icon = NULL, width = NULL,
                             leftLabel = TRUE, labelButton = TRUE, ...){
  
  controls <- actionButton_cell(inputId = inputId, 
                                label = "",
                                width = width,
                                leftLabel = leftLabel,
                                labelButton = labelButton)
  if (leftLabel) data.frame(label, controls, stringsAsFactors = FALSE)
  else data.frame(controls, label, stringsAsFactors = FALSE)
  
}

#' @rdname actionButtons
#' @export

actionLink_cell <- function(inputId, label, icon = NULL, ...){
  paste0("<a id='", inputId, "' ",
         "href='#' class='action-button'>",
         label,
         "</a>")
}