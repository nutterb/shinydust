#' @name downloadButtons
#' @title Action Buttons and Links
#' 
#' @description Create action buttons in tabular input panels.
#' 
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label The contents of the button or link-usually a text label, but
#'   you could also use any other HTML, like an image.
#' @param class Additional CSS classes to apply to the tag, if any.  Currently
#'  not implemented.
#' @param leftLabel Should the label column be the left column or the right column.
#'   \code{TRUE} puts the label on the left column.  This only takes effect when
#'   \code{labelButton = FALSE}.
#' @param labelButton Should the label be placed on the button? If \code{FALSE}, the 
#'   label text appears to the side of the button and the button itself is blank.
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link[shiny]{downloadButton}}
#'
#' @examples 
#' \dontrun{
#' options(pixiedust_print_method = "html")
#' 
#' #* Example of Download Buttons
#' shinyApp(
#'   ui = shinyUI(
#'     wellPanel(uiOutput("sampleUI"))
#'   ),
#'   
#'   server = shinyServer(function(input, output){
#'     output$sampleUI <- 
#'       renderText({ 
#'         downloadButton_row(inputId = paste0("car", 1:nrow(mtcars)),
#'                           label = rownames(mtcars),
#'                           labelButton = c(TRUE, FALSE)) %>%
#'                           dust() %>%
#'                           print(asis = FALSE)
#'       })
#'   })
#' )
#' 
#' 
#' 
#' #' #* Example of Download Links
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
#'             downloadLink_cell(inputId = paste0("car", 1:nrow(mtcars)),
#'                             label = rownames(mtcars)),
#'           stringsAsFactors = FALSE) %>%
#'         dust() %>%
#'         print(asis = FALSE)
#'       })
#'   })
#' )
#' }
#' @export 


downloadButton_cell <- function(inputId, label="Download", class=NULL, 
                                leftLabel = TRUE, labelButton = TRUE){
  if (length(leftLabel) == 1) leftLabel <- rep(leftLabel, length(inputId))
  if (length(labelButton) == 1) labelButton <- rep(labelButton, length(inputId))
  
  paste0(ifelse(leftLabel & !labelButton, label, ""),
         "<a id='", inputId, "' ",
         "class='btn btn-default shiny-download-link ' ",
         "href='' target='_blank'>",
         "<i class='fa fa-download'></i>",
          ifelse(labelButton, label, ""),
         "</a>",
         ifelse(!leftLabel & !labelButton, label, ""))
}

#' @rdname downloadButtons
#' @export

downloadButton_row <- function(inputId, label = "Download", class = NULL,
                               leftLabel = TRUE, labelButton = TRUE){
  controls <- downloadButton_cell(inputId = inputId, 
                                  label = "",
                                  leftLabel = leftLabel,
                                  labelButton = labelButton)
  if (leftLabel) data.frame(label, controls, stringsAsFactors = FALSE)
  else data.frame(controls, label, stringsAsFactors = FALSE)
}

#' @rdname actionButtons
#' @export

downloadLink_cell <- function(inputId, label, icon = NULL, ...){
  paste0("<a id='", inputId, "' ",
         "href='#' class='shiny-download-link' href='' target='_blank'>",
         label,
         "</a>")
}