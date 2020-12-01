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


downloadButton_cell <- function (outputId, label = "Download", class = NULL, 
                                 leftLabel = TRUE, labelButton = TRUE, 
                                 disabled = FALSE, hidden = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  mapply(checkmate::assertCharacter, 
         list(outputId, label), 
         .var.name = c("outputId", "label"), 
         MoreArgs = list(add = coll))
  mapply(checkmate::assertLogical, 
         list(leftLabel, labelButton, disabled, hidden), 
         .var.name = c("leftLabel", "labelButton", "disabled", "hidden"), 
         MoreArgs = list(add = coll))
  leftLabel <- rep(leftLabel, 
                   length.out = length(outputId))
  labelButton <- rep(labelButton, 
                     length.out = length(outputId))
  label <- rep(label, 
               length.out = length(outputId))
  disabled <- rep(disabled, 
                  length.out = length(outputId))
  hidden <- rep(hidden, 
                length.out = length(outputId))
  sprintf(paste0("%s<a id='%s' class='btn btn-default shiny-download-link%s%s' ", 
                 "href='' target='_blank'>", "<i class='fa fa-download'></i>%s", 
                 "</a>%s"), 
          ifelse(leftLabel & !labelButton, label, ""), 
          outputId, 
          ifelse(disabled, " shinyjs-disabled", ""), 
          ifelse(hidden, " shinyjs-hide", ""), 
          ifelse(labelButton, label, ""), 
          ifelse(!leftLabel & !labelButton, label, ""))
}

#' @rdname downloadButtons
#' @export

downloadButton_row <- function(outputId, label = "Download", class = NULL, 
                               leftLabel = TRUE, labelButton = TRUE, 
                               disabled = FALSE, hidden = FALSE) 
{
  controls <- downloadButton_cell(outputId = outputId, 
                                  label = "", 
                                  leftLabel = leftLabel, 
                                  labelButton = labelButton, 
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

#' @rdname actionButtons
#' @export

downloadLink_cell <- function (outputId, label, 
                               disabled = FALSE, hidden = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  
  lapply(list(outputId, label), 
         checkmate::assertCharacter, 
         add = coll)
  lapply(list(disabled, hidden), 
         checkmate::assertLogical, 
         add = coll)
  checkmate::reportAssertions(coll)
  
  sprintf(paste0("<a id='%s' href='#' class='shiny-download-link%s%s' ", 
                 "href='' target='_blank'>%s</a>"), 
          outputId, 
          ifelse(disabled, " shinyjs-disabled", ""), 
          ifelse(hidden, " shinyjs-hide", ""), 
          label)
}
