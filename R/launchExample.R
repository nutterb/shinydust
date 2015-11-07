#' @name launchExample
#' @title Launch an Example Shiny Application
#' 
#' @description \code{shinydust} contains several examples of applications that illustrate
#'  the functionality of tabular panels.  These can be launched quickly by providing the
#'  name of the example you wish to view.
#'  
#' @param example Character string giving the title of the example application to be
#'   viewed.
#'   
#' @details 
#' \tabular{
#'   Example Name \tab Description \cr
#'   \code{"textInputTable"} \tab A tabular panel with text input fields \cr
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @examples
#' \dontrun{
#'   launchExample("textInputTable")
#' }
#'
#' @export
launchExample <- function(example){
  example <- match.arg(example, c("textInputTable"))
  
  source(
    file.path(system.file("shinyAppExamples", package = "shinydust"),
            paste0(example, ".R"))
  )
  runApp(list(ui=ui, server=server))
  
}