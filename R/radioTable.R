#' @name radioTable
#' 
#' @title Generate a Table With a Column of Radio Buttons
#' @description The radio button table allows for display of tabular information
#'   with the option to select an option for further analysis.
#'   
#' @param tbl An object that inherits \code{data.frame}
#' @param inputId A string of length 1, as would be passed to the argument
#'   of the same name in \code{radioButtons}.
#' @param label A character vector of labels to appear next to the radio buttons.
#'   Most often, in a tabular display, this function can be served by another
#'   table in the column and \code{label} can be left blank.
#' @param choices A vector or list of choices for the radio buttons.  This must
#'   have length equal to \code{nrow(tbl)}.
#' @param selected A value of the choice to be initially selected.
#' @param table_label A character string to be displayed above the table.
#' @param radio_column The column position at which the radio buttons should
#'   be placed.
#' @param pixie A chain of \code{sprinkle} for customizing the appearance of the 
#'   table.  The chain must start with \code{.} and may take any number of 
#'   commands connected by the \code{\%>\%} operator.
#' @param display_table Logical.  Defaults to \code{FALSE}, which converts the 
#'   the table into a character string suitable for \code{renderText}. When
#'   \code{TRUE}, it prints the table to a viewing pane in order to assist the
#'   user in formatting the table without having to view it in the shiny application.
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link[pixiedust]{dust}}, \code{\link[pixiedust]{sprinkle}},
#'   \code{\link[shiny]{radioButtons}}
#'   
#' @examples
#' radioTable(tbl = mtcars, 
#'   inputId = "chooseCar", 
#'   label = rownames(mtcars), 
#'   choices = paste0("car", 1:nrow(mtcars)), 
#'   table_label = "Select a Vehicle",
#'   display_table=TRUE,
#'   pixie = . %>% sprinkle(bg_pattern_by = "rows"))
#'   
#' \dontrun{
#' library(shiny)
#' library(pixiedust)
#' library(shinydust)
#' 
#' server <- shinyServer(function(input, output) {
#'   output$table <- 
#'     renderText({
#'       cbind(rownames(mtcars), mtcars) %>%
#'         radioTable(inputId = "chooseCar", 
#'                    label = "", 
#'                    choices = paste0("car", 1:nrow(mtcars)), 
#'                    table_label = "Select a Vehicle",
#'                    pixie = . %>% 
#'                    sprinkle(bg_pattern_by = "rows") %>%
#'                    sprinkle_table(pad = 7) %>%
#'                    sprinkle_colnames("rownames(mtcars)" = "",
#'                                      radio = ""))
#'    })
#'    
#' output$choice <- renderText(input$chooseCar)
#' })
#' 
#' ui <- shinyUI(fluidPage(
#'   wellPanel(
#'     verbatimTextOutput("choice"),
#'     uiOutput("table")
#'   )
#' ))
#' 
#' shinyApp(ui = ui, server = server) 
#' }
#'   
#' @export

#* Generate the tags around the table that allow the radio buttons to react
radioTable <- function(tbl, inputId, label="", choices, selected=NULL,
                       table_label = "",
                       radio_column = 1, pixie=. %>% identity(),
                       display_table = FALSE){
  
  Check <- ArgumentCheck::newArgCheck()
  
  if (!inherits(tbl, "data.frame"))
    ArgumentCheck::addError(
      msg = "'tbl' must inherit class 'data.frame'",
      argcheck = Check)
  
  if (length(choices) != nrow(tbl))
    ArgumentCheck::addError(
      msg = "'choices' must have length equal to 'nrow(tbl)'",
      argcheck = Check)
  
  if (length(table_label) != 1){
    if (length(table_label) > 1){
      ArgumentCheck::addWarning(
        msg = "'table_label' has length > 1.  The first element is used",
        argcheck = Check)
      table_label <- table_label[1]
    }
    else {
      ArgumentCheck::addError(
        msg = "'table_label' had length zero and should have length 1.",
        argcheck = Check)
    }
  }
  
  if (!radio_column %in% c(seq_along(tbl), ncol(tbl)+1))
    ArgumentCheck::addWarning(
      msg = paste0("'radio_column' should be a value between 1 and ncol(tbl) + 1 = ",
                   ncol(tbl) + 1, "."),
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)  
  
  radio <- radio_html(inputId, label, choices, selected)

  tbl <- insert_radio(tbl, radio, radio_column)

  tbl <- 
    pixiedust::dust(tbl) %>%
    pixiedust::sprinkle_print_method("html") %>%
    pixie 
  
  if (display_table) return(tbl)
  
  tbl %>%
    print(asis = FALSE) %>%
    paste_radio_group(inputId, table_label)
 
}

#* Generate a vector of radio buttons to include in a data frame
radio_html <- function(inputId, label, choices, selected=NULL) {
  if (is.null(selected)) selected <- choices[1]
  
  paste0("<input class='radio' type='radio' name='", 
         inputId, "' value='", choices,
         "' ",
         ifelse(choices == selected, "checked = 'checked'", ""),
         "> ", label)
}

#* Insert the radio button into the desired column
insert_radio <- function(tbl, radio, radio_column){
  if (radio_column == 1) cbind(radio, tbl)
  else if (radio_column == (ncol(tbl) + 1)) cbind(tbl, radio)
  else cbind(tbl[, 1:(radio_column - 1), drop = FALSE], 
             radio, 
             tbl[, radio_column:ncol(tbl), drop = FALSE])
}

paste_radio_group <- function(tbl, inputId, table_label){
  paste0("<div id='", inputId, "' class='form-group shiny-input-radiogroup shiny-input-container'>",
         "  <label class='control-label' for='", inputId, "'>", table_label, "</label>",
         "  <div class='shiny-options-group'>",
         tbl,
         "  </div>",
         "</div>")
}

utils::globalVariables(".")