#' @name checkboxGroupTable
#' @title Generate a Table with a Checkbox Group Column
#' 
#' @description The checkbox group table allows for display of tabular information
#'   with the option to select multiple items for further analysis.
#'   
#' @param tbl An object that inherits \code{data.frame}
#' @param inputId A string of length 1, as would be passed to the argument
#'   of the same name in \code{checkboxGroupInput}.
#' @param label A character vector of labels to appear next to the check boxes.
#'   Most often, in a tabular display, this function can be served by another
#'   table in the column and \code{label} can be left blank.
#' @param choices A vector or list of choices for the check boxes.  This must
#'   have length equal to \code{nrow(tbl)}.
#' @param selected A value of the choice to be initially selected.
#' @param table_label A character string to be displayed above the table.
#' @param checkbox_column The column position at which the check boxes should
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
#'   \code{\link[shiny]{checkboxGroupInput}}
#'   
#'   \code{\link{radioTable}}
#'   
#' @examples 
#' checkboxGroupTable(tbl = mtcars,
#'                    inputId = "carChoice",
#'                    label = rownames(mtcars), 
#'                    choices = paste0("car", 1:nrow(mtcars)), 
#'                    table_label = "Select Vehicles",
#'                    display_table=TRUE,
#'                    pixie = . %>% sprinkle(bg_pattern_by = "rows"))
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
#'         checkboxGroupTable(inputId = "chooseCar", 
#'                    label = "", 
#'                    choices = paste0("car", 1:nrow(mtcars)), 
#'                    table_label = "Select a Vehicle",
#'                    pixie = . %>% 
#'                    sprinkle(bg_pattern_by = "rows") %>%
#'                    sprinkle_table(pad = 7) %>%
#'                    sprinkle_colnames("rownames(mtcars)" = "",
#'                                      control = ""))
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

checkboxGroupTable <- function(tbl, inputId, label="", choices, selected=NULL,
                               table_label = "",
                               checkbox_column = 1, pixie=. %>% identity(),
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
  
  ArgumentCheck::finishArgCheck(Check)
  
  
  checkbox <- checkboxGroup_html(inputId, label, choices, selected)
  
  tbl <- insert_control_column(tbl, checkbox, checkbox_column)
  
  tbl <- 
    pixiedust::dust(tbl) %>%
    pixiedust::sprinkle_print_method("html") %>%
    pixie 
  
  if (display_table) return(tbl)
  
  tbl %>%
    print(asis = FALSE) %>%
    paste_checkboxGroup_group(inputId, table_label)
}

checkboxGroup_html <- function(inputId, label, choices, selected=NULL){
  if (is.null(selected)) selected <- rep("", length(choices))
  
  paste0("<input type='checkbox' class='checkbox' name='", inputId, "'",
         " value='", choices, "'",
         ifelse(choices %in% selected, " checked='checked'", ""),
         "/>", label)
}

paste_checkboxGroup_group <- function(tbl, inputId, table_label){
  paste0("<div id='", inputId, "' class='form-group shiny-input-checkboxgroup shiny-input-container'> \n",
         "  <label class='control-label' for='", inputId,"'>", table_label, "</label> \n",
         "  <div class='shiny-options-group'>",
         tbl,
         "  </div> \n",
         "</div>")
}