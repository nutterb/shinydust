#' @name checkboxRadioTables
#' @title Generate a Table with a Checkbox or Radio Button Column
#' 
#' @description The checkbox table allows for display of tabular information
#'   with the option to select multiple items for further analysis.  The difference
#'   between \code{checkboxTable} and \code{checkboxGroupTable} is how the inputs
#'   are stored--the controls in a \code{checkboxTable} all have independent control
#'   names and are stored as logical.  The controls in a \code{checkboxGroupTable}
#'   act as a group and the inputs are stored as a character vector.  \code{RadioTable} 
#'   is similar to \code{checkboxGroupTable}, but only allows the user to select one
#'   value from the table.
#'   
#' @param tbl An object that inherits \code{data.frame}
#' @param inputId A string of length 1, as would be passed to the argument
#'   of the same name in \code{checkboxInput}.
#' @param label A character vector of labels to appear next to the check boxes.
#'   Most often, in a tabular display, this function can be served by another
#'   table in the column and \code{label} can be left blank.
#' @param value A logical vector setting the initial status of the check box.  This must
#'   have length 1 or equal to \code{nrow(tbl)}.
#' @param choices List of values to show checkboxes for. If elements of the list are named 
#'   then that name rather than the value is displayed to the user.
#' @param selected The values that should be initially selected, if any.  For radio buttons, 
#'   this must be of length 1.
#' @param table_label A character string to be displayed above the table.
#' @param control_column The column position at which the check boxes should
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
#'   \code{\link[shiny]{checkboxInput}}, \code{\link[shiny]{checkboxGroupInput}},
#'   \code{\link[shiny]{radioButtons}}
#'   
#' @examples 
#' checkboxTable(tbl = mtcars,
#'               inputId = paste0("carChoice", 1:nrow(mtcars)),
#'               label = rownames(mtcars), 
#'               value = FALSE, 
#'               table_label = "Select Vehicles",
#'               display_table=TRUE,
#'               pixie = . %>% sprinkle(bg_pattern_by = "rows"))
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
#'         checkboxTable(inputId = paste0("chooseCar", 1:nrow(mtcars)),
#'                    label = "", 
#'                    value = FALSE, 
#'                    table_label = "Select a Vehicle",
#'                    pixie = . %>% 
#'                    sprinkle(bg_pattern_by = "rows") %>%
#'                    sprinkle_table(pad = 7) %>%
#'                    sprinkle_colnames("rownames(mtcars)" = "",
#'                                      control = ""))
#'    })
#'    
#' output$chooseCar1 <- renderText(paste0("Mazda RX4: ", input$chooseCar1))
#' output$chooseCar2 <- renderText(paste0("Mazda RX4 Wag: ", input$chooseCar2))
#' })
#' 
#' ui <- shinyUI(fluidPage(
#'   wellPanel(
#'     verbatimTextOutput("chooseCar1"),
#'     verbatimTextOutput("chooseCar2"),
#'     uiOutput("table")
#'   )
#' ))
#' 
#' shinyApp(ui = ui, server = server) 
#' }
#' 
#' 
#' 
#' 
#' #****************************          
#' #* Checkbox Group Table
#' 
#'checkboxGroupTable(tbl = mtcars,
#'                    inputId = "carChoice",
#'                    label = rownames(mtcars), 
#'                    choices = paste0("car", 1:nrow(mtcars)), 
#'                    table_label = "Select Vehicles",
#'                    display_table=TRUE,
#'                    pixie = . %>% sprinkle(bg_pattern_by = "rows"))          
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
#' 
#' #***********************************
#' #* Radio Button Table
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
#' @export

checkboxTable <- function (tbl, inputId, label = "", value = FALSE, 
                           table_label = "", control_column = 1L, 
                           pixie = . %>% identity(), 
                           disabled = FALSE, hidden = FALSE, 
                           display_table = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  checkmate::assertClass(tbl, 
                         classes = "data.frame", 
                         add = coll)
  mapply(checkmate::assertLogical, 
         list(value, disabled, hidden, display_table), 
         .var.name = c("value", "disabled", "hidden", "display_table"), 
         len = list(NULL, NULL, NULL, 1), 
         MoreArgs = list(add = coll))
  mapply(checkmate::assertCharacter, 
         list(table_label, label), 
         .var.name = c("table_label", "label"), 
         len = list(1, NULL), 
         MoreArgs = list(add = coll))
  
  checkmate::assertInteger(control_column, 
                           len = 1, 
                           add = coll)
  checkmate::reportAssertions(coll)
  
  label <- rep(label, 
               length.out = length(inputId))
  disabled <- rep(disabled, 
                  length.out = length(inputId))
  hidden <- rep(hidden, 
                length.out = length(inputId))
  checkbox <- checkboxInput_cell(inputId, 
                                 label, 
                                 value, 
                                 disabled = disabled, 
                                 hidden = hidden)
  tbl <- insert_control_column(tbl, 
                               checkbox, 
                               control_column)
  tbl <- pixiedust::dust(tbl) %>% 
    pixiedust::sprinkle_print_method("html") %>% 
    pixie
  if (display_table) return(tbl)
  
  tbl %>% print(asis = FALSE)
}

