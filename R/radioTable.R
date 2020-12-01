#' @rdname checkboxRadioTables
#'   
#' @export

radioTable <- function (tbl, inputId, label = "", 
                        choices, selected = NULL, 
                        table_label = "", control_column = 1L, 
                        pixie = . %>% identity(), 
                        display_table = FALSE, 
                        disabled = FALSE, hidden = FALSE, 
                        disabled_table = FALSE, hidden_table = FALSE, 
                        force_default = TRUE) 
{
  coll <- checkmate::makeAssertCollection()
  checkmate::assertClass(tbl, 
                         classes = "data.frame", 
                         add = coll)
  mapply(checkmate::assertCharacter, 
         list(inputId, table_label, label), 
         .var.name = c("inputId", "table_label", "label"), 
         len = list(1, 1, NULL), 
         MoreArgs = list(add = coll))
  
  if (!is.null(selected)) {
    checkmate::assertCharacter(selected, 
                               len = 1, 
                               add = coll)
  }

  checkmate::assertInteger(control_column, 
                           len = 1, 
                           lower = 1L, 
                           upper = as.integer(ncol(tbl)), 
                           add = coll)
  
  mapply(checkmate::assertLogical, 
         list(display_table, disabled_table, 
              hidden_table, disabled, hidden, force_default), 
         .var.name = c("display_table", "disabled_table", "hidden_table", 
                       "disabled", "hidden", "force_default"), 
         len = list(1, 1, 1, NULL, NULL, 1), 
         MoreArgs = list(add = coll))
  
  checkmate::reportAssertions(coll)

  label <- rep(label, 
               length.out = length(choices))
  disabled <- rep(disabled, 
                  length.out = length(choices))
  hidden <- rep(hidden, 
                length.out = length(choices))
  radio <- radio_html(inputId, 
                      label, 
                      choices, 
                      selected, 
                      disabled = disabled, 
                      hidden = hidden, 
                      force_default = force_default)
  
  tbl <- insert_control_column(tbl, 
                               radio, 
                               control_column)
  
  tbl <- pixiedust::dust(tbl) %>% 
    pixiedust::sprinkle_print_method("html") %>% 
    pixie
  if (display_table) 
    return(tbl)
  
  tbl %>% 
    print(asis = FALSE) %>% 
    paste_radio_group(inputId, 
                      table_label, 
                      disabled_table = disabled_table, 
                      hidden_table = hidden_table)
}

#* Generate a vector of radio buttons to include in a data frame
radio_html <- function (inputId, label, 
                        choices, selected = NULL, 
                        disabled = FALSE, hidden = FALSE, 
                        force_default = TRUE) 
{
  if (force_default && is.null(selected)) 
    selected <- choices[1]
  
  paste0("<input class='radio", 
         ifelse(disabled, " shinyjs-disabled", ""), 
         ifelse(hidden, " shinyjs-hide", ""), 
         "' type='radio' name='", 
         inputId, 
         "' value='", 
         choices, "' ", 
         if (!is.null(selected)) {
           ifelse(choices == selected, "checked = 'checked'", "")
         } else "", 
         "> ", 
         label)
}

#* pastes the code to tie the radio buttons in the table into
#* a group control
paste_radio_group <- function (tbl, inputId, 
                               table_label, disabled_table = FALSE, 
                               hidden_table = FALSE) 
{
  paste0("<div id='", inputId, "' class='form-group shiny-input-radiogroup shiny-input-container", 
         if (disabled_table){
           " shinyjs-disabled"
         } else "", 
         if (hidden_table){
           " shinyjs-hide"
         } else "", 
         "'>", 
         "  <label class='control-label' for='", 
         inputId, 
         "'>", 
         table_label, 
         "</label>", 
         "  <div class='shiny-options-group'>", 
         tbl, 
         "  </div>", "</div>")
}

utils::globalVariables(".")