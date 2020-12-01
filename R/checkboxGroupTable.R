#' @rdname checkboxRadioTables
#' @export

checkboxGroupTable <- function(tbl, inputId, label = "", choices, 
                               selected = NULL, table_label = "", 
                               control_column = 1L, pixie = . %>% identity(), 
                               display_table = FALSE, disabled = FALSE, 
                               hidden = FALSE, 
                               disabled_table = FALSE, hidden_table = FALSE) 
{
  coll <- checkmate::makeAssertCollection()
  checkmate::assertClass(tbl, 
                         classes = "data.frame", 
                         add = coll)
  
  mapply(checkmate::assertCharacter, 
         list(label, choices, table_label), 
         .var.name = c("label", "choices", "table_label"), 
         len = list(NULL, nrow(tbl), NULL), 
         min.len = list(NULL, NULL, 1), 
         MoreArgs = list(add = coll))
  
  checkmate::assertInteger(control_column, 
                           len = 1, 
                           add = coll)
  
  mapply(checkmate::assertLogical, 
         list(display_table, disabled, hidden, disabled_table, hidden_table), 
         .var.name = c("display_table", "disabled", "hidden", 
                       "disabled_table", "hidden_table"), 
         len = list(1, NULL, NULL, 1, 1), 
         MoreArgs = list(add = coll))
  
  checkmate::reportAssertions(coll)
  
  disabled <- rep(disabled, 
                  length.out = length(choices))
  hidden <- rep(hidden, 
                length.out = length(choices))
  checkbox <- checkboxGroup_html(inputId, 
                                 label, 
                                 choices, 
                                 selected, 
                                 disabled = disabled, 
                                 hidden = hidden)
  tbl <- insert_control_column(tbl, 
                               checkbox, 
                               control_column)
  tbl <- pixiedust::dust(tbl) %>% 
    pixiedust::sprinkle_print_method("html") %>% 
    pixie
  
  if (display_table) 
    return(tbl)
  
  tbl %>% 
    print(asis = FALSE) %>% 
    paste_checkboxGroup_group(inputId, 
                              table_label, 
                              disabled_table = disabled_table, 
                              hidden_table = hidden_table)
}

checkboxGroup_html <- function(inputId, label, choices, selected = NULL, 
                               disabled = FALSE, hidden = FALSE) 
{
  if (is.null(selected)) 
    selected <- rep("", 
                    length(choices))
  
  sprintf("<input type='checkbox' class='checkbox%s%s' name='%s' value='%s'%s/>%s", 
          ifelse(disabled, 
                 " shinyjs-disabled ", 
                 ""), 
          ifelse(hidden, 
                 " shinyjs-hide", 
                 ""), 
          inputId, 
          choices, 
          ifelse(choices %in% selected, 
                 " checked='checked'", 
                 ""), 
          label)
}

paste_checkboxGroup_group <- function(tbl, inputId, table_label, 
                                      disabled_table = FALSE, 
                                      hidden_table = FALSE) 
{
  sprintf(paste0("<div id='%s' class='form-group shiny-input-checkboxgroup ", 
                 "shiny-input-container%s%s'>\\n  ", "  <label class='control-label' for'%s'>%s</label>\\n", 
                 "  <div class='shiny-options-group'>", "%s </div>\\n</div>"), 
          inputId, 
          if (disabled_table) " shinyjs-disabled " else "", 
          if (hidden_table) " shinyjs-hide" else "", 
          inputId, 
          table_label, 
          tbl)
}