#' @rdname checkboxRadioTables
#' @export

checkboxGroupTable <- function(tbl, inputId, label="", choices, selected=NULL,
                               table_label = "",
                               control_column = 1, pixie=. %>% identity(),
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
  
  tbl <- insert_control_column(tbl, checkbox, control_column)
  
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