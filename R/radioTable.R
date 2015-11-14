#' @rdname checkboxRadioTables
#'   
#' @export

radioTable <- function(tbl, inputId, label="", choices, selected=NULL,
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
  
  if (!control_column %in% c(seq_along(tbl), ncol(tbl)+1))
    ArgumentCheck::addWarning(
      msg = paste0("'control_column' should be a value between 1 and ncol(tbl) + 1 = ",
                   ncol(tbl) + 1, "."),
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)  
  
  radio <- radio_html(inputId, label, choices, selected)

  tbl <- insert_control_column(tbl, radio, control_column)

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

#* pastes the code to tie the radio buttons in the table into
#* a group control
paste_radio_group <- function(tbl, inputId, table_label){
  paste0("<div id='", inputId, "' class='form-group shiny-input-radiogroup shiny-input-container'>",
         "  <label class='control-label' for='", inputId, "'>", table_label, "</label>",
         "  <div class='shiny-options-group'>",
         tbl,
         "  </div>",
         "</div>")
}

utils::globalVariables(".")