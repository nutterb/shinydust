#' @name insert_control_column
#' @title Insert a Column of Controls Into a Table
#' 
#' @description The tabular panel functions generate the column of controls that then 
#'   need to be added to the table.  The use of this function permits the controls to be
#'   placed as any column in the table, instead of just the first, or last.
#'   
#' @param tbl An object that inherits \code{data.frame}.
#' @param control A vector of controls the same length at \code{tbl}.
#' @param control_column The ultimately desired column position of the control.
#' 
#' @details This is not an exported function, but is documented because it is used
#'   by multiple tabular panel functions.  Since it isn't exported, no 
#'   argument checks are performed--it is assumed that the arguments have been
#'   checked for appropriateness before they get passed to \code{insert_control_column}.
#'   
#'   When \code{control_column == 1}, the controls are placed in the first column and 
#'   the column index of all other columns increments by 1.  
#'   
#'   When \code{control_column == (ncol(tbl) + 1)}, the control column is simply appended
#'   as the last column in the table.
#'   
#'   When \code{control_column} is an integer between 2 and \code{ncol(tbl)}, it is at 
#'   the specified column index, and the column indices beyond \code{control_column}
#'   are incremented by 1.
   


insert_control_column <- function(tbl, control, control_column){
  if (control_column == 1) cbind(control, tbl)
  else if (control_column == (ncol(tbl) + 1)) cbind(tbl, control)
  else cbind(tbl[, 1:(control_column - 1), drop = FALSE], 
             control, 
             tbl[, control_column:ncol(tbl), drop = FALSE])
}