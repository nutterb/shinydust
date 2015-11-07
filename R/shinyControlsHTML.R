#' @name shinyControlsHTML
#' @title HTML Code for Shiny Application Controls
#' 
#' @description These utility functions produce the HTML code necessary for the shiny 
#'   application user interfaces.  They are based on the output from the functions in
#'   the \code{shiny} package, but in many cases are slighted altered to permit them
#'   to be placed within a table without having side effects in the table attributes.
#'   For instance, the extra \code{<div>} tags in the usual shiny output will increase 
#'   the height of each cell, removing control of the height of the rows from the 
#'   user.  
#'   
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control. If the control is being displayed as
#'   part of a table, this may often be left blank as the other columns in the table
#'   will likely elaborate on what the control is related to.
#' @param value For checkboxes, a logical value setting the initial status of the checkbox.
#'   
#'
#' @details The functions for generating radio buttons and checkbox groups are 
#'   not included here because they require additional tags around the table to 
#'   make them respond as a group.  The functions provided here do not need to work
#'   as a group.
#'   
#'   These functions do not operate in quite the same manner as their \code{shiny}
#'   counterparts.  As an example, the \code{inputId} argument to \code{checkboxInput}
#'   only accepts single length character vector.  The \code{shiny} dust functions, 
#'   however, are intended to facilitate tabular panels, and are vectorized to 
#'   make it easier to create multiple controls to fill a column in a table.
#'   
#' @author Benjamin Nutter
#' 


#' @rdname shinyControlsHTML
#' @export

checkbox_html <- function(inputId, label="", value=FALSE){
  paste0("<input id='", inputId, "' type='checkbox' class='form-group shiny-input-container checkbox'",
         ifelse(value, " checked='checked'", ""),
         "/>", label)
}

#' @rdname shinyControlsHTML
#' @export

textInput_html <- function(inputId, label="", value=NULL){
  paste0(label,
         "<input id='", inputId, "' type='text' class='form-group shiny-input-container'", 
         " value ='", value, "'/>")
}