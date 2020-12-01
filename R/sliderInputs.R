#' @name sliderInputs
#' @title Tabular Slider Input Widgets
#' 
#' @description Construct tabular slider widgets to select numeric values from a range
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or \code{""} for no label.
#' @param min The minimum value (inclusive) that can be selected
#' @param max The maximum value (inclusive) that can be selected
#' @param value The initial value of the slider.  A numeric vector of length one will
#'   create a regular slider; a numeric value of length two will create a double ended
#'   range slider. A warning will be issued if the value doesn't fit between \code{min}
#'   and \code{max}.
#' @param step Specifies the interval between each selectable value on the slider.
#' @param round \code{TRUE} to round all values to the nearest integer; \code{FALSE} if
#'   no rounding is desired; or an integer to round to that number of digits (for example,
#'   1 will round to the nearest 10, and -2 will round to the nearest .01). Any rounding will 
#'   be applied after snapping to the nearest step.
#' @param ticks \code{FALSE} to hide tick marks, \code{TRUE} to show them accoring to some
#'   simple heuristics.
#' @param animate \code{TRUE} to show simple animation controls with default settings;
#'   \code{FALSE} not to; or a custom settings list, such as those crated using 
#'   \code{\link[shiny]{animationOptions}}
#' @param width The width of the input, e.g. \code{'400px'} or \code{'100\%'}
#' @param sep Separator between thousands places in numbers.
#' @param pre A prefix string to put in front of the value.
#' @param post A suffix string to put after the value
#' @param timeFormat ONly used if the values are Date or POSIXlt objects. A time format 
#'   string, to be passed to the Javascript strftime library. See 
#'   \url{https://github.com/samsonjs/strftime} for more details.  The allowed format
#'   specifications are very similar, but not idential to those for R's 
#'   \code{\link[base]{strftime}} function. For Dates, the default is \code{"\%F"} (like 
#'   \code{"2015-07-01"}, and for POSIXlt, the default is \code{"\%F \%T"} (like 
#'   \code{"2015-07-01 15:32:10"}.
#' @param timezone Only used if the values are POSIXt objects. A string specifying the time
#'   zone offset for the displayed times, in the format \code{"+HHMM"} or \code{"-HHMM"}.
#'   If \code{NULL} (the default), times will be displayed in the browser's time zone.  The 
#'   value \code{"+0000"} will result in UTC time.
#' @param dragRange This option is used only if it is a range slider (with two values).
#'   If \code{TRUE} (the default), the range can be dragged.  In other words, the min and max
#'   can be dragged together.  If \code{FALSE}, the range cannot be dragged.
#' @param interval The interval, in milliseconds, between each animation step.
#' @param loop \code{TRUE} to automatically restart the animation when it reaches the end.
#' @param playButton Specifies the appearance of the play button.  Valid values are a 
#'   one-element character vector (for a simple text label), an HTML tag or list of tags
#'   (using \code{\link[shiny]{tag}} and friends), or raw HTML (using \code{\link[shiny]{HTML}}
#' @param pauseButton Similar to \code{playButton}, but for the pause button.
#' 

sliderInput_cell <- function()
  stop("Oops, this function hasn't been written yet")
