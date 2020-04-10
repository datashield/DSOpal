
#' Class OpalDriver with constructor Opal.
#'
#' An Opal driver implementing the DataSHIELD Interface (DSI) \code{\link{DSDriver-class}}.
#' This class should always be initialized with the \code{\link{Opal}} function.
#' It returns a singleton that allows you to connect to Opal.
#' 
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("OpalDriver", contains = "DSDriver")

#' Create a Opal driver
#' 
#' Convenient function for creating a OpalDriver object.
#' 
#' @import methods
#' @import DSI
#' @export
Opal <- function() {
  new("OpalDriver")
}
