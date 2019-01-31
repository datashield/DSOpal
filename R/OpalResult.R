#' @include OpalDriver.R OpalConnection.R
NULL

#' Class OpalResult.
#'
#' An Opal result implementing the DataSHIELD Interface (DSI)  \code{\link{DSResult-class}}.
#' 
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("OpalResult", contains = "DSResult", slots = list(
  conn = "OpalConnection",
  rid = "character"))

#' Get result info
#' 
#' Get the information about a command (if still available).
#' 
#' @param dsObj \code{\link{OpalResult-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#' 
#' @return The result information, including its status.
#' 
#' @import methods
#' @export
setMethod("dsGetInfo", "OpalResult", function(dsObj, ...) {
  o <- dsObj@conn@opal
  .datashield.command(o, dsObj@rid, wait=TRUE)
})

#' Fetch the result
#' 
#' Fetch the DataSHIELD operation result.
#' 
#' @param res \code{\link{OpalResult-class}} object.
#' 
#' @return TRUE if table exists.
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsFetch", "OpalResult", function(res) {
  o <- res@conn@opal
  .datashield.command_result(o, res@rid, wait = TRUE)
})


