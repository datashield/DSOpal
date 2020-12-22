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
  rval = "list"))

#' Get result info
#' 
#' Get the information about a command (if still available).
#' 
#' @param dsObj \code{\link{OpalResult-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#' 
#' @return The result information, including its status.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignExpr(con, "C", as.symbol("c(1, 2, 3)"))
#' res <- dsAggregate(con, as.symbol("length(C)"))
#' dsGetInfo(res)
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsGetInfo", "OpalResult", function(dsObj, ...) {
  if (is.null(dsObj@rval$rid)) {
    list(status="COMPLETED")
  } else {
    o <- dsObj@conn@opal
    .datashield.command(o, dsObj@rval$rid, wait=TRUE)  
  }
})

#' Get whether the operation is completed
#' 
#' Get the information about a command (if still available) and return TRUE if
#' the command was completed successfully or not. Always TRUE for synchronous
#' operations.
#' 
#' @param res \code{\link{OpalResult-class}} object.
#' 
#' @return A logical indicating the command completion.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignExpr(con, "C", as.symbol("c(1, 2, 3)"))
#' res <- dsAggregate(con, as.symbol("length(C)"))
#' dsIsCompleted(res)
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsIsCompleted", "OpalResult", function(res) {
  if (is.null(res@rval$rid)) {
    TRUE
  } else {
    o <- res@conn@opal
    cmd <- .datashield.command(o, res@rval$rid, wait=FALSE)
    cmd$status == "COMPLETED" || cmd$status == "FAILED"
  }
})

#' Fetch the result
#' 
#' Fetch the DataSHIELD operation result.
#' 
#' @param res \code{\link{OpalResult-class}} object.
#' 
#' @return TRUE if table exists.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignExpr(con, "C", as.symbol("c(1, 2, 3)"))
#' res <- dsAggregate(con, as.symbol("length(C)"))
#' length <- dsFetch(res)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsFetch", "OpalResult", function(res) {
  if (is.null(res@rval$rid)) {
    res@rval$result
  } else {
    o <- res@conn@opal
    .datashield.command_result(o, res@rval$rid, wait = TRUE)
  }
})


