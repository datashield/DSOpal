#' @include OpalDriver.R OpalConnection.R
NULL

#' Class OpalSession.
#'
#' An Opal session implementing the DataSHIELD Interface (DSI)  \code{\link[DSI]{DSSession-class}}.
#' 
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("OpalSession", contains = "DSSession", slots = list(
  conn = "OpalConnection",
  rval = "list"))

#' Get whether the remote R session is up and running
#' 
#' Get the state of the remote R session and return TRUE if
#' the state is RUNNING. Always TRUE for synchronous
#' operations.
#' 
#' @param session \code{\link{OpalSession-class}} object.
#' @return A logical indicating the readiness of the session.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "administrator", "password", "https://opal-demo.obiba.org")
#' session <- dsSession(con, async = TRUE)
#' ready <- dsIsReady(session)
#' while (!ready) {
#'   Sys.sleep(1)
#'   ready <- dsIsReady(session)
#'   cat(".")
#' }
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsIsReady", "OpalSession", function(session) {
  if (is.null(session)) {
    FALSE
  } else {
    o <- session@conn@opal
    opalr::opal.session_running(o)
  }
})

#' Get the remote R session state message
#' 
#' Explain the remote R session state as a human-readable message.
#' 
#' @param session \code{\link{OpalSession-class}} object.
#' @return A character string
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "administrator", "password", "https://opal-demo.obiba.org")
#' session <- dsSession(con, async = TRUE)
#' ready <- dsIsReady(session)
#' while (!ready) {
#'   Sys.sleep(1)
#'   ready <- dsIsReady(session)
#'   cat(dsStateMessage(session), "\n")
#' }
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import utils
#' @import methods
#' @export
setMethod("dsStateMessage", "OpalSession", function(session) {
  if (is.null(session)) {
    FALSE
  } else {
    o <- session@conn@opal
    last_message <- utils::tail(opalr::opal.session_events(o)$message, 1)[[1]]
    if (is.null(last_message) || is.na(last_message) || last_message == "") {
      if (opalr::opal.version_compare(o, "5.3.0") >= 0) {
        "No recent events"
      } else {
        "Ready"
      }
    } else {
      last_message
    }
  }
})


