
#' List the asynchronous commands
#' 
#' Get the list of asynchronous R commands in the remote Datashield session.
#' 
#' @param opal Opal object or list of opal objects.
#' @import opalr
#' @keywords internal
.datashield.commands <- function(opal) {
  if (opal.version_compare(opal,"2.1")<0) return(NULL)
  opalr::opal.get(opal, "datashield", "session", .getDatashieldSessionId(opal), "commands")
}

#' Get an asynchronous command
#' 
#' Get an asynchronous R commands in the remote Datashield session.
#' 
#' @param opal Opal object or list of opal objects.
#' @param id R command ID or list of R command IDs (one for each opal object).
#' @param wait Wait for the command to complete.
#' @import opalr
#' @keywords internal
.datashield.command <- function(opal, id, wait=FALSE) {
  if (is.null(id) || opalr::opal.version_compare(opal,"2.1")<0) return(NULL)
  query <- list()
  if (wait) {
    query["wait"] <- "true"
  }
  opalr::opal.get(opal, "datashield", "session", .getDatashieldSessionId(opal), "command", id, query=query)
}

#' Remove an asynchronous command
#' 
#' Remove an asynchronous R commands in the remote Datashield session.
#' 
#' @param opal Opal object or list of opal objects.
#' @param id R command ID or list of R command IDs (one for each opal object).
#' @import opalr
#' @keywords internal
.datashield.command_rm <- function(opal, id) {
  if (is.null(id) || opalr::opal.version_compare(opal,"2.1")<0) return()
  tryCatch(opalr::opal.delete(opal, "datashield", "session", .getDatashieldSessionId(opal), "command", id), error=function(e){})
}

#' Remove all asynchronous commands
#' 
#' Remove all asynchronous R commands in the remote Datashield session.
#' 
#' @param opal Opal object or list of opal objects.
#' @import opalr
#' @keywords internal
.datashield.commands_rm <- function(opal) {
  if (opalr::opal.version_compare(opal,"2.1")<0) return()
  res <- lapply(.datashield.commands(opal), function(cmd) {
    .datashield.command_rm(opal, cmd$id)
  })
}

#' Get result of an asynchronous command
#' 
#' Get the result of an asynchronous R commands in the remote Datashield session. The command is removed from the
#' remote Datashield session after this call.
#' 
#' @param opal Opal object or list of opal objects.
#' @param id R command ID or list of R command IDs (one for each opal object).
#' @param wait Wait for the command to complete.
#' @import opalr
#' @keywords internal
.datashield.command_result <- function(opal, id, wait=FALSE) {
  if (is.null(id) || opalr::opal.version_compare(opal,"2.1")<0) return(id)
  if (wait) {
    cmd <- .datashield.command(opal, id, wait=TRUE)
    if (cmd$status == "FAILED") {
      msg <- cmd$error
      if (is.null(cmd$error)) {
        msg <- "<no message>"
      }
      stop("Command '", cmd$script, "' failed on '", opal$name,"': ", msg, call.=FALSE)
    }
  }
  opalr::opal.get(opal, "datashield", "session", .getDatashieldSessionId(opal), "command", id, "result")
}
