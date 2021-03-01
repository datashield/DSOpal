
#' List R symbols
#' 
#' Get the R symbols available after the datashield.assign calls in the current Datashield session.
#' 
#' @param opal Opal object.
#' @keywords internal
.datashield.symbols <- function(opal) {
  ignore <- .getDatashieldSessionId(opal)
  opalr::opal.get(opal, "datashield", "session", opal$rid, "symbols", acceptType = "application/octet-stream")
}

#' Remove a R symbol
#' 
#' Remove a symbol from the current Datashield session.
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @keywords internal
.datashield.rm <- function(opal, symbol) {
  ignore <- .getDatashieldSessionId(opal)
  res <- opalr::opal.delete(opal, "datashield", "session", opal$rid, "symbol", symbol)
}