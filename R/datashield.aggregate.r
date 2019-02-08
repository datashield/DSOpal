
#' Data aggregation
#' 
#' Aggregates the expression result using the specified aggregation method in the current Datashield session.
#' This operation is asynchronous and non blocking.
#' 
#' @param opal Opal object or list of opal objects.
#' @param expr Expression to evaluate.
#' @param async Whether the call should be asynchronous.
#' 
#' @return The R command ID to be used to retrieve the command result (when async) or the agregated value.
#' 
#' @keywords internal
.datashield.aggregate <- function(opal, expr, async=TRUE) {
  expression = expr
  # convert a call to a string
  if(is.language(expr)) {
    expression <- .deparse(expr)
  } else if(! is.character(expr) ) {
    stop("Invalid expression type: '", class(expr), "'. Expected a call or character vector.")
  }
  
  query <- list(async=ifelse(async, "true", "false"))
  ignore <- .getDatashieldSessionId(opal)
  opalr::opal.post(opal, "datashield", "session", opal$rid, "aggregate", query=query, body=expression, contentType="application/x-rscript")
}
