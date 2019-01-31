
#' Data assignment
#' 
#' Assign a Opal value to a R symbol in the current Datashield session.
#' This operation is asynchronous and non blocking.
#' 
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value Fully qualified name of a variable or a table in Opal (must be the same in each Opal) or a R expression with allowed assign functions calls.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param tibble Assign table to a tibble (from tidyverse) instead of a plain data.frame.
#' 
#' @return The R command ID if the async flag is TRUE and if the wait flag is FALSE and if Opal version is at least 2.1, NULL otherwise.
#'
#' @examples 
#' \dontrun{
#' # assign a list of variables from table HOP of opal object o
#' .datashield.assign(o, symbol="D", value="demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' .datashield.assign(o, symbol="D", value="demo.HOP", variables="name().matches('LAB_')")
#' }
#' @keywords internal
.datashield.assign <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, tibble=FALSE) {
  if(is.language(value) || is.function(value)) {
    contentType <- "application/x-rscript"
    body <- .deparse(value)
    query <- list()
  } else if(is.character(value)) {
    contentType <- "application/x-opal"
    body <- value
    variableFilter <- NULL
    if (is.character(variables)) {
      if (length(variables) > 1) {
        # case variables is a char vector of variable names
        variableFilter <- as.list(variables)
      } else {  
        # case variables is a magma script
        variableFilter <- variables
      }
    } else if (is.list(variables)) {
      # case variables is a list of variable names
      variableFilter <- variables
    }
    
    # make a script from a list of variable names
    if (is.list(variableFilter)) {
      variableFilter <- paste("name().any('", paste(variableFilter, sep="", collapse="','"), "')", sep="")
    }
    query <- list(missings=missings, variables=variableFilter)
    if (!is.null(identifiers)) {
      query["identifiers"] <- identifiers
    }
  } else {
    stop("Invalid value type: '", class(value), "'. Use quote() to protect from early evaluation.")
  }
  
  query["async"] <- "true"
  if (tibble) {
    query["class"] <- "tibble"
  }
  ignore <- .getDatashieldSessionId(opal)
  opalr::opal.put(opal, "datashield", "session", opal$rid, "symbol", symbol, query=query, body=body, contentType=contentType)
}
