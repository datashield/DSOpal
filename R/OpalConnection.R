#' @include OpalDriver.R
setOldClass("opal")

#' Class OpalConnection.
#'
#' An Opal connection implementing the DataSHIELD Interface (DSI) \code{\link{DSConnection-class}}.
#' 
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("OpalConnection", contains = "DSConnection", slots = list(opal = "opal"))

#' Connect to a Opal server
#' 
#' Connect to a Opal server, with provided credentials. Does not create a DataSHIELD R session, only retrieves user profile.
#' 
#' @param drv \code{\link{OpalDriver-class}} class object.
#' @param username User name in opal(s). Can be provided by "opal.username" option.
#' @param password User password in opal(s). Can be provided by "opal.password" option.
#' @param url Opal url or list of opal urls. Can be provided by "opal.url" option.
#' @param opts Curl options as described by httr (call httr::httr_options() for details). Can be provided by "opal.opts" option.
#' @param restore Workspace ID to be restored.
#' @param ... Unused, needed for compatibility with generic.
#' 
#' @return A \code{\link{OpalConnection-class}} object.
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsConnect", "OpalDriver", 
          function(drv, username = NULL, password = NULL, url = NULL, opts = list(), restore = NULL, ...) {
            o <- opalr::opal.login(username, password, url, opts, restore)
            con <- new("OpalConnection", opal = o)
            con
          })

#' Disconnect from a Opal server
#' 
#' Disconnect from a Opal server and release all R resources. If a workspace ID is provided, the DataSHIELD
#' R session will be saved before being destroyed.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' @param save Save the DataSHIELD R session with provided ID (must be a character string).
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsDisconnect", "OpalConnection", function(conn, save = NULL) {
  o <- conn@opal
  saveId <- save
  if (!is.null(save)) {
    if (opalr::opal.version_compare(o,"2.6")<0) {
      warning(o$name, ": Workspaces are not available for opal ", o$version, " (2.6.0 or higher is required)")
    }
    saveId <- paste0(o$name, ":", save)
  }
  try(.rmDatashieldSession(o, saveId), silent=TRUE)
  o$rid <- NULL
  opalr::opal.logout(o)
})

#' List Opal tables 
#' 
#' List Opal tables that may be accessible for performing DataSHIELD operations.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return The fully qualified names of the tables.
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListTables", "OpalConnection", function(conn) {
  o <- conn@opal
  tables <- c()
  for (ds in opalr::opal.get(o, "datasources")) {
    if (!is.null(ds$table)) {
      for (tbl in ds$table) {
        tables <- append(tables, paste0(ds$name, ".", tbl))
      }
    }
  }
  tables
})

#' Verify Opal table 
#' 
#' Verify Opal table exist and can be accessible for performing DataSHIELD operations.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object.
#' @param table The fully qualified name of the table.
#' 
#' @return TRUE if table exists.
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsHasTable", "OpalConnection", function(conn, table) {
  o <- conn@opal
  parts <- unlist(strsplit(table, "\\."))
  if (length(parts) > 1) {
    res <- tryCatch(opalr::opal.table(o, datasource = parts[1], table = paste(parts[2:length(parts)], collapse = ".")), 
             error = function(cond) {
               NULL
             })  
    if (is.null(res)) {
      FALSE
    } else {
      TRUE
    }
  } else {
    FALSE
  }
})

#' Opal asynchronous support 
#' 
#' List that Opal supports asynchronicity on all DataSHIELD operations.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return The named list of logicals detailing the asynchronicity support.
#' 
#' @import methods
#' @export
setMethod("dsIsAsync", "OpalConnection", function(conn) {
  list(aggregate = TRUE, assignTable = TRUE, assignExpr = TRUE)
})

#' List R symbols
#' 
#' List symbols living in the DataSHIELD R session.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return A character vector.
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListSymbols", "OpalConnection", function(conn) {
  o <- conn@opal
  .datashield.symbols(o)
})

#' Remove a R symbol
#' 
#' Remoe a symbol living in the DataSHIELD R session.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' @param symbol Name of the R symbol.
#' 
#' @import methods
#' @export
setMethod("dsRmSymbol", "OpalConnection", function(conn, symbol) {
  o <- conn@opal
  .datashield.rm(o, symbol)
})

#' Assign a table
#' 
#' Assign a Opal table in the DataSHIELD R session.
#' 
#' @param conn \code{\link{OpalConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param table Fully qualified name of a table in Opal.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' 
#' @return A \code{\link{OpalResult-class}} object.
#' 
#' @import methods
#' @export
setMethod("dsAssignTable", "OpalConnection", function(conn, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL) {
  o <- conn@opal
  rid <- .datashield.assign(o, symbol, value=table, variables, missings, identifiers)
  new("OpalResult", conn = conn, rid = rid)
})

#' Assign the result of an expression
#' 
#' Assign a result of the execution of an expression in the DataSHIELD R session.
#' 
#' @param conn \code{\link{OpalConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param expr A R expression with allowed assign functions calls.
#' 
#' @return A \code{\link{OpalResult-class}} object.
#' 
#' @import methods
#' @export
setMethod("dsAssignExpr", "OpalConnection", function(conn, symbol, expr) {
  o <- conn@opal
  rid <- .datashield.assign(o, symbol, value=expr)
  new("OpalResult", conn = conn, rid = rid)
})

#' Aggregate data
#'
#' Aggregate some data from the DataSHIELD R session using a valid R expression. The aggregation expression
#' must satisfy the data repository's DataSHIELD configuration.
#'
#' @param conn \code{\link{OpalConnection-class}} object.
#' @param expr Expression to evaluate.
#'
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsAggregate(con, as.symbol("meanDS(D$WEIGHT)"))
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setMethod("dsAggregate", "OpalConnection", function(conn, expr) {
  o <- conn@opal
  rid <- .datashield.aggregate(o, expr)
  new("OpalResult", conn = conn, rid = rid)
})
