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
setClass("OpalConnection", contains = "DSConnection", slots = list(name = "character", opal = "opal"))

#' Connect to a Opal server
#' 
#' Connect to a Opal server, with provided credentials. Does not create a DataSHIELD R session, only retrieves user profile.
#' 
#' @param drv \code{\link{OpalDriver-class}} class object.
#' @param name Name of the connection, which must be unique among all the DataSHIELD connections.
#' @param restore Workspace name to be restored in the newly created DataSHIELD R session.
#' @param username User name in opal(s).
#' @param password User password in opal(s).
#' @param token Personal access token (since opal 2.15, ignored if username is specified).
#' @param url Opal url or list of opal urls. Can be provided by "opal.url" option.
#' @param opts Curl options as described by httr (call httr::httr_options() for details). Can be provided by "opal.opts" option.
#' @param profile The DataSHIELD R server profile (affects the R packages available and the applied configuration). If not provided or not supported, default profile will be applied.
#' @param ... Unused, needed for compatibility with generic.
#' 
#' @return A \code{\link{OpalConnection-class}} object.
#' 
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1", "username", "password", "https://opal.example.org")
#' con
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsConnect", "OpalDriver", 
          function(drv, name, restore = NULL, username = NULL, password = NULL, token = NULL, url = NULL, opts = list(), profile = NULL, ...) {
            o <- opalr::opal.login(username, password, token, url, opts, profile=profile, restore=restore)
            o$name <- name
            con <- new("OpalConnection", name = name, opal = o)
            con
          })


#' Keep connection with a Opal server alive
#' 
#' Makes a dummy web service request.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1", "username", "password", "https://opal.example.org")
#' dsKeepAlive(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsKeepAlive", "OpalConnection", function(conn) {
  tryCatch(dsListSymbols(conn), error = function(e) {})
})

#' Disconnect from a Opal server
#' 
#' Disconnect from a Opal server and release all R resources. If a workspace ID is provided, the DataSHIELD
#' R session will be saved before being destroyed.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' @param save Save the DataSHIELD R session with provided ID (must be a character string).
#' 
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1", "username", "password", "https://opal.example.org")
#' con
#' dsDisconnect(con)
#' }
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
  }
  try(.rmDatashieldSession(o, save), silent=TRUE)
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
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsListTables(con)
#' dsDisconnect(con)
#' }
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
  if (length(tables)) {
    tables 
  } else {
    vector(mode="character", length = 0)
  }
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
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsHasTable(con, "test.CNSIM")
#' dsDisconnect(con)
#' }
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

#' List Opal resources 
#' 
#' List Opal resources that may be accessible for performing DataSHIELD operations.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return The fully qualified names of the resources.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsListResources(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListResources", "OpalConnection", function(conn) {
  o <- conn@opal
  resources <- c()
  for (proj in opalr::opal.get(o, "projects")) {
    for (res in opalr::opal.resources(o, proj$name, df = FALSE)) {
      resources <- append(resources, paste0(proj$name, ".", res$name))
    }
  }
  if (length(resources)) {
    resources 
  } else {
    vector(mode="character", length = 0)
  }
})

#' Verify Opal resource 
#' 
#' Verify Opal resource exist and can be accessible for performing DataSHIELD operations.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object.
#' @param resource The fully qualified name of the resource.
#' 
#' @return TRUE if the resource exists.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsHasResource(con, "test.CNSIM")
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsHasResource", "OpalConnection", function(conn, resource) {
  o <- conn@opal
  parts <- unlist(strsplit(resource, "\\."))
  if (length(parts) > 1) {
    res <- tryCatch(opalr::opal.resource(o, project = parts[1], resource = paste(parts[2:length(parts)], collapse = ".")), 
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
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsIsAsync(con)
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsIsAsync", "OpalConnection", function(conn) {
  list(aggregate = TRUE, assignTable = TRUE, assignResource = TRUE, assignExpr = TRUE)
})

#' List R symbols
#' 
#' List symbols living in the DataSHIELD R session.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return A character vector.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsListSymbols(con)
#' dsDisconnect(con)
#' }
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
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsRmSymbol(con, "D")
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsRmSymbol", "OpalConnection", function(conn, symbol) {
  o <- conn@opal
  .datashield.rm(o, symbol)
})

#' List profiles
#' 
#' List profiles defined in the DataSHIELD configuration.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return A list containing the "available" character vector of profile names and the "current" profile (in case a default one was assigned).
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsListProfiles(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListProfiles", "OpalConnection", function(conn) {
  o <- conn@opal
  .datashield.profiles(o)
})

#' List methods
#' 
#' List methods defined in the DataSHIELD configuration.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' @param type Type of the method: "aggregate" (default) or "assign".
#' 
#' @return A data frame with columns: name, type ('aggregate' or 'assign'), class ('function' or 'script'), value, package, version.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsListMethods(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListMethods", "OpalConnection", function(conn, type = "aggregate") {
  o <- conn@opal
  .datashield.methods(o, type)
})

#' List packages
#' 
#' List packages defined in the DataSHIELD configuration.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return A data frame with columns: name, version.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsListPackages(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListPackages", "OpalConnection", function(conn) {
  o <- conn@opal
  methods <- rbind(dsListMethods(conn, type = "aggregate"), dsListMethods(conn, type = "assign"))
  unique(methods[,5:6])
})

#' List workspaces
#' 
#' List workspaces saved in the data repository.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' 
#' @return A data frame with columns: name, lastAccessDate, size.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsListWorkspaces(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListWorkspaces", "OpalConnection", function(conn) {
  o <- conn@opal
  .datashield.workspaces(o)
})

#' Save workspace
#' 
#' Save workspace on the data repository.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' @param name Name of the workspace.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsSaveWorkspace(con, "foo")
#' dsListWorkspaces(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsSaveWorkspace", "OpalConnection", function(conn, name) {
  o <- conn@opal
  .datashield.workspace_save(o, name)
})

#' Remove a workspace
#' 
#' Remove a workspace on the data repository.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
#' @param name Name of the workspace.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsSaveWorkspace(con, "foo")
#' dsListWorkspaces(con)
#' dsRmWorkspace(con, "foo")
#' dsListWorkspaces(con)
#' dsDisconnect(con)
#' }
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsRmWorkspace", "OpalConnection", function(conn, name) {
  o <- conn@opal
  .datashield.workspace_rm(o, name)
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
#' @param id.name Name of the column that will contain the entity identifiers. If not specified, the identifiers
#'   will be the data frame row names. When specified this column can be used to perform joins between data frames.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#' 
#' @return A \code{\link{OpalResult-class}} object.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsAssignTable", "OpalConnection", function(conn, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, async=TRUE) {
  o <- conn@opal
  rval <- .datashield.assign.table(o, symbol, value=table, variables, missings, identifiers, id.name, async=async)
  if (async) {
    new("OpalResult", conn = conn, rval = list(rid = rval, result = NULL)) 
  } else {
    new("OpalResult", conn = conn, rval = list(rid = NULL, result = rval))
  }
})

#' Assign a resource
#' 
#' Assign a Opal resource in the DataSHIELD R session.
#' 
#' @param conn \code{\link{OpalConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param resource Fully qualified name of a resource in Opal.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#' 
#' @return A \code{\link{OpalResult-class}} object.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignResource(con, "D", "test.CNSIM")
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsAssignResource", "OpalConnection", function(conn, symbol, resource, async=TRUE) {
  o <- conn@opal
  rval <- .datashield.assign.resource(o, symbol, value=resource, async=async)
  if (async) {
    new("OpalResult", conn = conn, rval = list(rid = rval, result = NULL)) 
  } else {
    new("OpalResult", conn = conn, rval = list(rid = NULL, result = rval))
  }
})

#' Assign the result of an expression
#' 
#' Assign a result of the execution of an expression in the DataSHIELD R session.
#' 
#' @param conn \code{\link{OpalConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param expr A R expression with allowed assign functions calls.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#' 
#' @return A \code{\link{OpalResult-class}} object.
#' 
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignExpr(con, "C", as.symbol("c(1, 2, 3)"))
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsAssignExpr", "OpalConnection", function(conn, symbol, expr, async=TRUE) {
  o <- conn@opal
  rval <- .datashield.assign.expr(o, symbol, value=expr, async=async)
  if (async) {
    new("OpalResult", conn = conn, rval = list(rid = rval, result = NULL)) 
  } else {
    new("OpalResult", conn = conn, rval = list(rid = NULL, result = rval))
  }
})

#' Aggregate data
#'
#' Aggregate some data from the DataSHIELD R session using a valid R expression. The aggregation expression
#' must satisfy the data repository's DataSHIELD configuration.
#'
#' @param conn \code{\link{OpalConnection-class}} object.
#' @param expr Expression to evaluate.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsAggregate(con, as.symbol("meanDS(D$WEIGHT)"))
#' dsDisconnect(con)
#' }
#' 
#' @import methods
#' @export
setMethod("dsAggregate", "OpalConnection", function(conn, expr, async=TRUE) {
  o <- conn@opal
  rval <- .datashield.aggregate(o, expr, async)
  if (async) {
    new("OpalResult", conn = conn, rval = list(rid = rval, result = NULL)) 
  } else {
    new("OpalResult", conn = conn, rval = list(rid = NULL, result = rval))
  }
})
