
#' Get the DataSHIELD workspaces.
#' 
#' @param opal Opal object.
#' @keywords internal
.datashield.workspaces <- function(opal) {
  if (opalr::opal.version_compare(opal,"2.6")<0) {
    warning(opal$name, ": Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    u <- opal$username
    if (is.null(u) || length(u) == 0) {
      stop("User name is missing or empty.")
    }
    query <- list(context='DataSHIELD', user=u)
    prefix <- paste0('^', opal$name, ':')
    res <- lapply(opalr::opal.get(opal, "service", "r", "workspaces", query=query),
           function(ws) {
             if (grepl(prefix, ws$name)) {
               return(ws)
             }
           })
    wss <- res[lapply(res, is.null) != TRUE]
    if (length(wss)) {
      name <- c()
      user <- c()
      lastAccessDate <- c()
      size <- c()
      for (i in 1:length(wss)) {
        ws <- wss[i]
        name <- c(name, ws[[1]]$name)
        user <- c(user, ws[[1]]$user)
        lastAccessDate <- c(lastAccessDate, ws[[1]]$lastAccessDate)
        size <- c(size, ws[[1]]$size)
      }
      data.frame(name=name, user=user, lastAccessDate=lastAccessDate, size=size)
    } else {
      data.frame()
    }
  }
}

#' Remove a DataSHIELD workspace from a opal.
#' 
#' @param opal Opal object.
#' @param ws The workspace name
#' @keywords internal
.datashield.workspace_rm <- function(opal, ws) {
  u <- opal$username
  if (is.null(u) || length(u) == 0) {
    stop("User name is missing or empty.")
  }
  if (length(ws) == 0) {
    stop("Workspace name is missing or empty.")
  }
  query <- list(context='DataSHIELD', name=ws, user=u)
  if (opalr::opal.version_compare(opal,"2.6")<0) {
    warning(opal$name, ": Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    ignore <- opalr::opal.delete(opal, "service", "r", "workspaces", query=query)
  }
}

#' Save current session in a DataSHIELD workspace.
#' 
#' @param opal Opal object.
#' @param ws The workspace name
#' @keywords internal
.datashield.workspace_save <- function(opal, ws) {
  u <- opal$username
  if (is.null(u) || length(u) == 0) {
    stop("User name is missing or empty.")
  }
  if (length(ws) == 0) {
    stop("Workspace name is missing or empty.")
  }
  query <- list(save=ws)
  if (opalr::opal.version_compare(opal,"2.6")<0) {
    warning(opal$name, ": Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    ignore <- opalr::opal.post(opal, "datashield", "session", opal$rid, "workspaces", query=query) 
  }
}
