
#' Extract R session Id from opal object, create a new Datashield R session if not found.
#' @keywords internal
.getDatashieldSessionId <- function(opal) {
  if(is.null(opal$rid)) {
    opal$rid <- .newDatashieldSession(opal, restore=opal$restore)
  }
  if(is.null(opal$rid)) {
    stop("Remote Datashield R session not available")
  }
  return(opal$rid)
}

#' Create a new Datashield R session in Opal.
#' @keywords internal
.newDatashieldSession <- function(opal, restore=NULL) {
  query <- list()
  if (!is.null(restore)) {
    query <- list(restore=restore)  
  }
  res <- opalr::opal.post(opal, "datashield", "sessions", query=query)
  return(res$id)
}

#' Remove a Datashield R session in Opal.
#' @keywords internal
.rmDatashieldSession <- function(opal, save=NULL) {
  query <- list()
  if (is.character(save)) {
    query <- list(save=save)
  }
  try(opalr::opal.delete(opal, "datashield", "session", opal$rid, query=query), silent=TRUE)
}

#' Turn expression into character strings.
#' @keywords internal
.deparse <- function(expr) {
  expression <- deparse(expr)
  if(length(expression) > 1) {
    expression = paste(expression, collapse='\n')
  }
  expression
}

#' @keywords internal
.nullToNA <- function(x) {
  ifelse(is.null(x), NA, x)
}

#' Extract absolute path to the pem file
#' @keywords internal
.getPEMFilePath <- function(pem, directory="~/.ssh") {
  path <- pem
  if (file.access(pem) == 0) {
    # file exists (absolute path)
    path <- path.expand(pem)
  } else if (file.access(paste0(directory, "/", pem)) == 0) {
    # file relative to given dir
    path <- path.expand(paste0(directory, "/", pem))
  } else if (file.access(paste0(getwd(), "/", pem)) == 0) {
    # file relative to working directory
    path <- paste0(getwd(), "/", pem)
  }
  
  path
}


