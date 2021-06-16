
#' List Datashield profiles
#' 
#' Get available Datashield profiles.
#' 
#' @param opal Opal object.
#' @keywords internal
.datashield.profiles <- function(opal) {
  if (opal.version_compare(opal, "4.2")<0)
    list(available = "default", current = "default")
  else
    list(available = sapply(opalr::opal.get(opal, "datashield", "profiles"), function(p) p$name),
        current = ifelse(nchar(opal$profile) == 0, "default", opal$profile))
}

#' List Datashield methods
#' 
#' Get available Datashield methods of a given type.
#' 
#' @param opal Opal object.
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @keywords internal
.datashield.methods <- function(opal, type="aggregate") {
  opalr::dsadmin.get_methods(opal, type)
}

