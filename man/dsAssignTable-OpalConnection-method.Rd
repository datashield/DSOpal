% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OpalConnection.R
\name{dsAssignTable,OpalConnection-method}
\alias{dsAssignTable,OpalConnection-method}
\title{Assign a table}
\usage{
\S4method{dsAssignTable}{OpalConnection}(
  conn,
  symbol,
  table,
  variables = NULL,
  missings = FALSE,
  identifiers = NULL,
  id.name = NULL,
  async = TRUE
)
}
\arguments{
\item{conn}{\code{\link{OpalConnection-class}} object.}

\item{symbol}{Name of the R symbol.}

\item{table}{Fully qualified name of a table in Opal.}

\item{variables}{List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: https://opaldoc.obiba.org/en/latest/magma-user-guide/methods.html}

\item{missings}{If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.}

\item{identifiers}{Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).}

\item{id.name}{Name of the column that will contain the entity identifiers. If not specified, the identifiers
will be the data frame row names. When specified this column can be used to perform joins between data frames.}

\item{async}{Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
the connections, when the connection supports that feature, with an extra overhead of requests.}
}
\value{
A \code{\link{OpalResult-class}} object.
}
\description{
Assign a Opal table in the DataSHIELD R session.
}
\examples{
\dontrun{
con <- dbConnect(DSOpal::Opal(), "server1",
  "username", "password", "https://opal.example.org")
dsAssignTable(con, "D", "test.CNSIM")
dsDisconnect(con)
}

}
