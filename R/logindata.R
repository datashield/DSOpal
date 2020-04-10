#' DataSHIELD login data file
#' 
#' DataSHIELD login data file based on Opal demo server, with CNSIM simulated data. The CNSIM datasets contain
#' synthetic data based on a model derived from the participants of the 1958 Birth Cohort,
#' as part of the obesity methodological development project. These datasets do contain some
#' NA values. Note that the Opal demo server is rebuilt every day and is possibly not accessible.
#'
#' | **Field**          | **Description**                  | **Type** | **Note** |
#' | ------------------ | -------------------------------- | -------- | -------- |
#' | server             | Server/study name                | char     ||
#' | url                | Server/study URL                 | char     | Opal demo URL |
#' | user               | User name                        | char     ||
#' | password           | User password                    | char     ||
#' | table              | Table unique name                | char     | CNSIM tables |
#' | driver             | Connection driver                | char     | OpalDriver |
#' 
#' @name logindata.opal.demo
#' @docType data
#' @references \url{https://opal-demo.obiba.org}
#' @keywords data
NULL