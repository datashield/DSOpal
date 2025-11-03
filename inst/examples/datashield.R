#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(DSOpal)

# datashield logins and assignments
url <- "https://opal-demo.obiba.org"
#url <- "http://localhost:8080"
builder <- DSI::newDSLoginBuilder()
builder$append("server1", user="administrator", password="password", url=url, profile="default")
builder$append("server2", user="administrator", password="password", url=url, profile="default")
builder$append("server3", user="administrator", password="password", url=url, profile="default")
login.data <- builder$build()
conns <- datashield.login(login.data)
print(conns)

# start remote R sessions (optional)
datashield.sessions(conns)

# check assigned variables
datashield.symbols(conns)

# table assignment can also happen later
datashield.assign.table(conns, "D", list(server1="CNSIM.CNSIM1", server2="CNSIM.CNSIM2", server3="CNSIM.CNSIM3"))
datashield.aggregate(conns, quote(classDS("D")))

# execute some aggregate calls (if these methods are available in the conns)
datashield.aggregate(conns, quote(colnamesDS("D")))
datashield.aggregate(conns, quote(lengthDS("D$GENDER")))

# clean symbols
datashield.rm(conns, "D")
datashield.symbols(conns)

# assign and aggregate arbitrary values
datashield.assign(conns, "x", quote(c("1", "2", "3")))
datashield.aggregate(conns, quote(lengthDS("x")))
datashield.aggregate(conns, quote(classDS(x)))

datashield.methods(conns, type="aggregate")
datashield.methods(conns$server1, type="aggregate")
datashield.method_status(conns, type="assign")
datashield.pkg_status(conns)
datashield.table_status(conns, list(server1="CNSIM.CNSIM1", server2="CNSIM.CNSIM2", server3="CNSIM.CNSIM3"))

datashield.logout(conns, save = "test")

conns <- datashield.login(login.data, assign=FALSE, restore = "test")
datashield.symbols(conns)
datashield.workspaces(conns)
datashield.workspace_save(conns, "toto")
datashield.workspaces(conns)
datashield.workspace_rm(conns, "toto")
datashield.workspaces(conns)
datashield.logout(conns)

