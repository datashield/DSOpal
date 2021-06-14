#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(DSOpal)

# datashield logins and assignments
data("logindata.opal.demo")
login.data <- logindata.opal.demo
login.data$url <- rep("http://localhost:8080", 3)
opals <- datashield.login(login.data, assign=T, variables=c("GENDER","PM_BMI_CONTINUOUS"))
print(opals)
# check assigned variables
datashield.symbols(opals)

# table assignment can also happen later
datashield.assign(opals, "T", "CNSIM.CNSIM1", variables=c("GENDER","PM_BMI_CONTINUOUS"))
datashield.aggregate(opals,'classDS(T)')

# execute some aggregate calls (if these methods are available in the opals)
datashield.aggregate(opals,'colnamesDS(D)')
datashield.aggregate(opals,quote(lengthDS(D$GENDER)))

# clean symbols
datashield.rm(opals,'D')
datashield.symbols(opals)

# assign and aggregate arbitrary values
datashield.assign(opals, "x", quote(c("1", "2", "3")))
datashield.aggregate(opals,quote(lengthDS(x)))
datashield.aggregate(opals,'classDS(x)')

datashield.methods(opals, type="aggregate")
datashield.methods(opals$server1, type="aggregate")
datashield.method_status(opals, type="assign")
datashield.pkg_status(opals)
datashield.table_status(opals, list(server1="CNSIM.CNSIM1", server2="CNSIM.CNSIM2", server3="CNSIM.CNSIM3"))

datashield.logout(opals, save = "test")

opals <- datashield.login(logindata.opal.demo, assign=FALSE, restore = "test")
datashield.symbols(opals)
datashield.workspaces(opals)
datashield.workspace_save(opals, "toto")
datashield.workspaces(opals)
datashield.workspace_rm(opals, "toto")
datashield.workspaces(opals)
datashield.logout(opals)

