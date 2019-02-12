#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(DSOpal)

# datashield logins and assignments
data("logindata.opal.demo")
opals <- datashield.login(logindata.opal.demo, assign=T, variables=c("GENDER","PM_BMI_CONTINUOUS"))
print(opals)
# check assigned variables
datashield.symbols(opals)

# table assignment can also happen later
datashield.assign(opals, "T", "datashield.CNSIM1", variables=c("GENDER","PM_BMI_CONTINUOUS"))
datashield.aggregate(opals,'class(T)')

# execute some aggregate calls (if these methods are available in the opals)
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,quote(length(D$GENDER)))

# clean symbols
datashield.rm(opals,'D')
datashield.symbols(opals)

# assign and aggregate arbitrary values
datashield.assign(opals, "x", quote(c("1", "2", "3")))
datashield.aggregate(opals,quote(length(x)))
datashield.aggregate(opals,'class(x)')
datashield.assign(opals, "xn", quote(as.numeric(x)))
datashield.aggregate(opals,'class(xn)')

datashield.methods(opals, type="aggregate")
datashield.methods(opals$server1, type="aggregate")
datashield.method_status(opals, type="assign")
datashield.pkg_status(opals)
datashield.table_status(opals, list(server1="datashield.CNSIM1", server2="datashield.CNSIM2", server3="datashield.CNSIM3"))

datashield.logout(opals, save = "test")

opals <- datashield.login(logindata.opal.demo, assign=FALSE, restore = "test")
datashield.symbols(opals)
datashield.workspaces(opals)
datashield.workspace_save(opals, "toto")
datashield.workspaces(opals)
datashield.workspace_rm(opals, "toto")
datashield.workspaces(opals)
datashield.logout(opals)

