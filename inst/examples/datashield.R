#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(DSOpal)

# datashield logins and assignments
server <- c("demo1", "demo2", "demo3")
url <- rep("http://localhost:8080", 3)
user <- rep("administrator", 3)
password <- rep("password", 3)
table <- c("datashield.CNSIM1", "datashield.CNSIM2", "datashield.CNSIM3")
logindata <- data.frame(server,url,user,password,table)
logindata
opals <- datashield.login(logindata, assign=F, variables=c("GENDER","PM_BMI_CONTINUOUS"))
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
datashield.methods(opals$demo1, type="aggregate")
datashield.method_status(opals, type="assign")
datashield.pkg_status(opals)
datashield.table_status(opals, list(demo1="datashield.CNSIM1", demo2="datashield.CNSIM2", demo3="datashield.CNSIM3"))

datashield.logout(opals, save = "test")

opals <- datashield.login(logindata, assign=FALSE, restore = "test")
datashield.symbols(opals)
datashield.workspaces(opals)
datashield.workspace_save(opals, "toto")
datashield.workspaces(opals)
datashield.workspace_rm(opals, "toto")
datashield.workspaces(opals)
datashield.logout(opals)

