#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(DSOpal)

message("**** datashield logins and assignments:")
server <- c("demo1", "demo2", "demo3")
url <- rep("http://localhost:8080", 3)
user <- rep("administrator", 3)
password <- rep("password", 3)
table <- c("datashield.CNSIM1", "datashield.CNSIM2", "datashield.CNSIM3")
logindata <- data.frame(server,url,user,password,table)
opals <- datashield.login(logindata, assign=TRUE, variables=c("GENDER","PM_BMI_CONTINUOUS"))
print(opals)
message("**** check assigned variables:")
datashield.symbols(opals)

message("**** table assignment can also happen later:")
datashield.assign(opals, "T", "datashield.CNSIM1", variables=c("GENDER","PM_BMI_CONTINUOUS"))
datashield.aggregate(opals,'class(T)')

message("**** execute some aggregate calls (if these methods are available in the opals):")
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,quote(length(D$GENDER)))

message("**** clean symbols:")
datashield.rm(opals,'D')
datashield.symbols(opals)

message("**** assign and aggregate arbitrary values:")
datashield.assign(opals, "x", quote(c("1", "2", "3")))
datashield.aggregate(opals,quote(length(x)))
datashield.aggregate(opals,'class(x)')
datashield.assign(opals, "xn", quote(as.numeric(x)))
datashield.aggregate(opals,'class(xn)')

datashield.logout(opals)
