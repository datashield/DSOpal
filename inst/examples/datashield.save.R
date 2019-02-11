#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(DSOpal)

# assign data and save in a workspace
data("logindata.opal.demo")
opals <- datashield.login(logindata.opal.demo, assign=TRUE)
datashield.symbols(opals)
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,'length(D$GENDER)')
datashield.workspace_save(opals,'step1')
datashield.assign(opals, 'GENDER', quote(D$GENDER))
datashield.symbols(opals)
datashield.workspace_save(opals,'step2')
datashield.assign(opals, 'LAB_HDL', quote(D$LAB_HDL))
datashield.symbols(opals)
datashield.logout(opals, save='ilovedatashield')

# restore the workspace
opals <- datashield.login(logindata.opal.demo, restore='ilovedatashield')
datashield.symbols(opals)
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,'length(D$GENDER)')
datashield.logout(opals)

# list datashield workspaces
opals <- datashield.login(logindata.opal.demo)
datashield.workspaces(opals)
datashield.workspace_rm(opals, ws='ilovedatashield')
datashield.workspaces(opals)
datashield.logout(opals)

# restore the step1
opals<-datashield.login(logindata.opal.demo, restore='step1')
datashield.symbols(opals)
datashield.logout(opals)

# restore the step2
opals<-datashield.login(logindata.opal.demo, restore='step2')
datashield.symbols(opals)
datashield.logout(opals)

# remove all workspaces
opals<-datashield.login(logindata.opal.demo)
datashield.workspaces(opals)
datashield.workspace_rm(opals, ws='step1')
datashield.workspace_rm(opals, ws='step2')
datashield.workspaces(opals)
datashield.logout(opals)
