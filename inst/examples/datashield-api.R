library(DSOpal)
o <- dsConnect(DSOpal::Opal(), name="server1", username="administrator", password="password", url="http://localhost:8080")
o
dsListTables(o)
dsHasTable(o, "datashield.CNSIM1")
dsHasTable(o, "datashield.CNSIM1xx")
dsIsAsync(o)
rbind(dsListMethods(o, type = "aggregate"), dsListMethods(o, type = "assign"))
dsListPackages(o)

res <- dsAssignTable(o, "D", "datashield.CNSIM1", async = TRUE)
dsGetInfo(res)
dsFetch(res)

res <- dsAggregate(o, "colnames(D)", async = TRUE)
dsGetInfo(res)
dsFetch(res)

res <- dsAssignTable(o, "D", "datashield.CNSIM1", id.name="id", async = FALSE)
dsGetInfo(res)
dsFetch(res)

res <- dsAggregate(o, "colnames(D)", async = FALSE)
dsGetInfo(res)
dsFetch(res)


dsListSymbols(o)
dsListWorkspaces(o)
dsSaveWorkspace(o, "server1:cnsim1")
dsSaveWorkspace(o, "server1:cnsim1-2")
dsListWorkspaces(o)
dsRmWorkspace(o, "server1:cnsim1")
dsDisconnect(o, save = "server1:xxx")

o <- dsConnect(DSOpal::Opal(), name="server1", username="administrator", password="password", url="http://localhost:8080", restore="server1:xxx")
dsListWorkspaces(o)
dsListSymbols(o)
dsDisconnect(o)