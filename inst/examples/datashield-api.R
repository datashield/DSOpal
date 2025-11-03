options(verbose=FALSE)
url <- "https://opal-demo.obiba.org"
#url <- "http://localhost:8080"
library(DSOpal)
o <- dsConnect(DSOpal::Opal(), name="server1", username="administrator", password="password", url=url, profile="default")
o
o@opal$context
dsListTables(o)
dsHasTable(o, "CNSIM.CNSIM1")
dsHasTable(o, "CNSIM.CNSIM1xx")

dsListResources(o)
dsHasResource(o, "test.CNSIM1")
dsHasResource(o, "test.CNSIM1xx")

dsIsAsync(o)

dsHasSession(o)
session <- dsSession(o, async = TRUE)
dsIsReady(session)
while (!dsIsReady(session)) {
  Sys.sleep(1)
  cat(dsStateMessage(session), "\n")
}

rbind(dsListMethods(o, type = "aggregate"), dsListMethods(o, type = "assign"))
dsListPackages(o)

res <- dsAssignTable(o, "D", "CNSIM.CNSIM1", async = TRUE)
dsGetInfo(res)
dsFetch(res)

dsListResources(o)
dsHasResource(o, "CNSIM.cnsim3")
res <- dsAssignResource(o, "R1", "CNSIM.cnsim3", async = TRUE)
dsGetInfo(res)
dsFetch(res)

res <- dsAggregate(o, "colnamesDS(D)", async = TRUE)
dsGetInfo(res)
dsFetch(res)

res <- dsAggregate(o, "classDS(D)", async = TRUE)
dsGetInfo(res)
dsFetch(res)

res <- dsAggregate(o, "classDS(R1)", async = TRUE)
dsGetInfo(res)
dsFetch(res)

res <- dsAssignTable(o, "D", "CNSIM.CNSIM1", id.name="id", async = FALSE)
dsGetInfo(res)
dsFetch(res)

res <- dsAggregate(o, "colnamesDS(D)", async = FALSE)
dsGetInfo(res)
dsFetch(res)

dsListSymbols(o)
dsListWorkspaces(o)
dsSaveWorkspace(o, "server1:cnsim1")
dsSaveWorkspace(o, "server1:cnsim1-2")
dsListWorkspaces(o)
dsRmWorkspace(o, "server1:cnsim1")
dsDisconnect(o, save = "server1:xxx")

o <- dsConnect(DSOpal::Opal(), name="server1", username="administrator", password="password", url=url, profile="default", restore="server1:xxx")
dsListWorkspaces(o)
dsListSymbols(o)
dsDisconnect(o)
