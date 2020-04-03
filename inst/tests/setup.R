#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Datashield test suite set up
#

library(DSOpal)
library(testthat)

options(verbose=FALSE)

options(opal.username='administrator', 
        opal.password='password', 
        opal.url='https://opal-demo.obiba.org'
        #opal.url='http://localhost:8080'
        )

builder <- newDSLoginBuilder()
builder$append(server="sim1", url=getOption("opal.url"), table="datashield.CNSIM1",
               user=getOption("opal.username"), password=getOption("opal.password"))
builder$append(server="sim2", url=getOption("opal.url"), table="datashield.CNSIM2",
               user=getOption("opal.username"), password=getOption("opal.password"))
builder$append(server="sim3", url=getOption("opal.url"), table="datashield.CNSIM3",
               user=getOption("opal.username"), password=getOption("opal.password"))
logindata <- builder$build()

myvar <- list("LAB_TSC", "LAB_HDL")
opals <- datashield.login(logins = logindata, assign = TRUE, variables = myvar)
