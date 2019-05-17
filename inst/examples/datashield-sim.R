library(DSOpal)
library(dsBaseClient)

data("logindata.opal.demo")
opals<-datashield.login(logindata.opal.demo, assign=T)

datashield.pkg_status(opals)
datashield.method_status(opals)
datashield.methods(opals)

#ds.histogram(x='D$LAB_TSC')
#ds.histogram(x='D$LAB_GLUC_ADJUSTED')
#ds.histogram(x='D$PM_BMI_CONTINUOUS')

ds.summary(x='D$DIS_CVA')
ds.summary(x='D$MEDI_LPD')
ds.summary(x='D$DIS_DIAB')
ds.summary(x='D$GENDER')
ds.summary(x='D$PM_BMI_CATEGORICAL')

datashield.assign(opals, 'G', as.symbol('D$GENDER'))
ds.summary(x='G')
datashield.assign(opals$server1, 'G2', as.symbol('D$GENDER'))
ds.class(x='D$GENDER')
ds.class(x='G2', datasources=list(server1=opals$server1))
ds.summary(x='G2', datasources=list(server1=opals$server1))
datashield.symbols(opals)

datashield.logout(opals)
