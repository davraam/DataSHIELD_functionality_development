################################################################
#     SCRIPT FOR CHECKING THE DATASHIELD SKEWNESS FUNCTION     #
################################################################

# START UP VMs
# HOW MANY VMs TO USE 1, 2 or 3
use.VMs <- 3

# CHOOSE THE TABLE WITH DATA YOU WANT TO USE
table <- "CNSIM.CNSIM"

# SET DEFAULT WORKING DIRECTORY
setwd("O:/Demetris/DataSHIELD/DataSHIELD_Programming")

# SOURCE CURRENT SET OF DEFAULT CLIENT FUNCTIONS THAT HAVE NOT BEEN ADDED TO dsBase.client
source("O:/Demetris/DataSHIELD/DataSHIELD_Programming/STARTUP.SCRIPTS/CURRENT.EXTRA.CLIENT.SOURCE.FILES.R")

# PACKAGES: devtools
library(devtools)

# PACKAGES: opal, opaladmin
library(opal)
library(opaladmin)

# When the current release is different to the most updated packages the
# following still work 
devtools::load_all("O:/Demetris/DataSHIELD/DataSHIELD_Programming/CLONED.REPOS/dsBaseClient")
devtools::load_all("O:/Demetris/DataSHIELD/DataSHIELD_Programming/CLONED.REPOS/dsStatsClient")
devtools::load_all("O:/Demetris/DataSHIELD/DataSHIELD_Programming/CLONED.REPOS/dsGraphicsClient")
devtools::load_all("O:/Demetris/DataSHIELD/DataSHIELD_Programming/CLONED.REPOS/dsModellingClient")
devtools::load_all("O:/Demetris/DataSHIELD/DataSHIELD_Programming/CLONED.REPOS/dsBetaTestClient")

# LOGIN PROCEDURE
if(use.VMs==1)
{
logindata.VMs.cm <- ds.createLogindata(100, table=table)
opals.cm <- datashield.login(logins=logindata.VMs.cm, assign=TRUE, symbol="D")
opals1.cm <- opals.cm[[1]]
}else
if(use.VMs==2)
{
logindata.VMs.cm <- ds.createLogindata(100, 101, table=table)
opals.cm <- datashield.login(logins=logindata.VMs.cm, assign=TRUE, symbol="D")
opals.cm1 <- opals.cm[[1]]
opals.cm2 <- opals.cm[[2]]
}else
if(use.VMs==3)
{
logindata.VMs.cm <- ds.createLogindata(100, 101, 102, table=table)
opals.cm <- datashield.login(logins=logindata.VMs.cm, assign=TRUE, symbol="D")
opals.cm1 <- opals.cm[[1]]
opals.cm2 <- opals.cm[[2]]
opals.cm3 <- opals.cm[[3]]
}


ds.ListClientsideFunctions()
ds.ListServersideFunctions()

ds.dim('D')
ds.colnames('D')

##################################################################################

# CHECK THE SERVER-SIDE FUNCTION

datashield.aggregate(opals.cm, paste0("skewnessDS(x=", 'D$LAB_TSC', ",", "formula='", 3, "')"))


# CHECK THE CLIENT-SIDE FUNCTION

ds.skewness('D$LAB_TSC')


