######################################################################
#                         START UP 101 VMs                           #
######################################################################

# HOW MANY VMs TO USE 1,2 or 3
use.VMs <- 3

# SET DEFAULT WORKING DIRECTORY
setwd("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/DataSHIELD_Programming")

# SOURCE CURRENT SET OF DEFAULT CLIENT FUNCTIONS THAT HAVE NOT BEEN ADDED TO dsBase.client
source("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/DataSHIELD_Programming/STARTUP.SCRIPTS/CURRENT.EXTRA.CLIENT.SOURCE.FILES.R")

# PACKAGES: devtools
library(devtools)

# PACKAGES: opal, opaladmin
library(opal)
library(opaladmin)


#When the current release is different to the most updated packages the
#following still work 
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsBaseClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsStatsClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsGraphicsClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsModellingClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsBetaTestClient")

###########################################################
############# LOGIN PROCEDURE CNSIM.CNSIMG DATA ###########
###########################################################
if(use.VMs==1)
{
logindata.VMs.cm <- ds.createLogindata.orig(101,table="CNSIM.CNSIM1")
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
}else
if(use.VMs==2)
{
logindata.VMs.cm <- ds.createLogindata.orig(101,101,table=c("CNSIM.CNSIM1","CNSIM.CNSIM2"))
logindata.VMs.cm <- logindata.VMs.cm[1:use.VMs,]
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
}else
if(use.VMs==3)
{
logindata.VMs.cm <- ds.createLogindata.orig(101,101,101,table=c("CNSIM.CNSIM1","CNSIM.CNSIM2","CNSIM.CNSIM3"))
logindata.VMs.cm <- logindata.VMs.cm[1:use.VMs,]
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
}
#######################

ds.dim('D',datasources=opals.cm)
ds.colnames("D",datasources=opals.cm)

ds.ListClientsideFunctions()
ds.ListServersideFunctions()

##################################################################################

# CHECK THE SERVER-SIDE FUNCTION
datashield.aggregate(opals.cm, paste0("synDS(", 'D', ")"))


# CHECK THE CLIENT-SIDE FUNCTION

synth.outcome <- ds.syn('D', datasources=opals.cm[1])


# Check the outcome
S <- synth.outcome$study1$syn
class(S)
names(S)


# Compare synthetic and original data
summary(E$LAB_GLUC_ADJUSTED)
ds.summary('D$LAB_GLUC_ADJUSTED', datasources=opals.cm[1])

mean(S$LAB_TSC, na.rm=TRUE)
ds.mean('D$LAB_TSC', datasources=opals.cm[1])


hist(E$PM_BMI_CONTINUOUS)
windows()
ds.histogram('D$PM_BMI_CONTINUOUS', datasources=opals.cm[1])


glm(E$DIS_DIAB~E$LAB_GLUC_ADJUSTED+E$PM_BMI_CONTINUOUS, family='binomial')
ds.glm(D$DIS_DIAB~D$LAB_GLUC_ADJUSTED+D$PM_BMI_CONTINUOUS, family='binomial', datasources=opals.cm[1])


mat.E <- cbind(E$LAB_TSC, E$PM_BMI_CONTINUOUS, E$LAB_TRIG, E$LAB_HDL)
cov(mat.E, use="complete.obs")
ds.cbind(c('D$LAB_TSC', 'D$PM_BMI_CONTINUOUS', 'D$LAB_TRIG', 'D$LAB_HDL'), newobj="mat.D", datasources=opals.cm[1])
ds.cov('mat.D', datasources=opals.cm[1])


