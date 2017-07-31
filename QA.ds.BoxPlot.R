###########################################
# SCRIPT FOR CHECKING THE BOX PLOT FUNCTION
###########################################

# START UP ANY OF V6 VMs
# HOW MANY VMs TO USE 1, 2 or 3
use.VMs <- 3

# CHOOSE THE TABLE WITH DATA YOU WANT TO USE
table <- "CNSIM.CNSIM"
# table <- "DASIM.DASIM"


# SET DEFAULT WORKING DIRECTORY
setwd("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/DataSHIELD_Programming")

# SOURCE CURRENT SET OF DEFAULT CLIENT FUNCTIONS THAT HAVE NOT BEEN ADDED TO dsBase.client
source("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/DataSHIELD_Programming/STARTUP.SCRIPTS/CURRENT.EXTRA.CLIENT.SOURCE.FILES.R")

#PACKAGES: devtools
library(devtools)

#PACKAGES: opal, opaladmin
library(opal)
library(opaladmin)

#When the current release is different to the most updated packages the
#following still work 
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsBaseClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsStatsClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsGraphicsClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsModellingClient")
devtools::load_all("C:/Users/da15564/Dropbox/Demetris_BRISTOL/DataSHIELD Development/Github/CLONED.REPOS/dsBetaTestClient")

#LOGIN PROCEDURE
if(use.VMs==1)
{
logindata.VMs.cm<-ds.createLogindata(100,table=table)
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
opals1.cm<-opals.cm[[1]]
}else
if(use.VMs==2)
{
logindata.VMs.cm<-ds.createLogindata(100,101,table=table)
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
opals.cm1<-opals.cm[[1]]
opals.cm2<-opals.cm[[2]]
}else
if(use.VMs==3)
{
logindata.VMs.cm<-ds.createLogindata(100,101,102,table=table)
opals.cm <- datashield.login(logins=logindata.VMs.cm,assign=TRUE,symbol="D")
opals.cm1<-opals.cm[[1]]
opals.cm2<-opals.cm[[2]]
opals.cm3<-opals.cm[[3]]
}

#ds.dim('D')
#ds.ListClientsideFunctions()
#ds.ListServersideFunctions()
ds.colnames("D")

##################################################################################

# To check the server-side function:
datashield.aggregate(opals.cm, paste0("BoxPlotDS(", 'D$PM_BMI_CONTINUOUS',")"))


# To check the client-side function:
# Example 1: Plot a combined boxplot of the variable 'LAB_TSC' (default behaviour)
ds.BoxPlot(x='D$LAB_TSC')

# Example 2: Plot distinct bloxplots of 'LAB_TSC' (one per study)
ds.BoxPlot(x='D$LAB_TSC', type='split')

# Example 3: Plot a combined histogram of the variable 'LAB_HDL' showing the notch
ds.BoxPlot(x='D$LAB_HDL', notch=TRUE)

# Example 4: Plot the distinct boxplots of 'LAB_HDL' (one per study) showing the notches
ds.BoxPlot(x='D$LAB_HDL', type='split', notch=TRUE)

# Example 5: D$PM_BMI_CATEGORICAL is of type factor! #Error: The input object must be an integer or numeric vector
ds.BoxPlot('D$PM_BMI_CATEGORICAL')



