#
#	PHARMA.R
#
#	Code written by Michael Barrett for ******** Hospital
#
#	This code takes a CSV datafile that lists individual perscriptions
#	by ordering physician, encounter number, patient diagnosis,
#	patient's admission and discharge times for the encounter, and
#	cost of the	perscription, and analyzes it.  The analysis consists
#	of 2 parts.
#	Part I reads the data and organizes it by ordering physician.
#	Part II creates a regression of expenditures based on the data.
#	The goal of this code is to make it eaiser to find spending
#	anomalies between physicians.
#
#	Note: to make a MS Excel file readable by this program, export it.
#		Go to File -> Export -> Change File Type -> CSV and save it. 
#
#	Note II: This program requires the "data.table" and "compiler" 
#		packages (which are included in most distributions)
#	
#------------------------------------------------------------------------
#
#	PART I
#
#	First, let's close the open windows and clear all variables
graphics.off()
rm(list = ls())
#	Record the system time (for diagnostic info) and load some libraries
stm <- Sys.time()
library(data.table)
library(compiler)
#	Let's read the CSV file, via preliminary scan, just to make sure we can allocate enough memory
rawdata1 <- read.csv(file = "PHARMA.csv", header = TRUE, nrows = 2)
#	Define the classes of each column
classes <- sapply(rawdata1, class)
classes[names(classes)] <- "NULL"
classes[c("OrderMedId", "OrderingProviderId", "MedicationId", "EncounterId", "DRG")] <- "character"
classes[c("AdmissionDateTime", "DischargeDateTime")] <- "POSIXct"
classes[c("Cost")] <- "numeric"
#	Now read the raw data in the classes we have set
rawdata <- fread("PHARMA.csv", header = TRUE, colClasses = classes, nrow = 5000, 
 na.strings = c("","**Unavailable"))
#	Note: Read 7859432 rows and 8 (of 81) columns from 8.533 GB file in 00:20:32
#	Create 2 blank matricies to enter data into, and a list that keeps track of the providers
data <- data.table(matrix(0, nrow = length(unique(rawdata$OrderingProviderId[which(rawdata$OrderingProviderId != -1)])), 
 ncol = 2*length(unique(rawdata$DRG[!is.na(rawdata$DRG)]))))
CostData <- data.table(matrix(0, nrow = nrow(data), ncol = length(unique(rawdata$MedicationId))+1))
colnames(data) <- c(paste0("Diagnosis", unique(rawdata$DRG[!is.na(rawdata$DRG)])), 
 paste0("diagnosis", unique(rawdata$DRG[!is.na(rawdata$DRG)])))
colnames(CostData) <- c(paste0("Drug", unique(rawdata$MedicationId)), "Total")
Provider <- list()
#	Let's make a function to apply to each provider
pt1 <- function(i, n) {
#	Find each line in the raw data attributed to provider "i" and make a subset
set1 <- subset(.GlobalEnv$rawdata, (.GlobalEnv$rawdata$OrderingProviderId == i))
#	Get ready to enter data from our subset by having a place for patient data, cost data, and the encounters
dataT <- data.table(matrix(0, nrow = 1, ncol = ncol(.GlobalEnv$data)))
CostDataT <- data.table(matrix(0, nrow = 1, ncol = ncol(.GlobalEnv$CostData)))
colnames(dataT) <- colnames(data)
colnames(CostDataT) <- colnames(CostData)
Encounters <- list()
#	Innitiate a loop to run over the entire subset
for (j in 1:nrow(set1)) {
#	Let's only use this data if it is a complete line of data
if ((any(is.na(set1[j, ])) == FALSE) && ((set1$OrderingProviderId[j] == -1) == FALSE)) {
#	Let's also not enter data that has been entered before in the same encounter
if (set1$EncounterId[j] %in% Encounters == FALSE) {
#	Record this Encounter so we don't repeat it
Encounters[length(Encounters) + 1] <- set1$EncounterId[j]
#	Add the data to the correct part of the dataframe
dataT[1, paste0("Diagnosis", set1$DRG[j]) := (dataT[[paste0("Diagnosis", set1$DRG[j])]][1]  + 1)]
dataT[1, paste0("diagnosis", set1$DRG[j]) := as.numeric(as.numeric(dataT[[paste0("diagnosis", set1$DRG[j])]][1]) +
 as.numeric(difftime(set1$DischargeDateTime[j], set1$AdmissionDateTime[j], units="mins")))]
#
###	Note: "Diagnosis_____" refers to the amount of patients with DRG "_____", while 
###		"diagnosis_____" refers to the total length of stay of patients with DRG 
###		"_____".  The difference is the capitialization of the first letter.
#
}
#	Add the cost data for the line in the subset (even if it belongs to an encounter that has been recroded before)
CostDataT[1, paste0("Drug", set1$MedicationId[j]) := as.numeric(as.numeric(CostDataT[[paste0("Drug", set1$MedicationId[j])]][1]) + as.numeric(set1$Cost[j]))]
}
}
#	Once we've run through the subset, put the extracted data into the master datatable in the global environment
.GlobalEnv$data[n, ] <- dataT[1, ]
.GlobalEnv$CostData[n, ] <- CostDataT[1, ]
}
#	Get ready for the loop by compiling the function, starting a counter, and making a progress bar
pt1c <- cmpfun(pt1)
n <- length(Provider)
pb <- txtProgressBar(min = n + 1, max = length(unique(rawdata$OrderingProviderId)), style = 3)
#	Execute the compiled function for each unique provider
for (i in unique(rawdata$OrderingProviderId[which(rawdata$OrderingProviderId != -1)])) {	
n <- n + 1
pt1c(i, n)
Provider[n] <- i
Sys.sleep(0.0000000000000000000000001)
setTxtProgressBar(pb, n)
}
#	Total up the cost data
CostData <- transform(CostData, Total = rowSums(CostData))
#	Let's also generate some statistics like average x-values and the sum of squares
xbar <- colMeans(data)
xbar <- as.numeric(xbar)
thesum <- 0
for (i in 1:nrow(data)) {
thesum <- thesum + crossprod(as.numeric(data[i, ]-xbar))
}
#	Save all the important data that we have created until this point
save(Provider, data, CostData, xbar, thesum, file = "PartOne.RData")
#
#	PART II
#
#	We need to have an intercept term in the data for later
data[1:nrow(data), "(Intercept)"] <- 1
#	Note: When running from "PartOne.RData", load libraries beforehand
#	Let's make a new progress bar so we know the computer is doing something when we run the code
pb <- txtProgressBar(min = 1, max = length(names(CostData)), style = 3)
#	Let's get ready for the loop by writing and compiling a function
pt2 <- function(i) {
#	Allocate some memory to some blank datatables that we will fill in later
FittedCostData <- data.table(matrix(0, nrow = nrow(.GlobalEnv$CostData), ncol = 1))
ResidualCostData <- data.table(matrix(0, nrow = nrow(.GlobalEnv$CostData), ncol = 1))
CoeffRegressionData <- data.table(matrix(0, nrow = 1, ncol = ncol(data)))
colnames(CoeffRegressionData) <- colnames(data)
SECoeffRegressionData <- data.table(matrix(0, nrow = nrow(data), ncol = ncol(data)))
colnames(SECoeffRegressionData) <- colnames(data)
SECostData <- data.table(matrix(0, nrow = nrow(data), ncol = 1))
SECostDataII <- data.table(matrix(0, nrow = nrow(data), ncol = 1))
SECostDataIII <- data.table(matrix(0, nrow = nrow(data), ncol = 1))
#	Create the regression
lmobject <- lm(.GlobalEnv$CostData[[i]] ~ ., data=subset(data, select = (colnames(data) != "(Intercept)")), model = FALSE)
#	Save the data from the regression's output to the respective datatable
FittedCostData[ , 1 := round(as.numeric(lmobject$fitted.values), digits = 2)]
ResidualCostData[ , 1 := round(as.numeric(lmobject$residuals), digits = 2)]
CoeffRegressionData[1, ] <- lmobject$coefficients + CoeffRegressionData[1, ]
RSq <- round(summary(lmobject)$r.squared, digits = 4)
#	We need to calculate the standard errors of the fitted values before we can save them. We have three methods to calculate them, and I used all of them
SECostData[ , 1 := as.numeric(predict.lm(lmobject, se.fit = TRUE)$se.fit)]
sigsquare <- crossprod(as.numeric(ResidualCostData[1]))/(nrow(.GlobalEnv$CostData)-(ncol(.GlobalEnv$data)+1))
SECostDataII <- sigsquare/nrow(data)+sigsquare*rowSums((data-xbar)^2)/thesum
SECoeff <- summary(lmobject)$coefficients[, 2]
SECoeffRegressionData[1:nrow(data), ] <- SECoeff + SECoeffRegressionData[1, ]
SECostDataIII[ , 1 := sqrt(rowSums((data*as.matrix(SECoeffRegressionData))^2))]
#	Now, let's save the datatables to a file so we can delete everything and still have useful info to refer to later
save(FittedCostData, ResidualCostData, CoeffRegressionData, SECoeffRegressionData, SECostData, SECostDataII, SECostDataIII,
 file = file.path("C:", "Users", "BH171628", "Documents", "PHARMARegressions", paste0(i, ".RData")))
}
#	Compile the function and clear the directory where we save the data (The directory MUST exist before running the code)
pt2c <- cmpfun(pt2)
unlink(file.path("C:", "Users", "BH171628", "Documents", "PHARMARegressions", "*"))
#	Innitiate a counter
j <- 0
#	For each dependant variable (the total expenditure on each drug perscribed) run the function
for (i in names(CostData)) {
#	Let's display a progress bar
j <- j + 1
Sys.sleep(0.0000000000000000000001)
setTxtProgressBar(pb, j)
#	Execute the function
pt2c(i)
#	End the loop, and do it for another dependant variable
}
#	Print the time elapsed
Sys.time() - stm
#
#	For Part II: Time difference of 1.318264 days
#