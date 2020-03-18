# analyze tree list outputs for accuracy
# (Validation Step 2)
# written by Karin Riley, 5/8/2018

library(foreign)
options("scipen"= 100, digits=8)

curzone <- "z2"
curfolder <- paste("F:\\Tree_List_c2014\\outputs\\", curzone, "_disturb", sep="")

xtable <- read.table(paste("F:\\Tree_List_c2014\\outputs\\", curzone, "_disturb\\", curzone, "_x_table_allplots_reclass.txt", sep=""), header=T, sep=",")

# EVC -------------------------------------------------------------------------------------------------------------
# read in attribute tables from combine for EVC, EVH, and EVG
# EVC values = 15, 25, 35, 45, 55, 65, 75, 85, 95
evcs <- read.dbf(paste(curfolder, "\\EVC_combine.dbf", sep=""))
evcvec <- c(15, 25, 35, 45, 55, 65, 75, 85, 95)
# make confusion matrix
evcmat <- matrix(data=0, nrow=9, ncol=9, byrow=FALSE)
for (j in 1:length(evcvec))
{
  tempvec <- which(evcs[,3]==evcvec[j]) # column 3 = imputed
  tempmat <- evcs[tempvec,]
  for (k in 1:length(evcvec))
  {
    temprow <- which(tempmat[,4]==evcvec[k])  # column 4 = reference
    if (length(temprow==1)) { evcmat[j,k] <- tempmat[temprow,2] } # assign count to matrix
  }
}
# columns of evcmat are reference and rows are imputed
sum(evcs$COUNT)==sum(evcmat)
sum(evcmat)  # compare to number of pixels in raster attribute table as a check
row.names(evcmat) <- evcvec
colnames(evcmat) <- evcvec
# add producer's and user's accuracy to matrix
accuracy3 <- as.numeric()
accuracy4 <- as.numeric()
for(i in 1:length(evcvec))
{
  accuracy3[i] <- evcmat[i,i]/sum(evcmat[i,])
  accuracy4[i] <- evcmat[i,i]/sum(evcmat[,i])
}
evcmat2 <- cbind(evcmat, accuracy3)
evcmat2 <- rbind(evcmat2, c(accuracy4, -99))
row.names(evcmat2) <- c(evcvec, "accuracy")
colnames(evcmat2) <- c(evcvec, "accuracy")
# calculate overall accuracy = 96% ! 
evcmat2[dim(evcmat2)[[1]], dim(evcmat2)[[2]]]  <- (evcmat2[1,1] + evcmat2[2,2] + evcmat2[3,3] + evcmat2[4,4] + evcmat2[5,5] + evcmat2[6,6] + evcmat2[7,7] + evcmat2[8,8] + evcmat2[9,9])/sum(evcmat)
write.table(evcmat2, paste(curfolder, "\\", curzone, "_EVC_confusion_matrix.txt", sep=""))
# bar chart
evcplottotals <- c(sum(evcmat[,1]), sum(evcmat[,2]), sum(evcmat[,3]), sum(evcmat[,4]), sum(evcmat[,5]), sum(evcmat[,6]), sum(evcmat[,7]), sum(evcmat[,8]), sum(evcmat[,9]))
sum(evcplottotals)
evcpolytotals <- c(sum(evcmat[1,]), sum(evcmat[2,]), sum(evcmat[3,]), sum(evcmat[4,]), sum(evcmat[5,]), sum(evcmat[6,]), sum(evcmat[7,]), sum(evcmat[8,]), sum(evcmat[9,]))
sum(evcpolytotals)
evcmat3 <- rbind(evcpolytotals, evcplottotals)
row.names(evcmat3) <- c("imputed", "reference")
colnames(evcmat3) <- evcvec
barplot(evcmat3, col=c("aquamarine3", "chartreuse4"), beside=TRUE, legend=rownames(evcmat3))
box()
canopycovvec <- sort(unique(xtable$canopy_cover))
canopycovcount <- as.numeric()
for (j in 1:length(canopycovvec))
{
  canopycovcount[j] <- length(which(xtable$canopy_cover==canopycovvec[j]))
}
sum(canopycovcount)==dim(xtable)[[1]]
plot(canopycovcount, accuracy4, xlab="Count of plots in canopy cover class", ylab="Accuracy")
canopyacc <- cbind(canopycovvec, canopycovcount, accuracy4)
write.table(canopyacc, paste(curfolder, "\\", curzone, "_EVC_plotcount_vs_accuracy.txt", sep=""))

# EVH ---------------------------------------------------------------------------------------------------------------
# EVH values = 3, 8, 18, 38 (some zones will have one higher height category)
evhs <- read.dbf(paste(curfolder, "\\EVH_combine.dbf", sep=""))
evhvec <- c(3, 8, 18, 38, 70)
# make confusion matrix
evhmat <- matrix(data=0, nrow=5, ncol=5, byrow=FALSE)
for (j in 1:length(evhvec))
{
  tempvec <- which(evhs[,3]==evhvec[j])
  tempmat <- evhs[tempvec,]
  for (k in 1:length(evhvec))
  {
    temprow <- which(tempmat[,4]==evhvec[k])
    if (length(temprow==1)) { evhmat[j,k] <- tempmat[temprow,2] } # assign count to matrix
  }
}
sum(evhmat)==sum(evhs$COUNT)
row.names(evhmat) <- evhvec
colnames(evhmat) <- evhvec
# add producer's and user's accuracy to matrix
accuracy3 <- as.numeric()
accuracy4 <- as.numeric()
for(i in 1:length(evhvec))
{
  accuracy3[i] <- evhmat[i,i]/sum(evhmat[i,])
  accuracy4[i] <- evhmat[i,i]/sum(evhmat[,i])
}
evhmat2 <- cbind(evhmat, accuracy3)
evhmat2 <- rbind(evhmat2, c(accuracy4, -99))
# calculate overall accuracy = 99.7% ! 
evhmat2[dim(evhmat2)[[1]], dim(evhmat2)[[2]]]  <- (evhmat2[1,1] + evhmat2[2,2] + evhmat2[3,3] + evhmat2[4,4] + evhmat2[5,5])/sum(evhmat)
write.table(evhmat2, paste(curfolder, "\\", curzone, "_EVH_confusion_matrix.txt", sep=""))
# bar chart
evhplottotals <- c(sum(evhmat[,1]), sum(evhmat[,2]), sum(evhmat[,3]), sum(evhmat[,4]), sum(evhmat[,5]))
sum(evhplottotals)
evhpolytotals <- c(sum(evhmat[1,]), sum(evhmat[2,]), sum(evhmat[3,]), sum(evhmat[4,]), sum(evhmat[5,]))
sum(evhpolytotals)
evhmat3 <- rbind(evhpolytotals, evhplottotals)
row.names(evhmat3) <- c("imputed", "reference")
colnames(evhmat3) <- evhvec
barplot(evhmat3, col=c("aquamarine3", "chartreuse4"), beside=TRUE, legend=rownames(evhmat3))
box()
canopyhtvec <- sort(unique(xtable$canopy_height))
canopyhtcount <- as.numeric()
for (j in 1:length(canopyhtvec))
{
  canopyhtcount[j] <- length(which(xtable$canopy_height==canopyhtvec[j]))
}
sum(canopyhtcount)==dim(xtable)[[1]]
plot(canopyhtcount, accuracy4, xlab="Count of plots in canopy height class", ylab="Accuracy")
canopyacc2 <- cbind(canopyhtvec, canopyhtcount, accuracy4)
write.table(canopyacc2, paste(curfolder, "\\", curzone, "_EVH_plotcount_vs_accuracy.txt", sep=""))



# EVG ----------------------------------------------------------------------------------------------------------------
# EVG values (get them from table)
evgs <- read.dbf(paste(curfolder, "\\EVG_combine.dbf", sep=""))
evgvec <- sort(unique(c(evgs[,3],evgs[,4])))
# make confusion matrix
evgmat <- matrix(data=0, nrow=length(evgvec), ncol=length(evgvec), byrow=FALSE)
for (j in 1:length(evgvec))
{
  tempvec <- which(evgs[,3]==evgvec[j])
  tempmat <- evgs[tempvec,]
  for (k in 1:length(evgvec))
  {
    temprow <- which(tempmat[,4]==evgvec[k])
    if (length(temprow==1)) { evgmat[j,k] <- tempmat[temprow,2] } # assign count to matrix
  }
}
sum(evgmat)==sum(evgs$COUNT)
row.names(evgmat) <- evgvec
colnames(evgmat) <- evgvec
# add producer's and user's accuracy to matrix
accuracy3 <- as.numeric()
accuracy4 <- as.numeric()
sum1 <- 0
sumrow <- as.numeric()
sumcol <- as.numeric()
for(i in 1:length(evgvec))
{
  accuracy3[i] <- evgmat[i,i]/sum(evgmat[i,])
  accuracy4[i] <- evgmat[i,i]/sum(evgmat[,i])
  sum1 <- sum1 + evgmat[i,i]
  sumrow[i] <- sum(evgmat[i,])
  sumcol[i] <- sum(evgmat[,i])
}
evgmat2 <- cbind(evgmat, accuracy3)
evgmat2 <- rbind(evgmat2, c(accuracy4, -99))
# calculate overall accuracy = 92% ! 
evgmat2[dim(evgmat2)[[1]], dim(evgmat2)[[2]]]  <- sum1/sum(evgmat)
write.table(evgmat2, paste(curfolder, "\\", curzone, "_EVG_confusion_matrix.txt", sep=""))
# bar chart
evgplottotals <- colSums(evgmat)
sum(evgplottotals)
evgpolytotals <- rowSums(evgmat)
sum(evgpolytotals)
evgmat3 <- rbind(evgpolytotals, evgplottotals)
row.names(evgmat3) <- c("imputed", "reference")
colnames(evgmat3) <- evgvec
barplot(evgmat3, col=c("aquamarine3", "chartreuse4"), beside=TRUE, legend=rownames(evgmat3))
box()
evgvec <- sort(unique(xtable$EVT_GP))
evgcount <- as.numeric()
for (j in 1:length(evgvec))
{
  evgcount[j] <- length(which(xtable$EVT_GP==evgvec[j]))
}
sum(evgcount)==dim(xtable)[[1]]
plot(evgcount, accuracy4, xlab="Count of plots in Existing Vegetation Group", ylab="Accuracy")
evgacc <- cbind(evgvec, evgcount, accuracy4)
write.table(evgacc, paste(curfolder, "\\", curzone, "_EVG_plotcount_vs_accuracy.txt", sep=""))



# disturbance code ----------------------------------------------------------------------------------------
dcs <- read.dbf(paste(curfolder, "\\disturb_code_combine.tif.vat.dbf", sep=""))
dcvec <- c(0,1,2)
# make confusion matrix
dcmat <- matrix(data=0, nrow=3, ncol=3, byrow=FALSE)
for (j in 1:length(dcvec))
{
  tempvec <- which(dcs[,3]==dcvec[j])
  tempmat <- dcs[tempvec,]
  for (k in 1:length(dcvec))
  {
    temprow <- which(tempmat[,4]==dcvec[k])
    if (length(temprow==1)) { dcmat[j,k] <- tempmat[temprow,2] } # assign count to matrix
  }
}
sum(dcmat)==sum(dcs$COUNT)
row.names(dcmat) <- dcvec
colnames(dcmat) <- dcvec
# add producer's and user's accuracy to matrix
accuracy3 <- as.numeric()
accuracy4 <- as.numeric()
for(i in 1:length(dcvec))
{
  accuracy3[i] <- dcmat[i,i]/sum(dcmat[i,])
  accuracy4[i] <- dcmat[i,i]/sum(dcmat[,i])
}
dcmat2 <- cbind(dcmat, accuracy3)
dcmat2 <- rbind(dcmat2, c(accuracy4, -99))
# calculate overall accuracy = 72.5%  
dcmat2[dim(dcmat2)[[1]], dim(dcmat2)[[2]]]  <- (dcmat2[1,1] + dcmat2[2,2] + dcmat2[3,3])/sum(dcmat)
write.table(dcmat2, paste(curfolder, "\\", curzone, "_disturb_code_confusion_matrix.txt", sep=""))
# bar chart
dcplottotals <- c(sum(dcmat[,1]), sum(dcmat[,2]), sum(dcmat[,3]))
sum(dcplottotals)
dcpolytotals <- c(sum(dcmat[1,]), sum(dcmat[2,]), sum(dcmat[3,]))
sum(dcpolytotals)
dcmat3 <- rbind(dcpolytotals, dcplottotals)
row.names(dcmat3) <- c("imputed", "reference")
colnames(dcmat3) <- dcvec
barplot(dcmat3, col=c("aquamarine3", "chartreuse4"), beside=TRUE, legend=rownames(dcmat3))
box()
dcvec2 <- sort(unique(xtable$disturb_code))
dccount <- as.numeric()
for (j in 1:length(dcvec2))
{
  dccount[j] <- length(which(xtable$disturb_code==dcvec2[j]))
}
sum(dccount)==dim(xtable)[[1]]
plot(dccount, accuracy4, xlab="Count of plots in disturbance code class", ylab="Accuracy")
dcacc2 <- cbind(dcvec, dccount, accuracy4)
write.table(dcacc2, paste(curfolder, "\\", curzone, "_disturb_code_plotcount_vs_accuracy.txt", sep=""))


# disturbance year ------------------------------------------------------------------------------------------
dys <- read.dbf(paste(curfolder, "\\disturb_year_combine.tif.vat.dbf", sep=""))
##dyvec <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,99)
dyvec <- sort(unique(c(dys$DISTURB_YE, dys$DISTURB__1)))
# make confusion matrix
dymat <- matrix(data=0, nrow=length(dyvec), ncol=length(dyvec), byrow=FALSE)
for (j in 1:length(dyvec))
{
  tempvec <- which(dys[,3]==dyvec[j])
  tempmat <- dys[tempvec,]
  for (k in 1:length(dyvec))
  {
    temprow <- which(tempmat[,4]==dyvec[k])
    if (length(temprow==1)) { dymat[j,k] <- tempmat[temprow,2] } # assign count to matrix
  }
}
sum(dymat)==sum(dys$COUNT)
row.names(dymat) <- dyvec
colnames(dymat) <- dyvec
# add producer's and user's accuracy to matrix
accuracy3 <- as.numeric()
accuracy4 <- as.numeric()
for(i in 1:length(dyvec))
{
  accuracy3[i] <- dymat[i,i]/sum(dymat[i,])
  accuracy4[i] <- dymat[i,i]/sum(dymat[,i])
}
dymat2 <- cbind(dymat, accuracy3)
dymat2 <- rbind(dymat2, c(accuracy4, -99))
# calculate overall accuracy = 72.5%  
dymat2[dim(dymat2)[[1]], dim(dymat2)[[2]]]  <- (dymat2[1,1] + dymat2[2,2] + dymat2[3,3] + dymat2[4,4] + dymat2[5,5] + dymat2[6,6] + dymat2[7,7] + dymat2[8,8] + dymat2[9,9] + dymat2[10,10] + dymat2[11,11] + dymat2[12,12] + dymat2[13,13] + dymat2[14,14] + dymat2[15,15] + dymat2[16,16] + dymat2[17,17])/sum(dymat)
write.table(dymat2, paste(curfolder, "\\", curzone, "_disturb_year_confusion_matrix.txt", sep=""))
# bar chart
dyplottotals <- c(sum(dymat[,1]), sum(dymat[,2]), sum(dymat[,3]), sum(dymat[,4]), sum(dymat[,5]), sum(dymat[,6]), sum(dymat[,7]), sum(dymat[,8]), sum(dymat[,9]), sum(dymat[,10]), sum(dymat[,11]), sum(dymat[,12]), sum(dymat[,13]), sum(dymat[,14]), sum(dymat[,15]), sum(dymat[,16]), sum(dymat[,17]))
sum(dyplottotals)
dypolytotals <- c(sum(dymat[1,]), sum(dymat[2,]), sum(dymat[3,]), sum(dymat[4,]), sum(dymat[5,]), sum(dymat[6,]), sum(dymat[7,]), sum(dymat[8,]), sum(dymat[9,]), sum(dymat[10,]), sum(dymat[11,]), sum(dymat[12,]), sum(dymat[13,]), sum(dymat[14,]), sum(dymat[15,]), sum(dymat[16,]), sum(dymat[17,]))
sum(dypolytotals)
dymat3 <- rbind(dypolytotals, dyplottotals)
row.names(dymat3) <- c("imputed", "reference")
colnames(dymat3) <- dyvec
barplot(dymat3, col=c("aquamarine3", "chartreuse4"), beside=TRUE) #, legend=rownames(dymat3), args.legend="topleft")
box()
##dyvec2 <- sort(unique(xtable$disturb_year))
dycount <- as.numeric()
for (j in 1:length(dyvec))
{
  dycount[j] <- length(which(xtable$disturb_year==dyvec[j]))
}
sum(dycount)==dim(xtable)[[1]]
plot(dycount, accuracy4, xlab="Count of plots in disturbance year class", ylab="Accuracy")
dyacc2 <- cbind(dyvec, dycount, accuracy4)
write.table(dyacc2, paste(curfolder, "\\", curzone, "_disturb_year_plotcount_vs_accuracy.txt", sep=""))




