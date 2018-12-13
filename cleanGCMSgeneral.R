#Read in inventory and GCMS data, reshape and filter compounds
#Revised for Shimadzu output by Diane Campbell - 20 November 2017# Further revisions made 1 August 2018. 
# Items in bold need to modification for your own computer and data set.


#####Packages#####
library(reshape2)
library("xlsx", lib.loc="~/R/win-library/3.4")

#####Load Data######

dat <- read.xlsx("communityvolatiles2018.xlsx",1) #load output from Shimadzu exported to Excel with ambient controls listed before floral samples

#Expected columns:
#sample: name of the sample
#RT: peak retention time
#Area: Peak integration 
#Name: Compound name 



#Eliminate early and late eluting samples. Change values as appropriate for your data.

dat$Name <- as.character(dat$Name)
dat$cat <- "U"
dat$cat <- ifelse(dat$RT < 4, "R", dat$cat) #Eluting early
dat$cat <- ifelse(dat$RT > 20, "R", dat$cat) #Eluting after Farnesol (19 on RMBL machine and Ipo method, use 20 on Raguso machine or 10deg method at RMBL). Modify to suit your GC method.

# Optionally, manually filter other compounds as contaminants (C)
cont <- c("Caprolactam","Decanal", "Homosalate", "Nonanal", "Oxybenzone", "Toluene", "Disulfide, dimethyl","NA")
dat$cat <- ifelse(dat$Name %in% cont, "C", dat$cat)


# Prune the data set to eliminate cases based on RT and known contaminants
data <- dat[dat$cat %in% "U",]



######Produce matrix for all data ######
alldat <- dcast(data, sample~Name, sum, value.var="Area")

rownames(alldat) <-  alldat[,1]


alldat[,1] <- NULL
chemmax2 <- sapply(alldat, max, na.rm=TRUE)
threshold <-  50000
alldat <- alldat[,chemmax2 > threshold] #Optional - drop low-abundance compounds - modify parameter of 50000 to suit

retention <- dcast(data, sample~Name, sum, value.var="RT")

#Print out retention time to assist in checking ids and doing manual filtering of volatiles after automated list is produced

write.xlsx(retention, "C:/Users/Diane16/Desktop/Community volatiles/community_2018/retention.xlsx")


####Automated compound filtering based on comparison of samples and ambient controls###


samptype <- read.xlsx("samptype18.xlsx",1) #load one column data with header "type" providing run types with 0 for ambient control and 1 for floral sample
alldata <- cbind(alldat,samptype)

write.xlsx(alldata, "C:/Users/Diane16/Desktop/Community volatiles/community_2018/alldata.xlsx")

attach(alldata)


nvol<-length(alldata)-1
nruns <- nrow(alldata)
nP <-sum(alldata$type)
nA <- nruns-nP #Provide the number of ambient controls 
starts <- nA+1 #Indicates the row where floral samples start

cmpds <-as.matrix(cbind(alldata[,1:nvol]))
cmpdnames <- (list(alldata[1,1:nvol]))


# Find mean values, determine compounds with sample means triple the ambient means, and perform t tests comparing ambient controls to floral samples
means <- apply(alldata,2,mean)
avgs <- sapply(1:nvol,function(x) (mean(alldata[starts:nruns,x])-mean(alldata[1:nA,x])))
avgd <- sapply(1:nvol,function(x) (mean(alldata[starts:nruns,x])> 3*mean(alldata[1:nA,x])))
ts <- sapply(1:nvol,function(x) (t.test((alldata[starts:nruns,x]), alldata[1:nA,x])))
Pvalue <- t(as.matrix(ts[3,]))
rval <- nruns+1

# Find counts of cases where compound is in ambient controls or is in floral samples

counting <- function(x) sum(x>1)

sumPn<-apply(alldata[starts:nruns,1:nvol],2,counting)
sumAm<-apply(alldata[1:nA,1:nvol],2,counting)

#Remove compounds seen in 0 or 1 floral sample 

alldata <- alldata[,sumPn > 1]

# Put the P values and counts into the data set

all<- rbind(cmpds,Pvalue,sumAm,sumPn,avgd)
Pval<-all[rval,]


# Method 1: Filter the data set to contain only compounds passing a P threshold based on a t test with a fixed P criterion.
# Not recommended as the final method, but it can be a useful first look screening device.

allmethod1 <- all[,Pval<0.002]


# Method 2: Filter the compounds to include those that passed the criterion in Method 1 or are not found in ambient controls but found in > 2 floral samples. 
# Modify the minimum number of >2 to suit.
allmethod2 <- all[,(sumAm==0 & sumPn>2) | Pval<0.002]

#Method 3: Filter the data set to contain compounds with no cases in ambient controls but found in > 2 floral samples (modify number of floral samples to suit). 
# Also lists an adjusted P based on applying fdr to this set of compounds.

allsc <- all[,sumAm==0 & sumPn>2]
adjP2 <- NA
allscalt <- rbind(allsc,adjP2)

adjP <- p.adjust(allsc[rval,], method="fdr")
allmethod3 <- as.data.frame(rbind(allsc,adjP))


#Method 4: FDR. Filter the data set to examine compounds found at levels at least 3x the mean in samples than ambients. 
# Then apply false discovery rate to t test results based on that set of compounds. 


all4 <- all[,avgd==TRUE]
adjP2 <- p.adjust(all4[rval,], method="fdr")
allsc3 <- (rbind(all4,adjP2))
allmethod4 <- allsc3[,adjP2<0.05]




#Method 5: Recommended final method. Filter the data set to contain compounds with at least 3x the mean in samples than ambients and apply fdr. 
# Then add compounds that were present in no ambient controls but found in at least 3 (or some other number) floral samples. Requires running methods 3 and 4 first.
# Note that all5 may list the same compound twice because it meets both criteria.


all5 <- cbind(allmethod4,allscalt)

#Write data files as desired
write.xlsx(allsc3, "C:/Users/Diane Campbell/Desktop/Community volatiles/allsc3.xlsx")
write.xlsx(all5, "C:/Users/Diane Campbell/Desktop/Community volatiles/all5.xlsx")

