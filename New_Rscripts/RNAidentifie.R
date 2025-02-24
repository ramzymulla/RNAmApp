##### RNAidentifie.R #####
# The overall goal of this script is to extract ALL SNP/INDEL information from mutants within a defined region of the genome
# The region will most likely be that defined by RNAmappe.R as being linked
# Th mutant SNPs are then filtered against known widltype SNPs to identify potentially deleterious changes
# The general steps are:
  # STEP1 - Extract ALL the SNP information from the region of linkage/interest
    # this will be a "dirty" list that needs to be filtered, but also want to retain all possible info for user to evaluate
    # vcf formatted file corresponding to the chromsome or interest is imported, must have DP4=... or I16=... in INFO column
    # importing SNPs from chrX named mut_chrX_allALT.vcf and wt_chrX_allALT.vcf from RNAmappe.R
    # and extracting information between positions Y and Z
    # X, Y, and Z are defined either by user or will be automated within pipeline
  # STEP2 - Identify INDELs within the region and filter against known INDELs
    # --> output  mutant INDEL list
  # STEP3 - Filter mutant SNPs against known wildtype SNPs
    # single vcf formatted file of SNPs to filter against is imported, must have DP4=... or I16=... in INFO column
    # ideally this is a vcf created from VCFmerge.R - which concatenates many vcfs together and outputs a more confident single list
    # --> output filtered mutant SNP list in vcf format allowing for piping into Variant Effect Predictor
    # --> output UNfiltered mutant SNP with wildtype SNP appended on - allows user to evaluate directly


print("##### START #####")
date()

##### Variables and data input #####

  # chromsome, left and right edges of linkage are used to import and extract regions of interest
  # users/pipeline must create file identifieRsettings.txt in _settings dirtory within current directory
  # three lines 1) chromsome=X 2) left edge of linkage=Y 3) right edge of linkage=Z
currentDir <- getwd()
subDir <- "_settings"
setwd(file.path(currentDir, subDir))
linkedsettings <- read.delim("identifieRsettings.txt", header=F)
setwd(file.path(currentDir))
chr <- as.character(linkedsettings[1,1])
chr <- strsplit(chr,"=")
chr <- as.numeric(chr[[1]][2])
Ledge <- as.character(linkedsettings[2,1])
Ledge <- strsplit(Ledge,"=")
Ledge <- as.numeric(Ledge[[1]][2])
Redge <- as.character(linkedsettings[3,1])
Redge <- strsplit(Redge,"=")
Redge <- as.numeric(Redge[[1]][2])
inpre <- as.character(linkedsettings[4,1])
inpre <- trimws(strsplit(inpre,"=")[[1]][2])
inpre <- paste(inpre,"_",sep="")
SNPfile <- as.character(linkedsettings[5,1])
SNPfile <- trimws(strsplit(SNPfile,"=")[[1]][2])


wtInput <- paste(inpre,"wt_chr",chr,"_allALT.vcf", sep="")
mutInput <- paste(inpre,"mut_chr",chr,"_allALT.vcf", sep="")
# wtSNPinput <- paste(inpre,"SNPs_chr",chr,".vcf", sep="")
wtSNPinput <- SNPfile

  # output for user to debug if necessary
print("wt sibling file")
print(wtInput)
print("mut file")
print(mutInput)
print("wt SNP file")
print(wtSNPinput)
print("left edge of linkage")
print(Ledge)
print("right edge of linkage")
print(Redge)

  # data in
wtSNP <- read.delim(wtInput, header=T)
mutSNP <- read.delim(mutInput, header=T)
SNP <- read.delim(wtSNPinput, header=T)

print("data readin done")

##### Functions #####
# to extract data about counts the INFO column of the .vcf file must be manipulated
# the readInfo funciton below pulls out the useful data from the inputted vcf formatted table

readInfo <- function(SNPtable)
{
    # vcf table INFO columns have varied structures, but we need the forward/reverse reads of reference and alternative
    # these are noted within the INFO column as either DP4=a,b,c,d OR I16=a,b,c,d,... Fref=a, Rref=b, Falt=c, Ralt=d
    # so we first need to decide which tag was used to note a,b,c,d 
    # and then use this to use to delimit the left side of INFO and then grab the next 4 numbers, which are a,b,c,d
  tag1 <- "DP4="
  tag2 <- "I16="
  if(length(grep(tag1,SNPtable[1,8]) != 0))
  {
    identifier <- tag1
  }
  if(length(grep(tag2,SNPtable[1,8]) != 0))
  {
    identifier <- tag2
  }
    # now pull out a,b,c,d which are found directly after the identifier
  numrows <- nrow(SNPtable)
  info <-strsplit(as.character(SNPtable$INFO),identifier)
  info <- lapply(1:numrows, function(i) strsplit(as.character(info[[i]][2]),","))
  SNPtable$Fref <- as.numeric(lapply(1:numrows, function(i) info[[i]][[1]][[1]][1]))
  SNPtable$Rref <- as.numeric(lapply(1:numrows, function(i) info[[i]][[1]][[2]][1]))
  SNPtable$Falt <- as.numeric(lapply(1:numrows, function(i) info[[i]][[1]][[3]][1]))
  SNPtable$Ralt <- as.numeric(lapply(1:numrows, function(i) info[[i]][[1]][[4]][1]))
    # do math on counts
  SNPtable$refTot <- SNPtable$Fref + SNPtable$Rref                # count ref total
  SNPtable$altTot <- SNPtable$Falt + SNPtable$Ralt                # count alt total
  SNPtable$Tot <- SNPtable$refTot + SNPtable$altTot               # count total
  SNPtable$refRat <- SNPtable$refTot / SNPtable$Tot               # ref frequency
  SNPtable$altRat <- SNPtable$altTot / SNPtable$Tot               # alt frequency
  Rat <- SNPtable[,1:2]
  Rat$ref <- SNPtable$refRat
  Rat$alt <- SNPtable$altRat
  SNPtable$highAllele<-round((apply(Rat[3:4],1,max)),2)  # highest frequency
  return(SNPtable)  # outputs the SNPtable that was modified above
}



##### STEP1 - Extract ALL the SNP information from the region of linkage/interest #####

  # make sure column names are nicely formatted in .vcf format
names(wtSNP) <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "AB")
names(mutSNP) <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "AB")
names(SNP) <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO")
  # make columns more useable
wtSNP$POS <- as.numeric(as.character(wtSNP$POS))
wtLink <- wtSNP[wtSNP$POS >= Ledge,]
wtLink <- wtLink[wtLink$POS <= Redge,]
mutSNP$POS <- as.numeric(as.character(mutSNP$POS))
mutLink <- mutSNP[mutSNP$POS >= Ledge,]
mutLink <- mutLink[mutLink$POS <= Redge,]
SNP$POS <- as.numeric(as.character(SNP$POS))
SNPLink <- SNP[SNP$POS >= Ledge,]
SNPLink <- SNPLink[SNPLink$POS <= Redge,]



##### STEP2 - Identify INDELs within the region and filter against knowns #####

  # find the INDELs, remove from SNP list if they exist, and save for later
mutINDEL <- mutLink[grep("INDEL",as.character(mutLink$INFO)),]
  # some vcfs don't contain INDELs so check to see if INDELs exist, and if they do remove and save them
if (!is.na(mutINDEL[1,1]))
{ 
  mutLink <- mutLink[-grep("INDEL",as.character(mutLink$INFO)),] # remove INDELs
}
  # Now remove "SNPs" within 10bp of INDELs, likely false
  # first check for INDELs
if (!is.na(mutINDEL[1,1]))
{
  for (i in 1:nrow(mutINDEL))
  {
    if (length(which(mutINDEL[i,2]-10 <= mutLink[,2] & mutINDEL[i,2]+10 >= mutLink[,2])))
    { 
      mutSNPNearINDEL <- mutLink[which(mutINDEL[i,2]-10 <= mutLink[,2] & mutINDEL[i,2]+10 >= mutLink[,2]),]
      mutLink <- mutLink[-which(mutINDEL[i,2]-10 <= mutLink[,2] & mutINDEL[i,2]+10 >= mutLink[,2]),]
      mutINDEL <- rbind(mutINDEL,mutSNPNearINDEL)
      mutINDEL <- mutINDEL[order(mutINDEL$POS),]
    }
  }
}

  # find the wt INDELs, remove from SNP list if they exist, and save for later
wtINDEL <- wtLink[grep("INDEL",as.character(wtLink$INFO)),]
  # some vcfs don't contain INDELs so check to see if INDELs exist, and if they do remove and save them
if (!is.na(wtINDEL[1,1]))
{ 
  wtLink <- wtLink[-grep("INDEL",as.character(wtLink$INFO)),] # remove INDELs
}
  # Now remove "SNPs" within 10bp of INDELs, likely false
  # first check for INDELs
if (!is.na(wtINDEL[1,1]))
{
  for(i in 1:nrow(wtINDEL))
  {
    if (length(which(wtINDEL[i,2]-10 <= wtLink[,2] & wtINDEL[i,2]+10 >= wtLink[,2])))
    { 
      wtSNPNearINDEL <- wtLink[which(wtINDEL[i,2]-10 <= wtLink[,2] & wtINDEL[i,2]+10 >= wtLink[,2]),]
      wtLink <- wtLink[-which(wtINDEL[i,2]-10 <= wtLink[,2] & wtINDEL[i,2]+10 >= wtLink[,2]),]
    }
  }
}

  # now put the mutINDEL with the SNPINDEL and filter
mutINDEL2 <- merge(mutINDEL,SNPLink,by.x="POS",by.y="POS",all.x=TRUE)
mutINDEL3 <- mutINDEL2
mutINDEL4 <- mutINDEL

  ##### FILTER now remove positions with the same call in both ALT fields
if (length(which(as.character(mutINDEL2$ALT.x) == as.character(mutINDEL2$ALT.y))) > 0)
{
  mutINDEL3 <- mutINDEL2[-which(as.character(mutINDEL2$ALT.x) == as.character(mutINDEL2$ALT.y)),]
  mutINDEL4 <- mutINDEL[-which(as.character(mutINDEL$ALT)==as.character(SNPLink$ALT)),]
} 

NameOut <- "INDELwithSNPs.txt"
  # write the table to the _IdentifieR directory
currentDir <- getwd()
subDir <- "_RNAidentifieRnew"
dir.create(file.path(currentDir, subDir), showWarnings = FALSE)
setwd(file.path(currentDir, subDir))
write.table(mutINDEL3,file=NameOut,sep="\t",quote=F,row.names=F)
setwd(file.path(currentDir))




##### STEP3 - Filter mutant SNPs against known wildtype SNPs #####

  # extract the read info at each site
mutLink <- readInfo(rbind(mutLink,mutINDEL4))
wtLink <- readInfo(rbind(wtLink,wtINDEL))
#SNPLink <- readInfo(SNPLink)
  # make ALT column characters
mutLink$ALT <- as.character(mutLink$ALT)
wtLink$ALT <- as.character(wtLink$ALT)
SNPLink$ALT <- as.character(SNPLink$ALT)

  ##### FILTER first conservatively remove any mutSNPs called that are not approaching homozygosity
mutLink <- mutLink[mutLink$altRat >= 0.5,]  # if there are 2 calls, 1 IS the mutant the other is error, this still gets it
  # stats
mutLinkStats <- nrow(mutLink)
mutLinkStats2x <- nrow(mutLink[which(mutLink$Falt >=1 & mutLink$Ralt >= 1),])
  # positions with multiple ALT calls (e.g. C,T) are annoying b/c all counts lumped into ALT
  # so if 5C and 5T, altTot=10, and there's no way to get back to individual calls using mpileup data
  # so have to strip it out now and save it for the user, otherwise the below FILTER will remove them inappropriately
mutLinkMult <- mutLink[mutLink$ALT != "<*>",]
if (length(mutLinkMult$POS) > 0)
{ 
  mutLink <- mutLink[-which(mutLink$ALT != "<*>"),]
}
  # also true of the input SNP list, so lets pull them out and keep them 'til the end
SNPLinkMult <- SNPLink[which(SNPLink$ALT != "<*>"),]
if (length(SNPLinkMult$POS) > 0)
{ 
  SNPLink <- SNPLink[-which(SNPLink$ALT != "<*>"),]
}

  # now look at known SNPs, first place to look is in WT sibling file
wtLink <- wtLink[wtLink$POS %in% mutLink$POS,]
  # if the wt sibs are homozygous for a non-ref allele we can remove it from further consideration
wtLink <- wtLink[wtLink$altRat >= 0.95,]
  # being conservative for removal, only believe its real if there are 5 calls (so 1 in 32 chance wrong (2^5))
wtLink <- wtLink[wtLink$Tot >= 5,]
  ##### FILTER now remove these sites from mut 
if(length(mutLink$POS %in% wtLink$POS) >= 1)
{ 
  mutLink <- mutLink[-which(mutLink$POS %in% wtLink$POS),]
}

  # now compare to SNPs from known wildtypes outside of experiment
  # ideally this file was created with VCFmerge.R and input above
  # VCFmerge.R creates a "trusted" list of SNPs
  # so below ANY POSITION that matches between mutant and SNP IS REMOVED!!!
  # so the SNP info MUST BE GOOD
  # first merge the tables together for easy compariston
mutLink2 <- merge(mutLink,SNPLink,by.x="POS",by.y="POS",all.x=TRUE)
mutLink3 <- mutLink2
  ##### FILTER now remove positions with the same call in both ALT fields
if (length(which(mutLink2$ALT.x == mutLink2$ALT.y)) > 0)
{
  mutLink3 <- mutLink2[-which(mutLink2$ALT.x == mutLink2$ALT.y),]
} 
  # stats
mutLinkFilteredStats <- nrow(mutLink3)
mutLinkFilteredStats2x <- nrow(mutLink3[which(mutLink3$Falt >=1 & mutLink3$Ralt >= 1),])

  # get back to vcf format
finalTable <- data.frame(mutLink3[2],mutLink3[1],mutLink3[3:10], check.names=F) 
names(finalTable) <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "AB")
  # and readd the positions with multiple calls in ALT that were saved above
mutLinkMult2 <- mutLinkMult[,1:10]
finalTable <- rbind(finalTable,mutLinkMult2)
finalTable <- finalTable[order(finalTable$POS),]
  # write the table to the _IdentifieR directory
subDir <- "_RNAidentifieRnew"
# dir.create(file.path(currentDir, subDir), showWarnings = FALSE)
setwd(file.path(currentDir, subDir))
write.table(finalTable,file="VEPinput.vcf",sep="\t",quote=F,row.names=F)
# setwd(file.path(currentDir))

  # rather than just outputting the filtered table
  # also output a table that has ALL the mutant and SNP data, without filtering, so the user can make decisions directly
  # also this table is 1 of the inputs for VEPsorte.R
  # include the ALT multiple calls and the extra math I've generated above
mutLinkMult3 <- merge(mutLinkMult,SNPLinkMult,by.x="POS",by.y="POS",all.x=TRUE)
finalTable2 <- rbind(mutLink2,mutLinkMult3)
finalTable2 <- finalTable2[order(finalTable2$POS),]
finalTable2 <- data.frame(finalTable2[2],finalTable2[1],finalTable2[3:(ncol(finalTable2))], check.names=F)
names(finalTable2)[1:27] <- c("#CHROM", "POS", "ID.mut", "REF.mut", "ALT.mut", "QUAL.mut", "FILTER.mut", "INFO.mut", "FORMAT.mut", "AB.mut","Fref.mut", "Rref.mut", "Falt.mut", "Ralt.mut", "refTot.mut", "altTot.mut", "Tot.mut", "refRat.mut", "altRat.mut", "highAllele.mut", "#CHROM", "ID.SNP", "REF.SNP", "ALT.SNP", "QUAL.SNP", "FILTER.SNP", "INFO.SNP")
  # make a nice name to output
NameOut <- "mutantWithKnownSNPs.txt"
  # write the table to the _IdentifieR directory
# setwd(file.path(currentDir, subDir))
write.table(finalTable2,file=NameOut,sep="\t",quote=F,row.names=F)

  # write the stats to the _IdentifieR directory
stats<-c("# linked mut SNPs",mutLinkStats,"# linked mut SNPs 1F1R alt read",mutLinkStats2x,"# linked mut SNPs after filtering",mutLinkFilteredStats,"# linked mut SNPs after filtering 1F1R alt read",mutLinkFilteredStats2x)
NameOut <- "stats.txt"
write.table(stats,file=NameOut,sep="\t",quote=F,row.names=F)

# finish up
date()
print("done")
print("____________")
