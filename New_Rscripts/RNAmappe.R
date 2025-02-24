##### RNAmappe.R #####
# The overall goal of this script is to identify wildtype SNPs and use this info to plot the frequency of the SNP in mutants
# This allows for the identification of regions of linkage to the mutation
# The general steps are:
  # STEP1 - ID SNPs in .vcf files - I've found that variant callers such as bcftools -v throw away many RNA-Seq SNPs
    # .vcf files MUST be v4.0 or v4.1 and only for indivdual chromsomes labeled wt_chr[1-25] and mut_chr[1-25]
    # a lynix for loop is used to input each chromosome sequentially into this script
    # --> output ALL SNPs and INDELs identified in widltype (wt) and mutant (mut) for user access
  # STEP2 - count allele frequency - this data is buried in the INFO column of the .vcf file and must be extracted
  # STEP3 - find high quality SNPs (markers) in wt useful for mapping
  # STEP4 - count the allele frequency of SNPs in mut at the marker positions
  # STEP5 - graph the data to identify regions of linkage in the mutant
    # --> output mutant SNP frequency at markers across ENTIRE genome
    # --> output mutant SNP frequency at markers on individual chromsomes


print("##### START #####")
date()

##### Variables and data input #####

# coverage and zygosity are used to identify high quality markers (step 3)
  # users must create file mappeRsettings.txt in _settings dirtory within current directory
  # two lines 1) coverage=X 2) zygosity=Y
  # these numbers are used to define a "good" mapping SNP
  # I've found 25x coverage and 25% heterozygosity works well and will be the suggested default (coverage=25 \n zygosity=25)
currentDir <- getwd()
subDir <- "_settings"
setwd(file.path(currentDir, subDir))
SNPsettings <- read.delim("mappeRsettings.txt", header=F)
setwd(file.path(currentDir))
coverage <- as.character(SNPsettings[1,1])
coverage <- strsplit(coverage,"=")
coverage <- as.numeric(coverage[[1]][2])
zygosity <- as.character(SNPsettings[2,1])
zygosity <- strsplit(zygosity,"=")
zygosity <- as.numeric(zygosity[[1]][2])

  # for mapping the neighboring SNPs allele frequencies are averaged
  # currently the window is set at 50 neighboring points - in the future I may want this to be user inputtable as above
neighbors <- 50
  # for defining regions of linkage generally the linkage for mutants goes to homozygous, or allele frequency = 1.0
  # so in practice simply using a frequency of > 0.98 works conservatively captures the linked region
linkedRatio <- 0.98
  # HOWEVER, some peaks do NOT reach 1.0 (e.g. Krox20/egr2b) and so would be missed by the simple version above
  # this matters for piping data and automating the next step (RNAidentifie.R)
  # currently my method wants the user to STOP after RNAmappe.R and look, then input the region they think is linked into RNAidentifie.R
  # instead we could come up with an algorithm within RNAmappe.R to ID the biggest peak and automatically pipe it
  # for now, I'm only using the simple version of > 0.98 which works with the current test data set just fine
  # so the whole workflow can be automated for the test dataset with the current simple version

  # SNP calling is done on each chromsome individually - this is computationally easier than reading in the whole genome
  # lynix for loop passes individual chromsomes into RNAmappe.R script
  # lynix for loop = for i in {1..25}; do Rscript --vanilla /path/to/RNAmappe.R wt_chr$i.vcf,mut_chr$i.vcf; done
  # below identifies the names of the chromosome files input to the Rscript
args <- commandArgs(T)
inputFile <- args[1]
inputSplit <- strsplit(inputFile, ",")
wtInput <- inputSplit[[1]][1]
mutInput <- inputSplit[[1]][2]
outpre <- ""

  # output for user to debug if necessary
print("wt file")
print(wtInput)
print("mut file")
print(mutInput)
print("coverage for accepting SNP")
print(coverage)
print("zygosity for accepting SNP")
print(zygosity)

  # readin individual chrosome .vcfs
wt <- read.delim(wtInput, header=F)
mut <- read.delim(mutInput, header=F)

  # debug
print("data read in done")
date()


##### Functions #####
# the most important point of this overall procedure is to count alleles called at SNPs, this feeds into steps 2-5
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



##### STEP1 - ID SNPs in .vcf files #####

  # make sure column names are nicely formatted in .vcf format
names(wt) <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "AB")
names(mut) <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "AB")
  # IDENTIFY SNPs - these are easily recognizable as they are represented in the ALT column as N,X or N,N,X, etc
  # ALSO keep INDELs if present
wtSNP <- wt[wt$ALT != "<*>",]
mutSNP <- mut[mut$ALT != "<*>",]
  # extra ",X" in ALT column generated by samtools is annoying - remove it
wtSNP$ALT <- as.character(wtSNP$ALT)
mutSNP$ALT <- as.character(mutSNP$ALT)
wtSNP$ALT <- gsub(",X","",wtSNP$ALT)
mutSNP$ALT <- gsub(",X","",mutSNP$ALT)
  # output ALL SNPs identified on the chrosome
  # first make a nice name to output
wtName <- gsub(".vcf","",basename(wtInput))
mutName <- gsub(".vcf","",basename(mutInput))
outAdd <- "_allALT.vcf"
wtNameOut <- paste(outpre,wtName,outAdd, sep='')
mutNameOut <- paste(outpre,mutName,outAdd,sep='')
  # write the tables
write.table(wtSNP,file=wtNameOut,sep="\t",quote=F,row.names=F)
write.table(mutSNP,file=mutNameOut,sep="\t",quote=F,row.names=F)



### STEP2 - count allele frequency in wildtype

  # first we need to cleanup the SNP files a bit before we can start to count the calls
  # remove leftover header from import
wtSNP <- wtSNP[2:nrow(wtSNP),]
mutSNP <- mutSNP[2:nrow(mutSNP),]
  # make POS column a number
wtSNP$POS <- as.numeric(as.character(wtSNP$POS)) 
mutSNP$POS <- as.numeric(as.character(mutSNP$POS))
  # mapping is poor many near INDELs causing false "SNPs" - user can look at INDELs and these "SNPs" in outputted allALT.vcf file above
  # retain info on INDELs and nearby (within 10bp) "SNPs" for user, but remove for mapping
wtINDEL <- wtSNP[grep("INDEL",as.character(wtSNP$INFO),ignore.case = T),]
  # some vcfs don't contain INDELs so check to see if INDELs exist, and if they do remove and save them
  # only need to do wtSNP file because we'll use wtSNPs to find positions in mutSNP, which will automatically "clean" it of INDELs
if(!is.na(wtINDEL[1,1]))
{ 
  wtSNP <- wtSNP[-grep("INDEL",as.character(wtSNP$INFO),ignore.case = T),] # remove INDELs
}
  # Now remove "SNPs" within 10bp of INDELs, likely false
  # first check for INDELs
if(nrow(wtINDEL) > 0)
{
  for(i in 1:nrow(wtINDEL))
  {
    if (length(which(wtINDEL[i,2]-10 <= wtSNP[,2] & wtINDEL[i,2]+10 >= wtSNP[,2])))
    { 
      wtSNP <- wtSNP[-which(wtINDEL[i,2]-10 <= wtSNP[,2] & wtINDEL[i,2]+10 >= wtSNP[,2]),]
    }
  }
}

  # Now extract information on allele count calls using the readInfo function
  # readInfo function also calculates totals, frequencies, etc, and appends them onto the table as new columns
wtSNP <- readInfo(wtSNP)
mutSNP <- readInfo(mutSNP)
  # Count entries for stats
statsWtSNPs <- nrow(wtSNP)
statsWtSNPs2x <- nrow(wtSNP[which(wtSNP$Falt >=1 & wtSNP$Ralt >= 1),])
statsMutSNPs <- nrow(mutSNP)
statsMutSNPs2x <- nrow(mutSNP[which(mutSNP$Falt >=1 & mutSNP$Ralt >= 1),])



#####STEP3 - find high quality SNPs (markers) in wt useful for mapping

wtMarker <- wtSNP[wtSNP$Tot >= coverage,]
  # calculate the ratio to use as zygosity filter
highRat <- 1-(zygosity / 100)
lowRat <- (zygosity / 100)
# good SNPs will have a high % of heterozygosity in the calls - currently using 20% because it works cleanly
wtMarker <- wtMarker[wtMarker$refRat <= highRat,]
wtMarker <- wtMarker[wtMarker$refRat >= lowRat,]
  # ANNOYING - if the calls are 2 nonRef (ref=T, calls=A,C) then .vcf file does not retain separate info on the ALT calls
  # so can't be used here with this data because can't count, have to remove calls with no ref
if(length(which(wtMarker$refTot == 0)) != 0)
{
  wtMarker <- wtMarker[-which(wtMarker$refTot == 0),]
}
  # stats
statsWtMarkers<-nrow(wtMarker)

  # now take the sliding average of the highest allele called at any position
  # currently using 50 neighboring points (assigned in variables at top) step size of 1 (at each position take the average)
  # this sliding window smooths the data, here in wt just to give an idea of average allele frequency across chromsome
numrows <- nrow(wtMarker)
  # first take average of 50 neighboring points "to the right"
  # want to take centered average but at the edges R can't handle "empty space"
wtMarker$highAve <- as.numeric(lapply(1:numrows, function(i) (sum(wtMarker[i:(i + (neighbors-1)),20])) / neighbors))
  # sliding average centered at position
wtMarker[(neighbors / 2):numrows,21] <- as.numeric(lapply((neighbors / 2):numrows, function(i) (sum(wtMarker[((i - ((neighbors/2) - 1)):(i + (neighbors / 2))),20])) / neighbors))
  # above leaves NAs at end, below averages "to the left" so end is filled in
wtMarker[(numrows - (neighbors / 2)):numrows,21]<-as.numeric(lapply((numrows - (neighbors / 2)):numrows, function(i) (sum(wtMarker[(i - (neighbors - 1)):i,20])) / neighbors))
  # alltogether the above averaging results in the first 25 points being averaged "to the right"
  # points 26 to numrows-25 being averaged "at the center"
  # the last 25 points being averaged "to the left"



##### STEP4 - count the allele frequency of SNPs in mut at the marker positions

  # now find the positions corresponding to the wtMarkers in the mutSNP data
mutMarker <- mutSNP[mutSNP$POS %in% wtMarker$POS,]
  # sometimes INDELs are found at these positions, remove them if they exist
if (length(grep("INDEL", as.character(mutMarker$INFO),ignore.case = T)))
{ 
  mutMarker<-mutMarker[-grep("INDEL",as.character(mutMarker$INFO),ignore.case = T),]
}
  # stats
statsMutMarkers<-nrow(mutMarker)
  # averaging as annotated above for wildtype at the end of STEP3
numrows <- nrow(mutMarker)
mutMarker$highAve <- as.numeric(lapply(1:numrows, function(i) (sum(mutMarker[i:(i + (neighbors-1)),20])) / neighbors))
mutMarker[(neighbors / 2):numrows,21] <- as.numeric(lapply((neighbors / 2):numrows, function(i) (sum(mutMarker[((i - ((neighbors/2) - 1)):(i + (neighbors / 2))),20])) / neighbors))
mutMarker[(numrows - (neighbors / 2)):numrows,21]<-as.numeric(lapply((numrows - (neighbors / 2)):numrows, function(i) (sum(mutMarker[(i - (neighbors - 1)):i,20])) / neighbors))



##### STEP5 - graph the data to identify regions of linkage in the mutant

  # the regions of linkage is currently simply defined by linkedRatio set to 0.98
  # see comments at the top about linkedRatio
mutMarker2 <- mutMarker[mutMarker$highAve >= linkedRatio,]
  # get the position of the edges
Ledge <- 0
Ledge <- as.numeric(as.character(mutMarker2[1,2]))
Redge <- 0
Redge <- as.numeric(as.character(mutMarker2[nrow(mutMarker2),2]))
edges <- paste(Ledge,Redge, sep="--")
linkSize <- 0
linkSize <- Redge-Ledge

  # now plot a graph of the mut allele frequency, both "raw" and averaged at each wtMarker position
  # first make a name and setup the table for the graph
outAdd <- ".jpg"
plotOut <- paste(outpre,mutName,outAdd, sep="")
mutMarker$POS <- as.numeric(as.character(mutMarker$POS))
  # and plot the graph
jpeg(plotOut, width=1000, bg="white")
plot(mutMarker$POS,mutMarker$highAve, pch=16, col="red", cex=2, ylim=c(0.5,1)) # plot averaged frequency
points(mutMarker$POS,mutMarker$highAllele, col="black")         # plot raw frequency
abline(v = Ledge, col = "blue")                                 # vertical line at Ledge of homozygosity
abline(v = Redge, col = "blue")                                 #  vertical line at Redge of homozygosity
abline(h = 1, col = "black", lty = 3)                           # dashed line at freq=1.0, i.e. homozygosity
mtext(edges, 1, col = "blue")                                   # write position of above lines
dev.off()

  # now output the data used to make the plot so the user can graph how the like
  # first a name
outAdd <- "_atMarkers.txt"
tableOut <- paste(outpre,mutName,outAdd, sep="")
  # write the table
write.table(mutMarker,file=tableOut,sep="\t",quote=F,row.names=F)

  # now write the stats
stats<-c("# wt SNPs",statsWtSNPs,"# wt SNPs 1F1R alt read",statsWtSNPs2x,"# mut SNPs",statsMutSNPs,"# mut SNPs 1F1R alt read",statsMutSNPs2x,"# wt markers",statsWtMarkers,"# corresponding mut markers",statsMutMarkers,"left edge of linkage",Ledge,"right edge of linkage",Redge,"size of linkage",linkSize)
outAdd <- "_stats.txt"
statsOut <- paste(outpre,mutName,outAdd, sep="")
write.table(stats,file=statsOut,sep="\t",quote=F,row.names=F)

  # finally, if we have gone through all the chromsomes lets output a graph of the linkage for the whole genome
if(length(grep("chr25",mutName)) == 1)
{
    # intialize a list to hold the input
  all <- list();
  allStats <- list();
  for(i in 1:25)
  {
      # read in tables created that contain the mut allele freqs at the markers for each chromsome
    chr <- paste("chr",i,sep="")
    name <- paste("mut_",chr,"_atMarkers.txt",sep="")
    graph <- read.delim(name, header=T)
    all[[i]] <- graph[,c(2,21)]
      # read in all the stat tables from each chromsome
    name <- paste("mut_",chr,"_stats.txt",sep="")
    statChr <- read.delim(name, header=T)
    allStats[[i]] <- statChr
  }
    ##### put all the mutant zygosity data together into one big graph laid out by chromosome
  jpeg("_RNAgenomeFrequency.jpg", width=2500, bg="white")   # setup graph environment
  par(mfrow=c(1,25), mar=c(5,0.5,0,0), cex.lab=4, col.axis="white")   # put all 25 graphs into one graphspace  
  plot(all[[1]][,1], all[[1]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[1]][,1])), type="p", xlab="1")
  plot(all[[2]][,1], all[[2]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[2]][,1])), type="p", xlab="2")   
  plot(all[[3]][,1], all[[3]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[3]][,1])), type="p", xlab="3")   
  plot(all[[4]][,1], all[[4]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[4]][,1])), type="p", xlab="4")   
  plot(all[[5]][,1], all[[5]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[5]][,1])), type="p", xlab="5")   
  plot(all[[6]][,1], all[[6]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[6]][,1])), type="p", xlab="6")   
  plot(all[[7]][,1], all[[7]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[7]][,1])), type="p", xlab="7")   
  plot(all[[8]][,1], all[[8]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[8]][,1])), type="p", xlab="8")   
  plot(all[[9]][,1], all[[9]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[9]][,1])), type="p", xlab="9")   
  plot(all[[10]][,1], all[[10]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[10]][,1])), type="p", xlab="10")
  plot(all[[11]][,1], all[[11]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[11]][,1])), type="p", xlab="11")
  plot(all[[12]][,1], all[[12]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[12]][,1])), type="p", xlab="12")
  plot(all[[13]][,1], all[[13]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[13]][,1])), type="p", xlab="13")
  plot(all[[14]][,1], all[[14]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[14]][,1])), type="p", xlab="14")
  plot(all[[15]][,1], all[[15]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[15]][,1])), type="p", xlab="15")
  plot(all[[16]][,1], all[[16]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[16]][,1])), type="p", xlab="16")
  plot(all[[17]][,1], all[[17]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[17]][,1])), type="p", xlab="17")
  plot(all[[18]][,1], all[[18]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[18]][,1])), type="p", xlab="18")
  plot(all[[19]][,1], all[[19]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[19]][,1])), type="p", xlab="19")
  plot(all[[20]][,1], all[[20]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[20]][,1])), type="p", xlab="20")
  plot(all[[21]][,1], all[[21]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[21]][,1])), type="p", xlab="21")
  plot(all[[22]][,1], all[[22]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[22]][,1])), type="p", xlab="22")
  plot(all[[23]][,1], all[[23]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[23]][,1])), type="p", xlab="23")
  plot(all[[24]][,1], all[[24]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[24]][,1])), type="p", xlab="24")
  plot(all[[25]][,1], all[[25]][,2], ylim=c(0.5,1), xlim=c(0,max(all[[25]][,1])), type="p", xlab="25")
  dev.off()   # turn graph off

    # now deal with the stats, have to extract each stat out of the subtables within allStats and then write
  statsAllChr <- data.frame(allStats[[1]][,1],allStats[[2]][,1],allStats[[3]][,1],allStats[[4]][,1],allStats[[5]][,1],allStats[[6]][,1],allStats[[7]][,1],allStats[[8]][,1],allStats[[9]][,1],allStats[[10]][,1],allStats[[11]][,1],allStats[[12]][,1],allStats[[13]][,1],allStats[[14]][,1],allStats[[15]][,1],allStats[[16]][,1],allStats[[17]][,1],allStats[[18]][,1],allStats[[19]][,1],allStats[[20]][,1],allStats[[21]][,1],allStats[[22]][,1],allStats[[23]][,1],allStats[[24]][,1],allStats[[25]][,1])
  names(statsAllChr) <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chr23","chr24","chr25")
  write.table(statsAllChr,file="_RNAgenomeStats.txt",sep="\t",quote=F,row.names=F)
}

  # finish up
date()
print("done")
print("____________")
