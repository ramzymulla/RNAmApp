##### VEPsorte.R #####
# The goal of this script is to sort the VEP output file
# The sorting is prioritzed based on effect of the SNP to the gene

print("##### START #####")
date()

##### Variables and data input #####

  # import the files, must be in the subdirectory _RNAidentifieR
  # file 1 must be called VEPoutput.txt, this is output by Variant Effect Predictor
  # file 2 must be called mutantWithKnownSNPs.txt, this is ouput by RNAidentifie.R
currentDir <- getwd()
subDir <- "_RNAidentifieR"
setwd(file.path(currentDir, subDir))

VEP <- read.delim("VEPoutput.txt", header=F, comment.char="#")
SNP <- read.delim("mutantWithKnownSNPs.txt", header=T)

# Consequence SO terms in order of severity
severity_order <- c("stop_gained","transcript_ablation", "splice_acceptor_variant", "splice_donor_variant", 
                        "frameshift_variant", "stop_lost", "start_lost",
                       "inframe_insertion", "inframe_deletion", "missense_variant",
                       "protein_altering_variant", "synonymous_variant", "intron_variant",
                       "upstream_gene_variant", "downstream_gene_variant", "intergenic_variant")

# setwd(file.path(currentDir))
print("data read in done")            # reading in the data takes the most time, nice to know how much
date()                                # print time after readin



##### Extraction, sorting, and output #####

  # now start to manipulate the files so we can get the info we need
names(VEP) <- c("#Uploaded_variation", "POS", "Allele", "Gene", "Feature", "Feature_type", "Consequence", "cDNA_position", "CDS_position", "Protein_position", "Amino_acids", "Codons", "Existing_variation","Extra")   #rename in .vcf format
  
  # extract the POS info from the VEP input, and turn it into a numbered list
VEP$POS.alt <- as.character(VEP$POS)
VEP$POS <- gsub(":.*-",":",VEP$POS.alt)
VEP$POS <- strsplit(VEP$POS,":")
VEP$POS <- as.numeric(lapply(1:nrow(VEP), function(i) VEP[i,2][[1]][2]))

  # merge the read info onto VEP info
both <- merge(VEP,SNP,by.x="POS",by.y="POS",all.x=TRUE)
both$MainConsequence <- factor(gsub(",.*","",both$Consequence),levels=rev(severity_order))
both <- both[order(both$MainConsequence, both$Tot.mut, both$refTot.mut,decreasing = T),]
# both <- both[order(both$MainConsequence),]

both = both[ , c("POS", "Tot.mut", names(both)[!(names(both) %in% c("POS", "Tot.mut"))])]



INDELS <- both[grep("-",both$POS.alt),]
SNPs <- both[-grep("-",both$POS.alt),]

if (nrow(INDELS)>0)
{
write.table(INDELS,file="_INDELcandidates.txt",sep='\t',quote=F,row.names = F)
}
write.table(SNPs,file="_SNPcandidates.txt",sep="\t",quote=F,row.names=F)

  # combine the stats from here with those from IdentfieR and write them to the _IdentifieR directory
  # first count the output from VEP
  # the VEP table often has multiple entries for each SNP, corresponding to multiple transcripts
  # for stats purposes, remove the duplicates and then count the number of each category
  # first STOPs

statsIn <- as.vector(read.delim("stats.txt", header=F))$V1
write(statsIn[1:9],file='stats.txt')

for (i in nrow(severity_order)) {
  so_term = severity_order[i]
  vartype <- both[grep(so_term,both$MainConsequence),]
  if (nrow(vartype) > 0) 
  {
    if (length(which(duplicated(vartype[,1]))))
    {
      vartype <- vartype[-which(duplicated(vartype[,1]))]
    }
    count_total <- nrow(vartype)
    count_1f1r <- nrow(vartype[which(vartype$Falt >=1 & vartype$Ralt >= 1),])
    stats <- c(paste("# ", so_term,sep=''),count_total,paste("# ",so_term," 1f1r alt read",sep=''),count_1f1r)
    write(stats,file='stats.txt',append=T)
  }
}

setwd(currentDir)
  # finish up
date()
print("done")
print("____________")
