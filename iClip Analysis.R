setwd("~/R Work/iClip Analysis")
library(Rsamtools)

#1:Experiment summary for ENCSR961WWI
#bam = scanBam("ENCFF081VUI.bam")
#bam = scanBam("ENCFF906CTD.bam")

#2:Experiment summary for ENCSR096IJV
#bam = scanBam("ENCFF002DOB.bam")
#bam = scanBam("ENCFF002DOC.bam")

#3:Experiment summary for ENCSR193PVE
#bam = scanBam("ENCFF002DNZ.bam")
#bam = scanBam("ENCFF002DOA.bam")


#4:Experiment summary for ENCSR427DED
#bam = scanBam("ENCFF002DNX.bam")
#bam = scanBam("ENCFF002DNY.bam")

#5:Experiment summary for ENCSR441YTO
#bam = scanBam("ENCFF002DNV.bam")
#bam = scanBam("ENCFF002DNW.bam")
#-------------------------------------------------
#End of iClip Experiments
#-------------------------------------------------
bam = scanBam("ENCFF000ZUZ.bam")


#-------------------------------------------------------
#http://davetang.org/muse/2013/09/07/creating-a-coverage-plot-in-r/
#function for collapsing the list of lists into a single list
#as per the Rsamtools vignette
.unlist <- function (x){
  ## do.call(c, ...) coerces factor to integer, which is undesired
  x1 <- x[[1L]]
  if (is.factor(x1)){
    structure(unlist(x), class = "factor", levels = levels(x1))
  } else {
    do.call(c, x)
  }
}

#store names of BAM fields
bam_field <- names(bam[[1]])

#go through each BAM field and unlist
list <- lapply(bam_field, function(y) .unlist(lapply(bam, "[[", y)))

#store as data frame
bam_df <- do.call("DataFrame", list)
names(bam_df) <- bam_field

#-------------------------------------------------------------------------
#Done Converting BAM File into Data File
#-------------------------------------------------------------------------

chrYLength = length(which(bam_df$rname == "chrY"))
chrXLength = length(which(bam_df$rname == "chrX"))

chrYLabel = paste("chrY: ", chrYLength)
chrXLabel = paste("chrX: ", chrXLength)

counts = table(c(rep(chrYLabel,chrYLength),
                 rep(chrXLabel,chrXLength)
)
)

barplot(counts, main="Gender Classification", 
        xlab="Chromosome", col = c('red','blue'))

print(length(which(bam_df$rname == "chrY")))
print(length(which(bam_df$rname == "chrX")))


