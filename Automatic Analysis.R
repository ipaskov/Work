library(Rsamtools)

setwd("~/R Work/iClip Analysis/ChIPseq/")
somePDFPath = "/Users/ivan/R Work/iClip Analysis/ChIPseq/plots.pdf"
pdf(file=somePDFPath)  


meta_data = read.csv("metadata_short.csv")
files = dir(pattern = "*.bam")


numFiles = length(files)

for (n in 1:numFiles)   
{ 
  print(n)
  bam = scanBam(files[n])
  
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
  print(n)
  
  #store as data frame
  bam_df <- do.call("DataFrame", list)
  names(bam_df) <- bam_field
  print(n)
  
  chrYLength = length(which(bam_df$rname == "chrY"))
  chrXLength = length(which(bam_df$rname == "chrX"))
  
  chrYLabel = paste("chrY: ", chrYLength)
  chrXLabel = paste("chrX: ", chrXLength)
  
  counts = table(c(rep(chrYLabel,chrYLength),
                   rep(chrXLabel,chrXLength)
  )
  )
  
  name = strsplit(files[n], split = ".bam")[[1]]
  ind = which(meta_data[,1] == name)
  gender = as.character(meta_data[ind,"Biosample.sex"])
  #title = paste("Gender Classification: ", gender)
  title = paste(name, ":", gender)
  
  barplot(counts, main=title, xlab="Chromosome", col = c('red','blue'))
} 
dev.off() 

