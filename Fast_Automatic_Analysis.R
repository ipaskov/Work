library(Rsamtools)
#library(RCurl)

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
  
  chrYLength = length(which(bam[[1]]$rname == "chrY"))
  chrXLength = length(which(bam[[1]]$rname == "chrX"))
  
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
  rm(bam)
  gc()
} 
dev.off() 

URL = "https://www.encodeproject.org/files/ENCFF000ZPF/@@download/ENCFF000ZPF.bam"


data = getURL(URL,ssl.verifypeer = FALSE)

