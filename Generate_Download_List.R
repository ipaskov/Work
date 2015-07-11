setwd("/Volumes/Ivan Paskov/Work")

meta_data = read.csv("metadata_short.csv")
types = as.vector(unique(meta_data$Biosample.type))

size = 20
num_types = length(types)

files = read.table("files.txt")

download_list = mat.or.vec(num_types*size,1)

for (i in 1:num_types) {
  sample_type = types[i]
  inds = which(meta_data$Biosample.type == sample_type)
  inds = sample(inds)
  
  for(j in 1:size) {
    file_name = as.character(meta_data[inds[j],1])
    pattern = paste("*",file_name,"*",sep = "")
    index = grep(pattern, files[,1])
    
    download_list[(i-1)*size + j] = as.character(files[index,1])
  }
}

write.table(download_list, "download_list.txt", sep="\t",row.names = FALSE, col.names = FALSE, quote = FALSE)

