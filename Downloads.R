setwd("/Volumes/Ivan Paskov/Work")

meta_data = read.csv("metadata_short.csv")
types = as.vector(unique(meta_data$Biosample.type))

size = 10
num_types = length(types)
name_holder = mat.or.vec(size,num_types)

for (i in 1:num_types) {
  sample_type = types[i]
  inds = which(meta_data$Biosample.type == sample_type)
  inds = sample(inds)
  
  for(j in 1:size) {
    name_holder[j,i] = as.character(meta_data[inds[j],1])
  }
}

files = read.table("files.txt")

pattern = paste("*",name_holder[1,1],"*",sep = "")

index = grep(pattern, files[,1])

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

