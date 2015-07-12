setwd("/Volumes/Ivan Paskov/Work")

meta_data = read.csv("metadata_short.csv")
types = as.vector(unique(meta_data$Biosample.type))

size = 20
num_types = length(types)

name_holder = mat.or.vec(1,num_types)
colnames(name_holder) = types

files = read.table("files.txt")

setwd("Downloads/")

downloaded_files = dir(pattern = "*.bam")

for (i in 1:length(downloaded_files)) {
  current_file = downloaded_files[i]
  
  file_name = strsplit(current_file,split = ".bam")[[1]]
  
  ind = which(meta_data[,1] == file_name)
  
  classification = as.character(meta_data[ind,"Biosample.type"])
  
  col_num = which(types == classification)
  
  name_holder[1,col_num] = name_holder[1,col_num] + 1
  
}

supplemental_download_list <- character()


for (j in 1:num_types) {
  print (j)
  while(name_holder[1,j] != 20) {
    #print(supplemental_download_list)
    sample_type = types[j]
    inds = which(meta_data$Biosample.type == sample_type)
    
    redundant = TRUE
    
    while(redundant) {
      inds = sample(inds)
      file_name = as.character(meta_data[inds[1],1])
      
      if(!file_name %in% downloaded_files) {
        redundant = FALSE
        
        pattern = paste("*",file_name,".bam","*",sep = "")
        index = grep(pattern, files[,1])
        
        print(as.character(files[index,1]))
        
        supplemental_download_list = c(supplemental_download_list, as.character(files[index,1]))
        
        name_holder[1,j] = name_holder[1,j] + 1
      }
    }
  }
}

write.table(supplemental_download_list, "supplemental_download_list.txt", sep="\t",row.names = FALSE, col.names = FALSE, quote = FALSE)





