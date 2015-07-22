setwd("~/Desktop/Work")

meta_data = read.csv("metadata_short.csv")
types = as.vector(unique(meta_data$Biosample.type))

setwd("~/Desktop/Work/Results/")

files = read.table("backup_100.txt", header = T)

rownames(files) = files$file_name

num_types = length(types)

excel = cbind(as.character(files$file_name), as.character(files$gender), as.character(files$chrXLength), as.character(files$chrYLength))

rownames(meta_data) = meta_data[,1]

relevant_names = excel[,1]

additional_cols_Experiment.accession = as.character(meta_data[relevant_names,]$Experiment.accession)
additional_cols_Biosample.term.id = as.character(meta_data[relevant_names,]$Biosample.term.id)

additional_cols_Biosample.term.name = as.character(meta_data[relevant_names,]$Biosample.term.name)
additional_cols_Biosample.type = as.character(meta_data[relevant_names,]$Biosample.type)
additional_cols_experimentType = rep("ChIP-Seq",length(additional_cols_Biosample.type))



excel = cbind(additional_cols_Biosample.term.id,
              as.character(files$file_name),
              additional_cols_Experiment.accession, 
              additional_cols_Biosample.term.name,
              additional_cols_Biosample.type,
              additional_cols_experimentType,
              as.character(files$gender), 
              as.character(files$chrXLength), 
              as.character(files$chrYLength))

colnames(excel) = c("Biosample.term.id", "File.accession", "Experiment.accession",
                    "Biosample.term.name", "Biosample.type", "Experiment.Type",
                    "Sex", "Num_XChrom", "Num_YChrom")

additional_statistical_cols = mat.or.vec(nrow(excel),2)
colnames(additional_statistical_cols) = c("%_MappedToX", "%_MappedToY")

for(i in 1:nrow(excel)) {
  additional_statistical_cols[i,"%_MappedToX"] = 100*as.numeric(excel[i,"Num_XChrom"])/(as.numeric(excel[i,"Num_XChrom"]) + as.numeric(excel[i,"Num_YChrom"]))
  additional_statistical_cols[i,"%_MappedToY"] = 100*as.numeric(excel[i,"Num_YChrom"])/(as.numeric(excel[i,"Num_XChrom"]) + as.numeric(excel[i,"Num_YChrom"]))
}

final_excel = cbind(excel,additional_statistical_cols)

only_human = meta_data[,c(1,11)]
human_ind = which(only_human[,2] == "Homo sapiens"  )
human_names = only_human[human_ind,]

intersect_human_names = intersect(human_names[,1], final_excel[,2])

rownames(final_excel) = final_excel[,2]

human_excel = final_excel[intersect_human_names,]

ratios = mat.or.vec(nrow(human_excel),1)

for(i in 1:nrow(human_excel)) {
  ratios[i] = as.numeric(human_excel[i,"Num_YChrom"])/as.numeric(human_excel[i,"Num_XChrom"])
}

human_excel_appended = cbind(human_excel,ratios)

female_ind = which(human_excel_appended[,"Sex"] == "female")
female_ratios = as.numeric(human_excel_appended[female_ind, "ratios"])
plot(density(female_ratios), main = "Female Y/X Ratios")
hist(female_ratios, main = "Female Y/X Ratios", col = "red", breaks = 10)


male_ind = which(human_excel_appended[,"Sex"] == "male")
male_ratios = as.numeric(human_excel_appended[male_ind, "ratios"])
plot(density(male_ratios), main = "Male Y/X Ratios")
hist(male_ratios, main = "Male Y/X Ratios", breaks = 10, col = "blue")


unknown_ind = which(human_excel_appended[,"Sex"] == "unknown")
unknown_ind_ratios = as.numeric(human_excel_appended[unknown_ind, "ratios"])
plot(density(unknown_ind_ratios), main = "Unknown Y/X Ratios")
hist(unknown_ind_ratios, main = "Unknown Y/X Ratios", breaks = 10, col = "green")


#Distributions:
plot(density(female_ratios), main = "Female Y/X Ratios")
plot(density(male_ratios), main = "Male Y/X Ratios")
plot(density(unknown_ind_ratios), main = "Unknown Y/X Ratios")

#Histograms:
hist(female_ratios, main = "Female Y/X Ratios", col = "red", breaks = 10, )
hist(male_ratios, main = "Male Y/X Ratios", breaks = 25, col = "blue")
hist(unknown_ind_ratios, main = "Unknown Y/X Ratios", breaks = 30, col = "green")

plot(density(ratios), main = "Y/X Ratios")

#write.table(final_excel, "final_excel_updated.txt", sep="\t",row.names = FALSE, col.names = TRUE, quote = FALSE)
