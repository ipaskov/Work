setwd("/Volumes/Ivan Paskov/Work/Results/")
table_1 = read.csv("final_excel_updated.csv")

setwd("/Volumes/Ivan Paskov/Work/")
meta_data = read.csv("metadata_short.csv")

only_human = meta_data[,c(1,11)]
human_ind = which(only_human[,2] == "Homo sapiens"  )
human_names = only_human[human_ind,]

intersect_human_names = intersect(human_names[,1], table_1[,2])
rownames(table_1) = table_1[,2]
human_excel = table_1[intersect_human_names,]

#Done with first set of analysis

setwd("/Volumes/Ivan Paskov/Seth_Work/Results/")
table_2 = read.csv("final_excel_updated2.csv")

#Done with second set of analysis

human_excel = rbind(human_excel,table_2)

ratios = mat.or.vec(nrow(human_excel),1)

for(i in 1:nrow(human_excel)) {
  ratios[i] = as.numeric(human_excel[i,"Num_YChrom"])/as.numeric(human_excel[i,"Num_XChrom"])
}

human_excel_appended = cbind(human_excel,ratios)

female_ind = which(human_excel_appended[,"Sex"] == "female")
female_ratios = as.numeric(human_excel_appended[female_ind, "ratios"])

male_ind = which(human_excel_appended[,"Sex"] == "male")
male_ratios = as.numeric(human_excel_appended[male_ind, "ratios"])

unknown_ind = which(human_excel_appended[,"Sex"] == "unknown")
unknown_ind_ratios = as.numeric(human_excel_appended[unknown_ind, "ratios"])

save(male_ratios,female_ratios,unknown_ind_ratios, file = "Results")


#Distributions:
plot(density(female_ratios), main = "Female Y/X Ratios")
plot(density(male_ratios), main = "Male Y/X Ratios")
plot(density(unknown_ind_ratios), main = "Unknown Y/X Ratios")

#Histograms:
hist(female_ratios, main = "Female Y/X Ratios", col = "red", breaks = 10, )
hist(male_ratios, main = "Male Y/X Ratios", breaks = 25, col = "blue")
hist(unknown_ind_ratios, main = "Unknown Y/X Ratios", breaks = 30, col = "green")

