setwd("/Volumes/Ivan Paskov/Seth_Work/Results/")
    
load("excel")

male_ind = which(human_excel_appended[,"Sex"] == "male")
female_ind = which(human_excel_appended[,"Sex"] == "female")
unknown_ind = which(human_excel_appended[,"Sex"] == "unknown")

male_points = human_excel_appended[male_ind,c("Num_XChrom", "Num_YChrom")]
female_points = human_excel_appended[female_ind,c("Num_XChrom", "Num_YChrom")]
unknown_points = human_excel_appended[unknown_ind,c("Num_XChrom", "Num_YChrom")]

average_num_reads = 0

temp = rbind(male_points,female_points,unknown_points)

for (i in 1:nrow(temp)) {
  average_num_reads = average_num_reads + sum(temp[i,])
}

average_num_reads = 20000000 #average_num_reads/nrow(temp)


for (i in 1:nrow(male_points)) {
  coeff = average_num_reads/sum(male_points[i,]) 
  male_points[i,] = male_points[i,]*coeff
}

for (i in 1:nrow(female_points)) {
  coeff = average_num_reads/sum(female_points[i,]) 
  female_points[i,] = female_points[i,]*coeff
}

for (i in 1:nrow(unknown_points)) {
  coeff = average_num_reads/sum(unknown_points[i,]) 
  unknown_points[i,] = unknown_points[i,]*coeff
}


plot(male_points, col = "skyblue", pch = 16)
par(new=T)
plot(female_points, col = "pink2", pch = 16)
par(new=T)
plot(unknown_points, col = "grey", pch = 16)
