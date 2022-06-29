###############################################################################################################
# DESCRIPTIVE DATA PREPROCESSING
#
# This is the preprocessing code for the PECAN data. We used two csv files, one with descriptive information
# and another one with all network matrices. 
###############################################################################################################

# Install packages and load libraries
install.packages("stringr")
install.packages("ggcorrplot")
install.packages('pheatmap')
install.packages("UpSetR")
install.packages("hash")
install.packages("tsne")
install.packages("Rtsne")
install.packages("tidyverse")
install.packages("hopkins")
install.packages("factoextra")
install.packages("devtools")
install_github("vqv/ggbiplot")
install.packages("berryFunctions")
install.packages("LoopDetectR")
install.packages("igraph")
install.packages("network")
install.packages("ggpubr")
library(ggpubr)
library(network)
library(igraph)
library(bootnet)
library(NetworkComparisonTest)
library(LoopDetectR)
library(qgraph)
library (berryFunctions)
library(ggbiplot)
library(devtools)
library(stats)
library(factoextra)
library(hopkins)
library(Rtsne)
library(tsne)
library(hash)
library(UpSetR)
library(pheatmap)
library(stringr)
library(ggcorrplot)
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

###############################################################################################################
# DESCRIPTIVE DATA PREPROCESSING
###############################################################################################################
descriptive_data_all <- read.csv("PECAN alldata.csv", sep = ";")

# check whether data has duplicates
descriptive_data_all[duplicated(descriptive_data_all$ID),]

# remove duplicates
descriptive_data_all <- descriptive_data_all[-c(393, 424), ]

# remove errors (EMTI, participant 762262)
descriptive_data_all[descriptive_data_all$ID == "EMTI",] # get the row
descriptive_data_all[descriptive_data_all$ID == "762262",] # get the row 
descriptive_data_all <- descriptive_data_all[-c(251, 352), ] # delete rows

# transform test-retest reliability (TRT) into number
descriptive_data_all[,26] <- readr::parse_number(descriptive_data_all[,26]) / 100

# create data with TRT of above 0.5
descriptive_data_TRT <- descriptive_data_all %>% filter(descriptive_data_all[,26] >= 0.5)

# count number of PHQ >= 10
descriptive_data_TRT %>% filter(descriptive_data_TRT$PHQ.sum >= 10) %>% count()

# count number of PHQ < 10 
descriptive_data_TRT %>% filter(descriptive_data_TRT$PHQ.sum < 10) %>% count()

# complete number of participants with TRT >= 0.5 
nrow(descriptive_data_TRT) 

# number of people excluded due to low TRT 
nrow(descriptive_data_all) - nrow(descriptive_data_TRT)

# information on sex
(descriptive_data_TRT %>% filter(descriptive_data_TRT$Male == 1) %>% count()) # count males - 36
(descriptive_data_TRT %>% filter(descriptive_data_TRT$Male == 0) %>% count()) # count females - 226
(descriptive_data_TRT %>% filter(descriptive_data_TRT$Male == -1) %>% count()) # count other - 3

# mean age 
round(mean(descriptive_data_TRT$Age),2)
round(sd(descriptive_data_TRT$Age),2)

# get rid of unnecessary columns
colnames(descriptive_data_TRT)
descriptive_data_TRT[,10:21] <- NULL # delete diagnosis
descriptive_data_TRT[,15:18] <- NULL # delete TRT for severity & modifiable
descriptive_data_TRT[,16:68] <- NULL # delete item weighted out-degree centralities
descriptive_data_TRT[,42:67] <- NULL # delete item severity scores 
descriptive_data_TRT[,42:93] <- NULL # delete UNK

# include missing problem numbers for IDs (were present in network data but missing in descriptives)
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "339500")] <- 15
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "381201")] <- 7
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "802259")] <- 7
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "814969")] <- 7
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "897406")] <- 8
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "897744")] <- 14
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "898884")] <- 12
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "977793")] <- 11
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "109287")] <- 11
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "385739")] <- 8
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "622693")] <- 8
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "121587")] <- 8
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "104644")] <- 8
descriptive_data_TRT$Prio.Problems[which(descriptive_data_TRT$ID == "151274")] <- 8
##############################################################################################################
# DESCPRIPTIVE DATA - DEPRESSED
##############################################################################################################
descriptive_data_depressed <- descriptive_data_TRT %>% filter(descriptive_data_TRT$PHQ.sum >= 10)

##############################################################################################################
# NETWORK DATA PREPROCESSING
##############################################################################################################
network_data_all <- list.files(path = "/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/individual csv n=429", 
                                    pattern = "*.csv", full.names = TRUE) %>% lapply(.,function(x)read.csv(x,sep=";")[-29:-31,]) %>% bind_rows() 

row_names <- c("", "Eat less", "No exercise", "Sleep problems", "Daytime resting", "Conflicts", 
               "Hypocondric worries", "Trouble concentrating", "Social media use", "Stays at home", 
               "Procrastinates", "Substance use", "Self-harm", "Suicidal thoughts", "Eat more",
               "Compulsions", "Ruminates", "Worries", "Flashbacks", "Panic", "Pain", "Social anxiety", 
               "Alone/sad", "Tired", "Stressed", "Bored", "Angry", "Sum")

file_names <- list.files(path = "/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/individual csv n=429", 
                         pattern = "*.csv") %>% gsub(".csv","",.) %>% as.numeric()

network_data_all <- cbind(ID = rep(file_names, each = 28), item = rep(row_names, 429), network_data_all)
network_data_all <- network_data_all[,1:31]
network_data_all[,3] <- NULL

colnames(network_data_all) <- c("ID", "Item", "Presence", "Unknown", "Eat less", "No exercise", "Sleep problems", 
                                     "Daytime resting", "Conflicts", "Hypocondric worries", "Trouble concentrating", 
                                     "Social media use", "Stays at home", "Procrastinates", "Substance use", "Self-harm", 
                                     "Suicidal thoughts", "Eat more", "Compulsions", "Ruminates", "Worries", "Flashbacks", 
                                     "Panic", "Pain", "Social anxiety", "Alone/sad", "Tired", "Stressed", "Bored", "Angry")
# delete unnecessary rows 
rows_relev <- subset(network_data_all, Presence == "Relev?")
network_data_all <- network_data_all[!row.names(network_data_all) %in% row.names(rows_relev),]
rows_sum <- subset(network_data_all, Item == "Sum")
network_data_all <- network_data_all[!row.names(network_data_all) %in% row.names(rows_sum),]

# include Unknown as row (for network matrix to be symmetrical)
rows_beginning <- subset(network_data_all, Item == "Eat less") # beginning of each individual network
stepper <- 0

for (row_name in rownames(rows_beginning)){
  row <- as.numeric(row_name)
  stepper <- stepper + 1
  network_data_all <- insertRows(network_data_all, row -stepper, new = NA)
  network_data_all[row-stepper,2] <- "Unknown"
}

rownames(network_data_all) <- 1:nrow(network_data_all)

for (row in 1:nrow(network_data_all)){
  if (is.na(network_data_all[row,3])){
    network_data_all[row,3] <- " "
    network_data_all[row,1] <- network_data_all[row+1,1]
  }
}

network_data_ukn <- network_data_all %>% filter(Unknown > 0) # all networks that included unknown
network_data_ukn_IDs <- unique(network_data_ukn$ID)

# include information whether unknown was present or not into the descriptive data
for (id in network_data_ukn_IDs){
  for (row in 1:nrow(network_data_all)){
    if (network_data_all[row,2] == "Unknown"){
      if (network_data_all[row,1] %in% id){
        network_data_all[row,3] <- 1
      }
    }
  }
}

##############################################################################################################
# NETWORK DATA TRT (SUBJECTS WITH A TRT > 0.5)
##############################################################################################################
ID_TRT <- descriptive_data_TRT$ID
network_data_TRT <- network_data_all %>% filter(ID %in% ID_TRT) 

##############################################################################################################
# NETWORK DEPRESSED DATA (INCLUDING UNKNOWN)
##############################################################################################################
network_data_depressed <- network_data_TRT %>% filter(ID %in% descriptive_data_depressed$ID)

##############################################################################################################
# SAVE ALL NEW DATAFRAMES
##############################################################################################################
# descriptives data
write.csv(descriptive_data_all,"/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/descriptive_data_all.csv", row.names = FALSE)
write.csv(descriptive_data_TRT,"/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/descriptive_data_TRT.csv", row.names = FALSE)
write.csv(descriptive_data_depressed,"/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/descriptive_data_depressed.csv", row.names = FALSE)

# network data
write.csv(network_data_all,"/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/network_data_all.csv", row.names = FALSE)
write.csv(network_data_TRT,"/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/network_data_TRT.csv", row.names = FALSE)
write.csv(network_data_depressed,"/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/network_data_depressed.csv", row.names = FALSE)
