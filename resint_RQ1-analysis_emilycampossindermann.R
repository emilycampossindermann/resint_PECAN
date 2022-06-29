###############################################################################################################
# RQ1 - Analysis
#
# This is the analysis code for research question 1. It is divided into the following sections: 
# 1) Symptom profiles (constellation, clustering using TSNE)
# 2) Symptom relations (co-occurrence matrices, edge count matrices, edge weight matrices)
# 3) Feedback loops 
# 4) Centrality 
# 5) Individual network comparison of two participants 
# 6) Group-level sub-network for comparison with Malgaroli et al., (2021)
###############################################################################################################
# load libraries 
library(ggpubr)
library(network)
library(igraph)
library(bootnet)
library(NetworkComparisonTest)
library(LoopDetectR)
library(qgraph)
library(berryFunctions)
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

# read in preprocessed data 
descriptive_data_all <- read.csv("descriptive_data_all.csv", sep = ",")
descriptive_data_TRT <- read.csv("descriptive_data_TRT.csv", sep = ",")
descriptive_data_depressed <- read.csv("descriptive_data_depressed.csv", sep = ",")

network_data_all <- read.csv("network_data_all.csv", sep = ",")
network_data_TRT <- read.csv("network_data_TRT.csv", sep = ",")
network_data_depressed <- read.csv("network_data_depressed.csv", sep = ",")

colnames(network_data_all) <- colnames(network_data_TRT) <- colnames(network_data_depressed) <- 
  c("ID", "Item", "Presence", "Unknown", "Eat less", "No exercise", "Sleep problems", 
    "Daytime resting", "Conflicts", "Hypocondric worries", "Trouble concentrating", 
    "Social media use", "Stays at home", "Procrastinates", "Substance use", "Self-harm", 
    "Suicidal thoughts", "Eat more", "Compulsions", "Ruminates", "Worries", "Flashbacks", 
    "Panic", "Pain", "Social anxiety", "Alone/sad", "Tired", "Stressed", "Bored", "Angry")

row_names <- c("", "Eat less", "No exercise", "Sleep problems", "Daytime resting", "Conflicts", 
               "Hypocondric worries", "Trouble concentrating", "Social media use", "Stays at home", 
               "Procrastinates", "Substance use", "Self-harm", "Suicidal thoughts", "Eat more",
               "Compulsions", "Ruminates", "Worries", "Flashbacks", "Panic", "Pain", "Social anxiety", 
               "Alone/sad", "Tired", "Stressed", "Bored", "Angry", "Sum")

##############################################################################################################
# 1) SYMPTOM PROFILES
##############################################################################################################
descriptive_data_depressed_subset <- descriptive_data_depressed[,c("ID","Prio.Problems", "Eats.less.Modif", 
                                                                   "No.exercise.Modif", "Insomnia.Modif", "Resting.Modif", 
                                                                   "Conflicts.Modif", "Hypocondria.Modif", "Unfocused.Modif", 
                                                                   "social.media.Modif", "Stayshome.Modif", "Procrast.Modif", 
                                                                   "Substances.Modif", "Selfharm.Modif", "Suicidal.Modif", 
                                                                   "Eats.more.Modif", "Compulsions.Modif", "Ruminate.Modif", 
                                                                   "Worry.Modif", "Flashback.avoid.Modif", "Panic.avoid.Modif", 
                                                                   "Pain.avoid.Modif", "Social.anxiety.avoid.Modif", 
                                                                   "Alone.sad.avoid.Modif", "Tired.avoid.Modif", "Stressed.avoid.Modif",     
                                                                   "Bored.avoid.Modif", "Angry.avoid.Modif")]
# data frame with count of symptoms and percentages
symptoms_count <- descriptive_data_depressed_subset %>% group_by(Prio.Problems) %>% count()
symptoms_count$percentage <- round(symptoms_count$n / sum(symptoms_count$n), 2)

##############################################################################################################
# CONSTELLATIONS
# We created a new column in descriptive_data_TRT with a list consisting of the present symptoms in each 
# participant (excluding Unknown)
##############################################################################################################
descriptive_data_TRT <- descriptive_data_TRT %>% add_column(Constellation = NA)

for (id in descriptive_data_TRT$ID){
  present_symptoms <- network_data_TRT %>% filter(ID %in% id) %>% filter(Presence == 1) %>% filter(!(Item == "Unknown")) %>% select("Item") %>% as.list()
  for (row in 1:nrow(descriptive_data_TRT)){
    if (descriptive_data_TRT[row,1] == id){
      descriptive_data_TRT[row, 42] <- list(present_symptoms)
    }
  }
}

# view constellations for PHQ9 >= 10
symptom_profiles_depressed <- descriptive_data_TRT %>% filter(descriptive_data_TRT$PHQ.sum >= 10) %>% group_by(Constellation) %>% count()
number_included_symptoms <- c(rep(7,46), rep(8,20),rep(9,18),rep(10,16),rep(11,17),rep(12,12), rep(13,12), rep(14,8), rep(15,14))
mean(number_included_symptoms)
median(number_included_symptoms)

##############################################################################################################
# CLUSTERING SYMPTOM PROFILES
# We first performed dimensionality reduction of the symptom profiles using tsne. The plot showed that there 
# were no clear clusters among the symptom profiles. We then computed the hopkins' statistic which further 
# indicated tat the data is not clusterable. 
##############################################################################################################
# TSNE
data_tsne <- network_data_depressed %>% select(ID,Item,Presence) %>% filter(Item != "Unknown")
data_tsne <- data_tsne %>% pivot_wider(names_from = Item, values_from = Presence, values_fill = 0) 
data_tsne[is.na(data_tsne)] <- 0

set.seed(12934801)
tSNE_fit <- data_tsne %>% select(where(is.numeric)) %>% column_to_rownames("ID") %>% scale() %>% Rtsne(perplexity = 2,theta = 0.5)
tSNE_df <- tSNE_fit$Y %>% as.data.frame() %>% rename(tSNE1="V1",tSNE2="V2") %>% mutate(ID=data_tsne$ID)
tSNE_df <- tSNE_df %>% inner_join(data_tsne, by="ID")

# HOPKINS STATISTIC
set.seed(12934801)
res <- get_clust_tendency(data_tsne[2:27], n = nrow(data_tsne)-1, graph = FALSE)
res$hopkins_stat # close to 0.5 > the data set is not clusterable 

##############################################################################################################
# CO-OCCURRENCES
# We created a non-symmetrical matrix (symptom_co_occurrences_relative) including the proportion of co-
# occurrence when one of the two symptoms was present. It reads: When the row symptom was present, it co-
# occurred X % with the column symptom. 
##############################################################################################################
symptoms_co_occurrences <- matrix(0, nrow = 27, ncol = 27)
names <- append("Unknown", row_names[2:27])
rownames(symptoms_co_occurrences) <-colnames(symptoms_co_occurrences) <- names

for (i in 1:26){
  symptom_i <- names[i]
  for (j in (i+1):27){
    symptom_j <- names[j]
    count <- 0
    for (id in descriptive_data_depressed$ID){
      individual_network <- network_data_depressed %>% filter(ID == id) %>% filter(Presence == 1)
      if(symptom_i %in% individual_network$Item & symptom_j %in% individual_network$Item){
        count <- count+1
      }
    }
    symptoms_co_occurrences[i,j] <- count
    symptoms_co_occurrences[j,i] <- count
  }
}

# count occurrence of each symptom 
symptoms_count_each <- network_data_depressed %>% filter(Presence == 1) %>% group_by(Item) %>% count()
symptoms_count_each <- symptoms_count_each %>% arrange(factor(Item, levels = names))
symptoms_count_each$percentage <- round((symptoms_count_each$n / 163),2)
symptoms_count_each$scaled <- symptoms_count_each$n * (6/132) + 4 # for node sizes in network visualizations

round(mean(symptoms_count_each$n),2) # mean

symptoms_co_occurrences_relative <- symptoms_co_occurrences[1:27,1:27]

for (row in 1:27){
  for (i in 1:nrow(symptoms_count_each )){
    if (symptoms_count_each[i,1] == rownames(symptoms_co_occurrences_relative)[row]){
      symptoms_co_occurrences_relative[row,] <- (symptoms_co_occurrences_relative[row,] / as.numeric(symptoms_count_each[i,2]))
    } 
  }
}

##############################################################################################################
# 2) SYMPTOM RELATIONS
##############################################################################################################

##############################################################################################################
# COMMUNITY DETECTION FOR EACH INDIVIDUAL
# We performed community detection for each individual to see whether we could find consistent symptom clusters
# across individuals.
##############################################################################################################
df_community <- data.frame(matrix(ncol = 3, nrow = 0)) # crate a data frame 
colnames(df_community) <- c("ID", "Item", "infomap") # column names 

for (Id in descriptive_data_TRT$ID){
  # for each individual get subset of network where presence == 1 - store items in individual data frame (excluding Unknown)
  subset_com <- network_data_TRT %>% filter(ID == Id) %>% filter(Presence == 1) %>% filter(Item != "Unknown")
  item_list <- subset_com$Item
  subset_com <- subset_com %>% select(item_list)
  rownames(subset_com) <- item_list
  subset_com[is.na(subset_com)] <- 0 # make all NA 0
  
  # check whether row and column zeros if so, delete (infomap cannot handle unconnected nodes)
  delete_items <- c()
  for(item in 1:length(item_list)){
    if(all(subset_com[item,] %in% 0) == TRUE){
      if(all(subset_com[,item] %in% 0) == TRUE){
        delete_items <- append(delete_items, item)
      }
    }
  }
  
  if(length(delete_items) > 0){
    subset_com <- subset_com[- delete_items, - delete_items]
  }

  # create a matrix (get rid of columns that were not reported)
  matrix_com <- data.matrix(subset_com) # transform data frame to matrix
  matrix_com <- t(matrix_com)
  
  # create qgraph object
  g <- qgraph(matrix_com, 
              layout = "spring",
              colFactor = 1,
              minimum = 0)
  
  # transform graph object to igraph object
  graph_com = igraph::as.igraph(g,attributes = TRUE)
  
  # community detection with infomap (louvain does not work for directed networks)
  set.seed(123)
  infomap <- cluster_infomap(graph_com)
  
  df_com_id <-  data.frame(ID = Id, Item = rownames(subset_com))
  df_com_id$infomap <- membership(infomap)

  # merge individual df with group df 
  df_community <- merge(df_community, df_com_id, all = TRUE) 
}

# get data only from depressed sample 
df_community_depressed <- df_community %>% filter(ID %in% descriptive_data_depressed$ID)

# make data frame to plot how many times symptoms were reported as part of different communities
community_plot <- df_community_depressed %>% group_by(Item,infomap) %>% count()

# calculate percentages
community_plot$percentage <- round((community_plot$n / c(rep(87,3), rep(60,3), rep(89,3), rep(10,2), rep(21,2), rep(69,2),
                                                         rep(57,1), rep(37,3), rep(19,3), rep(24,2), rep(82,2), rep(88,2), rep(42,2),
                                                         rep(97,4), rep(92,3), rep(14,2), rep(78,2), rep(59,3), rep(78,3), 
                                                         rep(43,2), rep(94,4), rep(42,3), rep(20,3), rep(107,3), rep(101,3),
                                                         rep(103,3))),2)

# 86 items with no community
nrow(network_data_depressed %>% filter(Presence == 1) %>% filter(!(Item == "Unknown"))) - nrow(df_community_depressed)
nrow(network_data_depressed %>% filter(Presence == 1) %>% filter(!(Item == "Unknown")))

com_number <- c()

for (id in descriptive_data_depressed$ID){
  subset_com <- df_community_depressed %>% filter(ID == id)
  number_com <- length(unique(subset_com$infomap))
  com_number <- append(com_number,number_com)
}

df_com_number <- data.frame(ID = descriptive_data_depressed$ID, Communities = com_number)
df_com_number <- df_com_number %>% group_by(Communities) %>% count()

##############################################################################################################
# SYMPTOM PAIRS & EDGE WEIGHT MATRIX
# Here we created a matrix showing how often a given edge was present. 
##############################################################################################################
symptom_links_presence <- matrix(0, nrow = 27, ncol = 27)
rownames(symptom_links_presence) <- colnames(symptom_links_presence) <- names

symptom_links_weights <- matrix(0, nrow = 27, ncol = 27)
rownames(symptom_links_weights) <- colnames(symptom_links_weights) <- names

subset_present_symptoms <- network_data_depressed %>% filter(ID %in% descriptive_data_depressed$ID) %>% filter(Presence == 1) 

for (symptom in names){
  subset_symp <- subset_present_symptoms %>% filter(Item == symptom)
  for (row in 1:nrow(subset_present_symptoms)){
    for (column in 4:ncol(subset_present_symptoms)){
      if (is.na(subset_symp[row,column]) == FALSE){
        if (subset_symp[row,column] > 0){
          symptom_links_presence[names[column - 3], symptom] <- symptom_links_presence[names[column - 3], symptom] + 1
          symptom_links_weights[names[column - 3], symptom] <- symptom_links_weights[names[column - 3], symptom] + subset_symp[row,column]
        }
      } 
    }
  }
}

# number of symptom pairs (relative)
symptom_links_presence_relative <- symptom_links_presence / symptoms_co_occurrences
symptom_links_presence_relative[is.na(symptom_links_presence_relative)] <- 0

# average edge weight matrix (over all individuals) (NOT USED IN REPORT !!)
symptom_links_weights_average <- symptom_links_weights / 163

# relative edge weight matrix (over all individuals which have both symptoms) (NOT USED IN REPORT !!)
symptom_links_weights_relative <- symptom_links_weights / symptoms_co_occurrences
symptom_links_weights_relative[is.na(symptom_links_weights_relative)] <- 0

# relative edge weight matrix (over all individuals which have specific edge)
symptom_links_weights_relative_edge <- symptom_links_weights / symptom_links_presence
symptom_links_weights_relative_edge[is.na(symptom_links_weights_relative_edge)] <- 0

##############################################################################################################
# STRONGEST EDGES (NOT USED IN REPORT !!)
##############################################################################################################
strongest_edges <- descriptive_data_depressed %>% select(ID,Prio.Problems,PHQ.sum)

for (id in strongest_edges$ID){
  subset_df <- network_data_depressed %>% filter(ID == id) 
  maximum <- max(subset_df[,4:ncol(subset_df)],na.rm=TRUE)
  index <- which(subset_df == maximum, arr.ind = T)
  strongest_edges$max[which(strongest_edges$ID == id)] <- maximum
  for (rows in 1:nrow(index)){
    strongest_edges[which(strongest_edges$ID == id),rows+4] <- paste(names[as.numeric(index[rows,2])-3], names[as.numeric(index[rows,1])])
  }
}

# transform into long format 
strongest_edges_long <- strongest_edges %>% 
  pivot_longer(V5:V13, names_to = "number", values_to = "symptom_links_presence")

strongest_pairs <- strongest_edges_long %>% group_by(symptom_links_presence) %>% count()
strongest_edges_max_counts <- strongest_edges %>% group_by(max) %>% count()
strongest_edges_max_counts$percentage <- round(strongest_edges_max_counts$n/sum(strongest_edges_max_counts$n),2)

##############################################################################################################
# 3) FEEDBACK LOOPS
# Here we performed loop detection for each individual and calculated for each symptom (items_feedback_loops)
# a) n: total number of loops which included the symptom
# b) participants: number of participants which reported the symptom within feedback loops 
# c) percentage_fl: percentage of (total) feedback loops that included the symptom 
# d) perc_part: percentage of participants who reported the symptom + did so within a feedback loop
# e) average_fl: average number of feedback loops per person containing the symptom
# f) no_fl_person: average percentage of (individual) feedback loops that included the symptom
# 
# We also looked at the length of the feedback loops (how many included edges) and prominent feedback loops.
##############################################################################################################
df_feedback_loops <- data.frame(matrix(ncol = 3, nrow = 0)) # create empty data frame for all individuals
colnames(df_feedback_loops) <- c('ID', 'loop', 'length') # column names

items_feedback_loops <- data.frame(matrix(ncol=3,nrow=27)) # create empty data frame for average across symptoms
colnames(items_feedback_loops) <- c('item', 'n','participants') # column names
items_feedback_loops$item <- c(1:27) 
items_feedback_loops$n <- items_feedback_loops$participants <- 0

fl_proportion <- data.frame(matrix(ncol=27, nrow=0)) # create empty data frame for proportion of feedback loops per symptom
colnames(fl_proportion) <- names # column names (item names)

for (Id in descriptive_data_depressed$ID){
  # build individual matrix
  subset_id <- network_data_depressed %>% filter(ID == Id) 
  subset_id[is.na(subset_id)] <- 0 # make all NA 0
  matrix <- data.matrix(subset_id[,4:ncol(subset_id)]) # transform data frame to matrix
  matrix <- t(matrix) # transpose matrix
  
  # identify loops 
  loops_df_id <- find_loops(matrix, max_num_loops = 2000)
  
  # make individual feedback loop proportion data frame
  fl_proportion_ind <- data.frame(matrix(ncol=27, nrow=1,0))
  colnames(fl_proportion_ind) <- names
  
  # identify total feedback loop count per item 
  for (item in 1:27){
    for (row in 1:nrow(loops_df_id)){
      if (item %in% unlist(loops_df_id[row,1])){
        items_feedback_loops[item,2] <- items_feedback_loops[item,2] + 1
        fl_proportion_ind[1,item] <- fl_proportion_ind[1,item] + 1
      }
    }
  }
  
  # identify count of participants who reported items within feedback loops
  for (item in 1:27){
    for (row in 1:nrow(loops_df_id)){
      if (item %in% unlist(loops_df_id[row,1])){
        items_feedback_loops[item,3] <- items_feedback_loops[item,3] + 1
        break
      }
    }
  }
  
  # divide number of feedback loops per item by total number of individual feedback loops 
  for (column in 1:27){
    if (fl_proportion_ind[1,column] != 0){
      fl_proportion_ind[1,column] <- fl_proportion_ind[1,column] / nrow(loops_df_id)
    }
  }
  
  # change loop from list to character (to compare & count later)
  loops_df_id$loop <- as.character(loops_df_id$loop)
  loops_df_id$ID <- Id
  loops_df_id <- loops_df_id %>% select(ID, everything())
  
  # merge data into data frames 
  df_feedback_loops <- merge(df_feedback_loops, loops_df_id[,1:3], all = TRUE) 
  fl_proportion <- merge(fl_proportion, fl_proportion_ind, all = TRUE)
}

# add information to the individual item data frame 
items_feedback_loops$percentage_fl <- round((items_feedback_loops$n /nrow(df_feedback_loops %>% filter(!(loop == "NULL")))),2)
items_feedback_loops$item_names <- names
items_feedback_loops$perc_part <- round(items_feedback_loops$participants / symptoms_count_each$n,2)
items_feedback_loops$average_fl <- round(items_feedback_loops$n / items_feedback_loops$participants,2)
items_feedback_loops$average_fl[1] <- 0 # for unknown
items_feedback_loops$no_fl_person <- round(colSums(fl_proportion) / items_feedback_loops$participants,2)

# count & percentage per loop length
df_feedback_length_grouped <- df_feedback_loops %>% group_by(length) %>% count()
df_feedback_length_grouped$percentage <- round(df_feedback_length_grouped$n / nrow(df_feedback_loops %>% 
                                                                                     filter(!(loop == "NULL"))),2)

# filter most prominent loops (that were reported by more than 10 individuals across the sample)
df_feedback_loop_grouped <- df_feedback_loops %>% group_by(loop) %>% count() %>% filter(n > 10)

##############################################################################################################
# 4) CENTRALITY
# Here we calculated the out and in-degree centrality as well as the out-/to in-degree centrality ratio for 
# each symptom within each individual. Lastly we computed the average out-/to in-degree centrality ratio for
# each symptom. 
##############################################################################################################
centrality <- data.frame(matrix(ncol = 5, nrow = 0)) # make centrality data frame
colnames(centrality) <- c("ID", "Item", "Outdegree", "Indegree", "Out_In_ratio") # column names

for (Id in descriptive_data_TRT$ID){
  # build individual matrix
  subset_id_cent <- network_data_TRT %>% filter(ID == Id) # take the individual subset network
  subset_id_cent[is.na(subset_id_cent)] <- 0 # make all NA 0
  matrix_cent <- data.matrix(subset_id_cent[,4:ncol(subset_id_cent)]) # transform data frame to matrix
  matrix_cent <- t(matrix_cent) # transpose matrix
  matrix_cent <- matrix_cent[2:27,2:27] # exclude unknown
  
  centrality_item <- centrality(matrix_cent) # compute centrality 
  centrality_item$OutDegree[centrality_item$OutDegree == 0] <- 0.001 # transform 0 to 0.001 to allow for out/in division
  centrality_item$InDegree[centrality_item$InDegree == 0] <- 0.001 # transform 0 to 0.001 to allow for out/in division
  
  # create individual centrality data frame
  centrality_subset <- data.frame(ID = c(rep(Id,26)),
                                  Item = names[2:27],
                                  Outdegree = centrality_item$OutDegree,
                                  Indegree = centrality_item$InDegree,
                                  Out_In_ratio = round(centrality_item$OutDegree / centrality_item$InDegree,2))
  
  present_symptoms <- subset_id_cent %>% filter(Presence == 1) %>% filter(Item != "Unknown") 
  present_symptoms <- present_symptoms$Item # make list with present symptoms
  
  # make empty vector and fill it with the symptoms that were not present, then delete the corresponding rows 
  delete <- c()
  for(row in 1:nrow(centrality_subset)){
    if (!(centrality_subset[row,2] %in% present_symptoms)){
      delete <- append(delete, row)
    }
  }
  centrality_subset <- centrality_subset[-delete,]
  
  # merge data into data frame 
  centrality <- merge(centrality, centrality_subset, all = TRUE) 
}

# make centrality data frame for depressed individuals and cap out/in ratio at 5 (to allow for meaningful plot)
centrality_depressed <- centrality %>% filter(ID %in% descriptive_data_depressed$ID) 
centrality_depressed$Out_In_ratio[centrality_depressed$Out_In_ratio > 5] <- 5

# average out/in degree centrality ratio per symptom
n_symptoms <- centrality_depressed %>% group_by(Item) %>% count()
average_out_in_ratio <- ddply(centrality_depressed, .(Item), transform, sum_name = sum(Out_In_ratio))
average_out_in <- round(unique(average_out_in_ratio$sum_name) / n_symptoms$n,2) # average out_in_ratio

df_average_out_in_ratio <- data.frame(Item = n_symptoms$Item,
                                      out_in = average_out_in)
##############################################################################################################
# 5) INDIVIDUAL NETWORK COMPARISON 
# Here we performed an individual network comparison between participant 180989 and participant 200234. 
# We therefore computed both individual networks, looked at the centralities & feedback loops.
##############################################################################################################

##############################################################################################################
# PARTICIPANT 1 - 180989 
##############################################################################################################
participant01 <- 180989
network_participant01 <- network_data_TRT %>% filter(ID == participant01) %>% filter(Presence == 1) 
network_participant01 <- network_participant01 %>% select(network_participant01[,2])
network_participant01[is.na(network_participant01)] <- 0 # make all NA 0
matrix_participant01 <- t(data.matrix(network_participant01))# transform df to matrix
matrix_participant01_excl_ukn <- matrix_participant01[2:nrow(matrix_participant01),2:ncol(matrix_participant01)]

df_participant01 <- data.frame(matrix(0, ncol = 3, nrow = 7))
colnames(df_participant01) <- c("Item", "Out", "In")
df_participant01$Item <- rownames(matrix_participant01_excl_ukn)

# centrality
df_participant01$Out <- centrality(matrix_participant01_excl_ukn)$OutDegree
df_participant01$In <- centrality(matrix_participant01_excl_ukn)$InDegree
df_participant01$Out[df_participant01$Out == 0] <- 0.001 
df_participant01$In[df_participant01$In == 0] <- 0.001 
df_participant01$Out_in <- round(df_participant01$Out / df_participant01$In,2)
df_participant01$Out_in[6] <- df_participant01$Out_in[6] / 10000
df_participant01$Out_in[7] <- df_participant01$Out_in[7] / 10000

pal_participant01 <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_participant01$Out_in), min(df_participant01$Out_in)))
df_participant01$hex_from_scales <- pal_participant01(df_participant01$Out_in)

network_participant01[5:nrow(network_participant01),1] <- network_participant01[5:nrow(network_participant01),1] / 100

network_vis_01 <- qgraph(matrix_participant01_excl_ukn, 
                         shape = "circle", 
                         layout = "spring",
                         theme = "colorblind",
                         color = df_participant01$hex_from_scales,
                         colFactor = 1,
                         labels = c("sleeppro", "subst", "rumin", "worry", "flashb", "panic", "stress"),
                         minimum = 0,
                         pie = network_participant01[2:nrow(network_participant01),1],
                         pieColor = "yellow",
                         pieColor2 = "gray88",
                         legend.cex = 0.4,
                         #legend.mode = 'style2',
                         GLratio = 2.5,
                         layoutScale = c(1, 1),
                         layoutOffset = c(0,0),
                         nodeNames = rownames(matrix_participant01_excl_ukn)
)

# community detection (infomap)
graph_participant01 = igraph::as.igraph(network_vis_01,attributes = TRUE)
set.seed(123)
imc <- cluster_infomap(graph_participant01)
membership(imc)
communities(imc)
plot(imc, graph_participant01)

# feedback loops 
loops_participant01 <- find_loops(matrix_participant01, max_num_loops = 2000)
loops_count_participant01 <- loops_participant01 %>% filter(length >= 1) %>% count()

##############################################################################################################
# PARTICIPANT 2 - 200234
##############################################################################################################
participant02 <- 200234
network_participant02 <- network_data_TRT %>% filter(ID == participant02) %>% filter(Presence == 1) 
network_participant02 <- network_participant02 %>% select(network_participant02[,2])
network_participant02[is.na(network_participant02)] <- 0 # make all NA 0
matrix_participant02 <- t(data.matrix(network_participant02)) # transform df to matrix

df_participant02 <- data.frame(matrix(0, ncol = 3, nrow = 15))
colnames(df_participant02) <- c("Item", "Out", "In")
df_participant02$Item <- rownames(matrix_participant02)

# centrality
df_participant02$Out <- centrality(matrix_participant02)$OutDegree
df_participant02$In <- centrality(matrix_participant02)$InDegree
df_participant02$Out[df_participant02$Out == 0] <- 0.001 
df_participant02$In[df_participant02$In == 0] <- 0.001 
df_participant02$Out_in <- round(df_participant02$Out / df_participant02$In,2)

pal_participant02 <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_participant02$Out_in), min(df_participant02$Out_in)))
df_participant02$hex_from_scales <- pal_participant02(df_participant02$Out_in)

network_vis_02 <- qgraph(matrix_participant02, 
                         shape = "circle", 
                         layout = "spring",
                         theme = "colorblind",
                         color = df_participant02$hex_from_scales,
                         colFactor = 1,
                         labels = c("eatless", "sleeppro", "dayrest", "troubco", "subst", "suicid", "rumin", "worry", "panic", 
                                    "pain", "socanx", "sad", "stress", "bored", "angry"),
                         minimum = 0,
                         pie = c(rep(0,15)),
                         pieColor = "yellow",
                         pieColor2 = "gray88",
                         legend.cex = 0.4,
                         #legend.mode = 'style2',
                         GLratio = 2.5,
                         layoutScale = c(1, 1),
                         layoutOffset = c(0,0),
                         nodeNames = rownames(matrix_participant02)
)

graph_participant02 = igraph::as.igraph(network_vis_02,attributes = TRUE)

# community detection (infomap)
set.seed(123)
imc <- cluster_infomap(graph_participant02)
membership(imc)
communities(imc)
plot(imc, graph_participant02)

# feedback loops
loops_participant02 <- find_loops(matrix_participant02, max_num_loops = 2000)
loops_count_participant02 <- loops_participant02 %>% filter(length >= 1) %>% count()

##############################################################################################################
# 6) COMPARISON BETWEEN AVERAGE PECAN & GROUP-LEVEL MDD NETWORK 
# Here we computed an average (undirected) network across the entire depressed sample for the group comparison.
# For the network visualization we chose the colors corresponding to the different communities reported by 
# Malgaroli et al., 2021, to facilitate direct comparison. We performed community detection using the spinglass
# algorithm, as this was also used in the Malgaroli et al., 2021 paper.
##############################################################################################################
# make initial 27x27 matrix with zeros 
matrix_group <- matrix(ncol = 27, nrow = 27,0)

# in a for loop add matrices for each participant 
for (Id in descriptive_data_depressed$ID){
  # build individual matrix
  subset_individual <- network_data_depressed %>% filter(ID == Id) 
  subset_individual[is.na(subset_individual)] <- 0 # make all NA 0
  matrix_individual <- data.matrix(subset_individual[,4:ncol(subset_individual)]) # transform data frame to matrix
  matrix <- t(matrix) # transpose matrix
  matrix_group <- matrix_group + matrix_individual # add to group matrix
}

# average network 
average_network <- round((matrix_group / nrow(descriptive_data_depressed)),2) # divide matrix by number of participants
average_network_undirected <- average_network + t(average_network) # make it undirected
average_network_undirected[c(4,8,14,23,24,26),c(4,8,14,23,24,26)] # take only sub-sample relevant for comparison

average_network_vis <- qgraph(average_network_undirected[c(2,4,8,14,15,23,24,26),c(2,4,8,14,15,23,24,26)], 
                              negDashed=TRUE, 
                              layout = "spring",
                              colFactor = 1,
                              color = c("deepskyblue4","deepskyblue4","deepskyblue4","firebrick3","deepskyblue4",
                                        "firebrick3","deepskyblue4","firebrick3"),
                              labels = names[c(2,4,8,14,15,23,24,26)],
                              minimum = 0)

# centrality
centralityPlot(average_network_undirected)

# community detection (spinglass algorithm)
set.seed(12934801)
graph_average_network = igraph::as.igraph(average_network_vis,attributes = TRUE)
sgc <- spinglass.community(graph_average_network)
sgc$membership

