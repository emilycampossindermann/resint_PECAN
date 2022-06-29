###############################################################################################################
# RQ3 - Analysis
#
# This is the analysis code for research question 3. It is divided into the following sections: 
# 1) Symptom profiles (MDD & Insomnia occurrences within symptom profiles) 
# 2) Symptom relations (Community detection, MDD & Insomnia co-occurrences, MDD & Insomnia relations)
# 3) Feedback loops
# 4) Centrality
# 5) Groups 
###############################################################################################################

# source RQ1 analysis script
source("/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/resint_RQ1-analysis_emilycampossindermann.R")

##############################################################################################################
# 1) MDD-INSOMNIA SYMPTOM PROFILES
##############################################################################################################
# Presence of individual symptoms in whole sample 
mdd_in <- c("Alone/sad", "Bored", "Daytime resting", "Sleep problems", "Tired", "Unknown")

df_symptoms_total <- network_data_TRT %>% filter(Presence == 1) %>% group_by(Item) %>% count()
df_symptoms_total$percentage <- round((df_symptoms_total$n / nrow(descriptive_data_TRT)),2)

# get only MDD and insomnia data frame 
df_mdd_in <- df_symptoms_total %>% filter(Item %in% mdd_in)

mean(df_symptoms_total$percentage)
median(df_symptoms_total$percentage)

##############################################################################################################
# MDD-INSOMNIA CO-OCCURRENCES 
##############################################################################################################
occurrences_mdd_in <- matrix(0, nrow = 6, ncol = 6)
rownames(occurrences_mdd_in) <- colnames(occurrences_mdd_in) <- mdd_in

for (i in 1:5){
  s1 <- mdd_in[i]
  for (j in (i+1):6){
    s2 <- mdd_in[j]
    count = 0
    for (id in descriptive_data_TRT$ID){
      subset <- network_data_TRT %>% filter(ID == id) %>% filter(Presence == 1)
      if(s1 %in% subset$Item & s2 %in% subset$Item){
        count = count+1
      }
    }
    occurrences_mdd_in[i,j] <- count
    occurrences_mdd_in[j,i] <- count
  }
}

occurrences_mdd_in_relative <- round((occurrences_mdd_in / df_mdd_in$n),2)

##############################################################################################################
# 2) MDD-INSOMNIA SYMPTOM RELATIONS
##############################################################################################################

##############################################################################################################
# COMMUNITY DETECTION
##############################################################################################################
# make plot with only specified MDD and I symptoms 
community_plot_mdd_in <- df_community %>% filter(Item %in% mdd_in[1:5]) %>% group_by(Item,infomap) %>% count()

community_plot_mdd_in$percentage <- round((community_plot_mdd_in$n / c(rep(117,3),rep(134,3),rep(102,2),rep(125,2),rep(156,3))),2)

##############################################################################################################
# SYMPTOM PAIRS 
##############################################################################################################
# Symptom pairs (small heat map 5x5) - relative & absolute 
symptom_links_count_mdd_in <- matrix(0, nrow = 6, ncol = 6)
rownames(symptom_links_count_mdd_in) <- colnames(symptom_links_count_mdd_in) <- mdd_in

# edge weight matrix
symptom_links_weights_mdd_in <- matrix(0, nrow = 6, ncol = 6)
rownames(symptom_links_weights_mdd_in) <- colnames(symptom_links_weights_mdd_in) <- mdd_in

present_symptoms_mdd_in <- network_data_TRT%>% filter(Presence == 1) %>% filter(Item %in% mdd_in) %>% select(Item, all_of(mdd_in))

for (s in mdd_in){
  subset_s <- present_symptoms_mdd_in %>% filter(Item == s)
  for (row in 1:nrow(present_symptoms_mdd_in)){
    for (column in 2:ncol(present_symptoms_mdd_in)){
      if (is.na(subset_s[row,column]) == FALSE){
        if (subset_s[row,column] > 0){
          symptom_links_count_mdd_in[mdd_in[column - 1], s] <- symptom_links_count_mdd_in[mdd_in[column - 1], s] + 1
          symptom_links_weights_mdd_in[mdd_in[column - 1], s] <- symptom_links_weights_mdd_in[mdd_in[column - 1], s] + subset_s[row,column]
        }
      } 
    }
  }
}

# plot number of symptom pairs (relative)
symptom_links_count_mdd_in_relative <- round((symptom_links_count_mdd_in / occurrences_mdd_in),2)
symptom_links_count_mdd_in_relative[is.na(symptom_links_count_mdd_in_relative)] <- 0

# plot relative edge weight matrix (over all individuals which have specific edge)
symptom_links_weights_mdd_in_relative <- round((symptom_links_weights_mdd_in / symptom_links_count_mdd_in),2)
symptom_links_weights_mdd_in_relative[is.na(symptom_links_weights_mdd_in_relative)] <- 0

##############################################################################################################
# 3) FEEDBACK LOOPS
##############################################################################################################
df_feedback_loops_mdd_in <- data.frame(matrix(ncol = 3, nrow = 0)) # create empty data frame for all individuals
colnames(df_feedback_loops_mdd_in) <- c('ID', 'loop', 'length') # column names

items_mdd_in_fl <- data.frame(matrix(ncol=3,nrow=27)) # create empty data frame for average across symptoms
colnames(items_mdd_in_fl) <- c('item', 'n','participants') # column names
items_mdd_in_fl$item <- c(1:27)
items_mdd_in_fl$n <- items_mdd_in_fl$participants <- 0

fl_proportion_mdd_in <- data.frame(matrix(ncol=27, nrow=0)) # create empty data frame for proportion of feedback loops per symptom
colnames(fl_proportion_mdd_in) <- names # column names (symptom names)

for (Id in descriptive_data_TRT$ID){
  # build individual matrix
  subset_id <- network_data_TRT %>% filter(ID == Id) 
  subset_id[is.na(subset_id)] <- 0 # make all NA 0
  matrix <- data.matrix(subset_id[,4:ncol(subset_id)]) # transform data frame to matrix
  matrix <- t(matrix) # transpose matrix
  
  # identify loops 
  loops_df_id <- find_loops(matrix, max_num_loops = 2000)
  
  # make individual feedback loop proportion data frame
  fl_proportion_mdd_in_ind <- data.frame(matrix(ncol=27, nrow=1,0))
  colnames(fl_proportion_mdd_in_ind) <- names
  
  # identify count per item 
  for (item in 1:27){
    for (row in 1:nrow(loops_df_id)){
      if (item %in% unlist(loops_df_id[row,1])){
        items_mdd_in_fl[item,2] <- items_mdd_in_fl[item,2] + 1
        fl_proportion_mdd_in_ind[1,item] <- fl_proportion_mdd_in_ind[1,item] + 1
      }
    }
  }
  
  for (item in 1:27){
    for (row in 1:nrow(loops_df_id)){
      if (item %in% unlist(loops_df_id[row,1])){
        items_mdd_in_fl[item,3] <- items_mdd_in_fl[item,3] + 1
      }
      break
    }
  }
  
  # divide number of feedback loops per item by total number of individual feedback loops 
  for (column in 1:27){
    if (fl_proportion_mdd_in_ind[1,column] != 0){
      fl_proportion_mdd_in_ind[1,column] <- fl_proportion_mdd_in_ind[1,column] / nrow(loops_df_id)
    }
  }
  
  # change loop from list to character (to compare & count later)
  loops_df_id$loop <- as.character(loops_df_id$loop)
  loops_df_id$ID <- Id
  loops_df_id <- loops_df_id %>% select(ID, everything())
  
  # merge data into data frame 
  df_feedback_loops_mdd_in <- merge(df_feedback_loops_mdd_in, loops_df_id[,1:3], all = TRUE) 
  fl_proportion_mdd_in <- merge(fl_proportion_mdd_in, fl_proportion_mdd_in_ind, all = TRUE)
}

# add information to the individual item data frame 
items_mdd_in_fl$percentage_fl <- round((items_mdd_in_fl$n /nrow(df_feedback_loops_mdd_in%>% filter(!(loop == "NULL")))),2)
items_mdd_in_fl$item_names <- names
items_mdd_in_fl$perc_part <- round(items_mdd_in_fl$participants / df_symptoms_total$n,2)
items_mdd_in_fl$average_fl <- round(items_mdd_in_fl$n / items_mdd_in_fl$participants,2)
items_mdd_in_fl$average_fl[1] <- 0 # for unknown
items_mdd_in_fl$no_fl_person <- round(colSums(fl_proportion_mdd_in) / items_mdd_in_fl$participants,2)

items_mdd_in_fl_only <- items_mdd_in_fl %>% filter(item_names %in% mdd_in)

# count & percentage per loop length
df_fl_mdd_in_length_grouped <- df_feedback_loops_mdd_in%>% group_by(length) %>% count()
df_fl_mdd_in_length_grouped$percentage <- round(df_fl_mdd_in_length_grouped$n / nrow(df_feedback_loops_mdd_in%>% 
                                                                                     filter(!(loop == "NULL"))),2)

# filter most prominent loops (that were reported by more than 10 individuals across the sample)
df_fl_mdd_in_loop_grouped <- df_feedback_loops_mdd_in %>% group_by(loop) %>% count() %>% filter(n > 10)

##############################################################################################################
# 4) CENTRALITY
##############################################################################################################
# bar plot with centralities of only specified MDD and I symptoms 
df_centrality_mdd_in <- centrality %>% filter(Item %in% mdd_in[1:5])
df_centrality_mdd_in$Out_In_ratio[df_centrality_mdd_in$Out_In_ratio > 4] <- 4

# average out/in degree centrality ratio per symptom
centrality_symptoms <- df_centrality_mdd_in %>% group_by(Item) %>% count()
average_out_in_ratio_mdd_in <- ddply(df_centrality_mdd_in, .(Item), transform, sum_name = sum(Out_In_ratio))
average_cent_mdd_in <- round(unique(average_out_in_ratio_mdd_in$sum_name) / centrality_symptoms$n,2) # average out_in_ratio

df_average_cent_mdd_in <- data.frame(Item = mdd_in[1:5],
                                     Out_In_div = average_cent_mdd_in)

##############################################################################################################
# 5) GROUP PARTICIPANTS 
##############################################################################################################
df_mdd_in_groups <- data.frame(matrix(ncol = 4, nrow = 1, 0)) # empty data frame for DIRECT RELATIONSHIP 
colnames(df_mdd_in_groups) <- c("IN causing", "MDD causing", "bidirectional", "no relationship")
in_names <- c("Sleep problems", "Daytime resting") # insomnia symptoms
mdd_names <- c("Alone/sad", "Tired", "Bored") # MDD symptoms

df_mdd_in_loops <- data.frame(matrix(ncol = 6, nrow = 0)) # empty data frame for INDIRECT RELATIONSHIP (loops)
colnames(df_mdd_in_loops) <- c("ID", "IN", "MDD", "both", "no", "group")

net_mdd <- net_in <- net_both <- net_no <- matrix(nrow = 27, ncol = 27, 0)

for (Id in descriptive_data_TRT$ID){
  participant_network <- network_data_TRT %>% filter(ID == Id) 
  
  # take networks that have symptoms of both disorders
  if (all(is.na(as.numeric(participant_network[c(4,5),3]))) == FALSE & 
      all(is.na(as.numeric(participant_network[c(23,24,26),3]))) == FALSE){
    participant_network[is.na(participant_network)] <- 0 # make all NA 0
    participant_matrix_complete <- data.matrix(participant_network[,4:ncol(participant_network)]) # transform data frame to matrix
    participant_matrix_complete <- t(participant_matrix_complete) # transpose matrix
    
    # make subset matrix including only MDD & IN symptoms 
    participant_matrix <- participant_matrix_complete[c(4,5,23,24,26),c(4,5,23,24,26)]
    colnames(participant_matrix) <- rownames(participant_matrix) <- c("Sleep problems", "Daytime resting", 
                                                                      "Alone/sad", "Tired", "Bored")
    
    # make 4 boolean (MDD, IN, bi, no)
    mdd <- insom <- bi <- no <- FALSE
    group <- ""
    
    # make subset nets
    matrix_ins_causing <- participant_matrix[c("Sleep problems", "Daytime resting"), c("Alone/sad", "Tired", "Bored")]
    matrix_mdd_causing <- participant_matrix[c("Alone/sad", "Tired", "Bored"), c("Sleep problems", "Daytime resting")]
    
    if (all(matrix_ins_causing == 0) == FALSE & all(matrix_mdd_causing == 0) == FALSE){
      bi <- TRUE
      group <- "bi"
      df_mdd_in_groups[1,3] <- df_mdd_in_groups[1,3] + 1
      net_both <- net_both + participant_matrix_complete
    } else if (all(matrix_ins_causing == 0) == FALSE & all(matrix_mdd_causing == 0) == TRUE){
      insom <- TRUE
      group <- "in"
      df_mdd_in_groups[1,1] <- df_mdd_in_groups[1,1] + 1
      net_in <- net_in + participant_matrix_complete
    } else if (all(matrix_ins_causing == 0) == TRUE & all(matrix_mdd_causing == 0) == FALSE){
      mdd <- TRUE
      group <- "mdd"
      df_mdd_in_groups[1,2] <- df_mdd_in_groups[1,2] + 1
      net_mdd <- net_mdd + participant_matrix_complete
    } else if (all(matrix_ins_causing == 0) == TRUE & all(matrix_mdd_causing == 0) == TRUE){
      no <- TRUE
      group <- "no"
      df_mdd_in_groups[1,4] <- df_mdd_in_groups[1,4] + 1
      net_no <- net_no + participant_matrix_complete
    }
    
    # include group info in descriptive data TRT 
    descriptive_data_TRT$mdd_in_group[descriptive_data_TRT$ID == Id] <- group
    
    # calculate loops 
    participant_loops <- find_loops(participant_matrix_complete, max_num_loops = 2000)
    loop_in <- loop_mdd <- loop_bi <- loop_no <- 0
    
    # for each loop count whether loop contains only MDD, only INSOMNIA, both or no symptoms - count variables up 
    if (nrow(participant_loops) == 0){
      print("no fl")
    }
    
    for (row in 1:nrow(participant_loops)){
      if ((grepl("4",as.character(participant_loops$loop[row])) | (grepl("5",as.character(participant_loops$loop[row]))))){
        if ((grepl("23",as.character(participant_loops$loop[row])) | (grepl("24",as.character(participant_loops$loop[row]))) | (grepl("26",as.character(participant_loops$loop[row]))))){
          loop_bi <- loop_bi + 1
        } else {
          loop_in <- loop_in + 1
        }
      } else if ((grepl("23",as.character(participant_loops$loop[row])) | (grepl("24",as.character(participant_loops$loop[row]))) | (grepl("26",as.character(participant_loops$loop[row]))))){
        loop_mdd <- loop_mdd + 1
      } else {
        loop_no <- loop_no + 1
      }
    }
    
    # make data frame for participant containing the count variables 
    total <- loop_in + loop_mdd + loop_bi + loop_no
    df_participant_loop <- data.frame("ID" = Id,
                                      "IN" =  round(loop_in / total, 2),
                                      "MDD" = round(loop_mdd / total,2),
                                      "both" = round(loop_bi / total,2), 
                                      "no" = round(loop_no/ total, 2),
                                      "group" = group)
    
    # merge participant data frame with overall data frame  
    df_mdd_in_loops <- merge(df_mdd_in_loops, df_participant_loop, all = TRUE) 
  }
}

round((df_mdd_in_groups[1,]/157)*100,2)

# calculate percentage of feedback loops containing symptoms of both disorders
round(nrow(df_mdd_in_loops %>% filter(group == "mdd") %>% filter(both > 0)) / nrow(df_mdd_in_loops %>% filter(group == "mdd")),4)*100
round(nrow(df_mdd_in_loops %>% filter(group == "in") %>% filter(both > 0)) / nrow(df_mdd_in_loops %>% filter(group == "in")),4)*100
round(nrow(df_mdd_in_loops %>% filter(group == "no") %>% filter(both > 0)) / nrow(df_mdd_in_loops %>% filter(group == "no")),4)*100

# subset of data_TRT 
subset_mdd <- descriptive_data_TRT %>% filter(mdd_in_group == "mdd")
subset_in <- descriptive_data_TRT %>% filter(mdd_in_group == "in")
subset_bi <- descriptive_data_TRT %>% filter(mdd_in_group == "bi")
subset_no <- descriptive_data_TRT %>% filter(mdd_in_group == "no")

##############################################################################################################
# CO-OCCURRENCE MATRICES 
##############################################################################################################
# make co-occurrence matrix for each mdd-insomnia group 
occurrences_mdd <- matrix(0, nrow = 6, ncol = 6)
occurrences_in <- matrix(0, nrow = 6, ncol = 6)
occurrences_bi <- matrix(0, nrow = 6, ncol = 6)
occurrences_no <- matrix(0, nrow = 6, ncol = 6)
rownames(occurrences_mdd) <- colnames(occurrences_mdd) <- mdd_in
rownames(occurrences_in) <- colnames(occurrences_in) <- mdd_in
rownames(occurrences_bi) <- colnames(occurrences_bi) <- mdd_in
rownames(occurrences_no) <- colnames(occurrences_no) <- mdd_in

for (i in 1:5){
  s1 <- mdd_in[i]
  for (j in (i+1):6){
    s2 <- mdd_in[j]
    for (id in descriptive_data_TRT$ID){
      subset <- network_data_TRT %>% filter(ID == id) %>% filter(Presence == 1)
      if(s1 %in% subset$Item & s2 %in% subset$Item){
        if (!is.na(descriptive_data_TRT$mdd_in_group[descriptive_data_TRT$ID == id])){
          if (descriptive_data_TRT$mdd_in_group[descriptive_data_TRT$ID == id] == "mdd"){
            occurrences_mdd[i,j] <- occurrences_mdd[i,j] + 1
            occurrences_mdd[j,i] <- occurrences_mdd[j,i] + 1
          } else if (descriptive_data_TRT$mdd_in_group[descriptive_data_TRT$ID == id] == "in"){
            occurrences_in[i,j] <- occurrences_in[i,j] + 1
            occurrences_in[j,i] <- occurrences_in[j,i] + 1
          } else if (descriptive_data_TRT$mdd_in_group[descriptive_data_TRT$ID == id] == "bi"){
            occurrences_bi[i,j] <- occurrences_bi[i,j] + 1
            occurrences_bi[j,i] <- occurrences_bi[j,i] + 1
          } else if (descriptive_data_TRT$mdd_in_group[descriptive_data_TRT$ID == id] == "no"){
            occurrences_no[i,j] <- occurrences_no[i,j] + 1
            occurrences_no[j,i] <- occurrences_no[j,i] + 1
          }
        }
      }
    }
  }
}

##############################################################################################################
# SYMPTOM LINK MATRICES
##############################################################################################################
# make symptom link presence and weight matrices for each mdd-insomnia group 
symptom_links_count_mdd <- matrix(0, nrow = 6, ncol = 6)
symptom_links_count_in <- matrix(0, nrow = 6, ncol = 6)
symptom_links_count_bi <- matrix(0, nrow = 6, ncol = 6)
symptom_links_count_no <- matrix(0, nrow = 6, ncol = 6)
rownames(symptom_links_count_mdd) <- colnames(symptom_links_count_mdd) <- mdd_in
rownames(symptom_links_count_in) <- colnames(symptom_links_count_in) <- mdd_in
rownames(symptom_links_count_bi) <- colnames(symptom_links_count_bi) <- mdd_in
rownames(symptom_links_count_no) <- colnames(symptom_links_count_no) <- mdd_in

# edge weight matrix
symptom_links_weights_mdd <- matrix(0, nrow = 6, ncol = 6)
symptom_links_weights_in <- matrix(0, nrow = 6, ncol = 6)
symptom_links_weights_bi <- matrix(0, nrow = 6, ncol = 6)
symptom_links_weights_no <- matrix(0, nrow = 6, ncol = 6)
rownames(symptom_links_weights_mdd) <- colnames(symptom_links_weights_mdd) <- mdd_in
rownames(symptom_links_weights_in) <- colnames(symptom_links_weights_in) <- mdd_in
rownames(symptom_links_weights_bi) <- colnames(symptom_links_weights_bi) <- mdd_in
rownames(symptom_links_weights_no) <- colnames(symptom_links_weights_no) <- mdd_in

# MDD GROUP
present_symptoms_mdd <- network_data_TRT %>% filter(ID %in% subset_mdd$ID) %>% filter(Presence == 1) %>% 
  filter(Item %in% mdd_in) %>% select(Item, all_of(mdd_in))

for (symptom in mdd_in){
  subset <- present_symptoms_mdd %>% filter(Item == symptom)
  subset[is.na(subset)] <- 0
  for (column in 2:ncol(subset)){
    symptom_links_count_mdd[mdd_in[column - 1], symptom] <- length(which(subset[,column] != 0))
    symptom_links_weights_mdd[mdd_in[column - 1], symptom] <- sum(subset[,column])
  }
}

# plot number of symptom pairs (relative)
symptom_links_count_mdd_relative <- round((symptom_links_count_mdd / occurrences_mdd),2)
symptom_links_count_mdd_relative[is.na(symptom_links_count_mdd_relative)] <- 0

# plot relative edge weight matrix (over all individuals which have specific edge)
symptom_links_weights_mdd_relative <- round((symptom_links_weights_mdd / symptom_links_count_mdd),2)
symptom_links_weights_mdd_relative[is.na(symptom_links_weights_mdd_relative)] <- 0

# INSOMNIA GROUP
present_symptoms_in <- network_data_TRT %>% filter(ID %in% subset_in$ID) %>% filter(Presence == 1) %>% 
  filter(Item %in% mdd_in) %>% select(Item, all_of(mdd_in))

for (symptom in mdd_in){
  subset <- present_symptoms_in %>% filter(Item == symptom)
  subset[is.na(subset)] <- 0
  for (column in 2:ncol(subset)){
    symptom_links_count_in[mdd_in[column - 1], symptom] <- length(which(subset[,column] != 0))
    symptom_links_weights_in[mdd_in[column - 1], symptom] <- sum(subset[,column])
  }
}

# plot number of symptom pairs (relative)
symptom_links_count_in_relative <- round((symptom_links_count_in / occurrences_in),2)
symptom_links_count_in_relative[is.na(symptom_links_count_in_relative)] <- 0

# plot relative edge weight matrix (over all individuals which have specific edge)
symptom_links_weights_in_relative <- round((symptom_links_weights_in / symptom_links_count_in),2)
symptom_links_weights_in_relative[is.na(symptom_links_weights_in_relative)] <- 0

# BIDIRECTIONAL GROUP
present_symptoms_bi <- network_data_TRT %>% filter(ID %in% subset_bi$ID) %>% filter(Presence == 1) %>% 
  filter(Item %in% mdd_in) %>% select(Item, all_of(mdd_in))

for (symptom in mdd_in){
  subset <- present_symptoms_bi %>% filter(Item == symptom)
  subset[is.na(subset)] <- 0
  for (column in 2:ncol(subset)){
    symptom_links_count_bi[mdd_in[column - 1], symptom] <- length(which(subset[,column] != 0))
    symptom_links_weights_bi[mdd_in[column - 1], symptom] <- sum(subset[,column])
  }
}

# plot number of symptom pairs (relative)
symptom_links_count_bi_relative <- round((symptom_links_count_bi / occurrences_bi),2)
symptom_links_count_bi_relative[is.na(symptom_links_count_bi_relative)] <- 0

# plot relative edge weight matrix (over all individuals which have specific edge)
symptom_links_weights_bi_relative <- round((symptom_links_weights_bi / symptom_links_count_bi),2)
symptom_links_weights_bi_relative[is.na(symptom_links_weights_bi_relative)] <- 0

# NO CONNECTION GROUP
present_symptoms_no <- network_data_TRT %>% filter(ID %in% subset_no$ID) %>% filter(Presence == 1) %>% 
  filter(Item %in% mdd_in) %>% select(Item, all_of(mdd_in))

for (symptom in mdd_in){
  subset <- present_symptoms_no %>% filter(Item == symptom)
  subset[is.na(subset)] <- 0
  for (column in 2:ncol(subset)){
    symptom_links_count_no[mdd_in[column - 1], symptom] <- length(which(subset[,column] != 0))
    symptom_links_weights_no[mdd_in[column - 1], symptom] <- sum(subset[,column])
  }
}

# plot number of symptom pairs (relative)
symptom_links_count_no_relative <- round((symptom_links_count_no / occurrences_no),2)
symptom_links_count_no_relative[is.na(symptom_links_count_no_relative)] <- 0

# plot relative edge weight matrix (over all individuals which have specific edge)
symptom_links_weights_no_relative <- round((symptom_links_weights_no / symptom_links_count_no),2)
symptom_links_weights_no_relative[is.na(symptom_links_weights_no_relative)] <- 0

##############################################################################################################
# CENTRALITY
##############################################################################################################
# MDD
centrality_mdd <- df_centrality_mdd_in %>% filter(ID %in% subset_mdd$ID)
centrality_symptoms_mdd <- centrality_mdd %>% group_by(Item) %>% count()
average_out_in_ratio_mdd <- ddply(centrality_mdd, .(Item), transform, sum_name = sum(Out_In_ratio))
average_cent_mdd <- round(unique(average_out_in_ratio_mdd$sum_name) / centrality_symptoms_mdd$n,2) # average out_in_ratio

df_average_cent_mdd <- data.frame(Item = mdd_in[1:5], Out_In_div = average_cent_mdd)

# INSOMNIA 
centrality_in <- df_centrality_mdd_in %>% filter(ID %in% subset_in$ID)
centrality_symptoms_in <- centrality_in %>% group_by(Item) %>% count()
average_out_in_ratio_in <- ddply(centrality_in, .(Item), transform, sum_name = sum(Out_In_ratio))
average_cent_in <- round(unique(average_out_in_ratio_in$sum_name) / centrality_symptoms_in$n,2) # average out_in_ratio

df_average_cent_in <- data.frame(Item = mdd_in[1:5], Out_In_div = average_cent_in)

# BIDIRECTIONAL 
centrality_bi <- df_centrality_mdd_in %>% filter(ID %in% subset_bi$ID)
centrality_symptoms_bi <- centrality_bi %>% group_by(Item) %>% count()
average_out_in_ratio_bi <- ddply(centrality_bi, .(Item), transform, sum_name = sum(Out_In_ratio))
average_cent_bi <- round(unique(average_out_in_ratio_bi$sum_name) / centrality_symptoms_bi$n,2) # average out_in_ratio

df_average_cent_bi <- data.frame(Item = mdd_in[1:5], Out_In_div = average_cent_bi)

# NO CONNECTION
centrality_no <- df_centrality_mdd_in %>% filter(ID %in% subset_no$ID)
centrality_symptoms_no <- centrality_no %>% group_by(Item) %>% count()
average_out_in_ratio_no <- ddply(centrality_no, .(Item), transform, sum_name = sum(Out_In_ratio))
average_cent_no <- round(unique(average_out_in_ratio_no$sum_name) / centrality_symptoms_no$n,2) # average out_in_ratio

df_average_cent_no <- data.frame(Item = mdd_in[1:5], Out_In_div = average_cent_no)
