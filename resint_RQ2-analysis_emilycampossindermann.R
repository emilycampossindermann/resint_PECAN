###############################################################################################################
# RQ2 - Analysis
#
# This is the analysis code for research question 2. It is divided into the following sections: 
# 1) Calculate number of feedback loops between 3 and 5 edges & density > include them in data frame 
# 2) Compute correlations between the indicator variables (feedback loops & density) with number of symptoms 
# 3) Compute correlations between the indicator variables (feedback loops & density) with severity
# 4) Compute correlations between the indicator variables (feedback loops & density) with duration
# 5) Compute two linear models (one for each outcome variable - severity & duration)
###############################################################################################################

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

##############################################################################################################
# 1) COMPUTE & INCLUDE NUMBER OF FEEDBACK LOOPS AND DENSITY IN DATA
##############################################################################################################
for (Id in descriptive_data_TRT$ID){
  # build individual matrix
  subset_id <- network_data_TRT %>% filter(ID == Id) %>% filter(Presence == 1) %>% filter(Item != "Unknown")
  item_list <- subset_id$Item
  subset_id <- subset_id %>% select(item_list)
  rownames(subset_id) <- item_list
  subset_id[is.na(subset_id)] <- 0
  
  matrix <- data.matrix(subset_id) # transform data frame to matrix
  matrix <- t(matrix)
  
  # network density n*3
  descriptive_data_TRT$network_density_new[which(descriptive_data_TRT$ID == Id)] <- 
    round(length(matrix[matrix > 0]) / (3*descriptive_data_TRT$Prio.Problems[descriptive_data_TRT$ID == Id]),2)
  
  # identify loops containing between 3 and 5 edges 
  loops <- find_loops(matrix, max_num_loops = 2000)
  loops_rerun <- loops %>% filter(length %in% c(3:5)) %>% count()
  descriptive_data_TRT$feedback_loops_rerun[which(descriptive_data_TRT$ID == Id)] <- loops_rerun[1,1]
}

# make subset for outcome variable severity 
df_severity <- descriptive_data_TRT %>% 
  select(ID, PHQ.sum, feedback_loops_rerun, network_density_new, Prio.Problems)

# make subset for outcome variable duration (all participants that identified as depressed)
df_duration <- descriptive_data_TRT %>% 
  filter(is.na(Depp.dura) == FALSE) %>% 
  select(ID, Depp.dura, feedback_loops_rerun, network_density_new, Prio.Problems)

##############################################################################################################
# CORRELATIONS
##############################################################################################################
# correlation feedback loops - severity + plot
shapiro.test(df_severity$PHQ.sum) # SEVERITY - data not normally distributed
shapiro.test(df_severity$feedback_loops_rerun) # FEEDBACK LOOPS - data not normally distributed
shapiro.test(df_severity$network_density_new) # DENSITY - data not normally distributed
shapiro.test(df_severity$Prio.Problems) # NUMBER OF SELECTED SYMPTOMS - data not normally distributed

# correlation feedback loops - duration + plot
shapiro.test(df_duration$Depp.dura) # DURATION - not normally distributed
shapiro.test(df_duration$feedback_loops_rerun) # FEEDBACK LOOPS - not normally distributed
shapiro.test(df_duration$network_density_new) # DENSITY - data not normally distributed

##############################################################################################################
# LINEAR MODELS
##############################################################################################################
# indicator variables: (1) feedback loops, (2) density
# outcome variable: depression severity 
severity_fit <- lm(cbind(df_severity$feedback_loops_rerun, df_severity$network_density_new) ~ df_severity$PHQ.sum) 
summary(severity_fit)

severity_fit_res <- resid(severity_fit)
hist(severity_fit_res[,1])
hist(severity_fit_res[,2])

# indicator variables: (1) feedback loops, (2) density
# outcome variable: depression duration 
duration_fit <- lm(cbind(df_duration$feedback_loops_rerun, df_duration$network_density_new) ~ df_duration$Depp.dura) 
summary(duration_fit)

duration_fit_res <- resid(duration_fit)
hist(duration_fit_res[,1])
hist(duration_fit_res[,2])
