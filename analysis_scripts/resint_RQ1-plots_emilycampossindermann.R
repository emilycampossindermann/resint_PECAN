###############################################################################################################
# RQ1 - Plots & Visualizations
#
# This is the visualizations code for research question 1. It is divided into the following sections: 
# 1) Symptom profiles (constellation, clustering using TSNE)
# 2) Symptom relations (co-occurrence matrices, edge count matrices, edge weight matrices)
# 3) Feedback loops 
# 4) Centrality 
# 5) Individual network comparison of two participants u
# 6) Group-level sub-network for comparison with Malgaroli et al., (2021)
###############################################################################################################

# source RQ1 analysis script
source("/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/resint_RQ1-analysis_emilycampossindermann.R")

##############################################################################################################
# 1) SYMPTOM PROFILES
##############################################################################################################
# SYMPTOM PROFILE SIZES: count length of symptom profiles (corresponding percentage on top)
ggplot(symptoms_count, aes(x = Prio.Problems, y = n)) + 
  geom_bar(stat = "identity",  width=0.7, fill="steelblue") +
  labs(y= "Count", x = "Number of selected symptoms") +
  geom_text(aes(label = percentage), vjust = -0.7) + 
  theme_minimal()

# CLUSTERING: tsne plot 
tSNE_df %>% ggplot(aes(x = tSNE1, y = tSNE2))+
  geom_point()+
  theme_minimal()

##############################################################################################################
# 2) SYMPTOM RELATIONS
##############################################################################################################
# COMMUNITY DETECTION: plot communities per symptom
ggplot(community_plot, aes(fill=as.factor(infomap), y=n, x=Item)) + 
  geom_col(position=position_dodge(width=0.8)) +
  labs(y= "Count", x = "") +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_discrete(name="Community")+
  geom_text(aes(label = percentage),position = position_dodge(0.9), vjust = 0)+
  theme_minimal()

# COMMUNITY DETECTION
ggplot(df_com_number, aes(x = Communities, y = n)) +  # Plot with values on top
  geom_bar(stat = "identity", fill="steelblue") +
  labs(y= "Count", x = "Communities") +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  geom_text(aes(label = n), vjust = -0.7) + 
  theme_minimal()

# CO-OCURRENCES COUNT: correlation plot (including unknown) (NOT USED IN REPORT !!)
ggcorrplot(round(symptoms_co_occurrences,1), lab = TRUE)

# CO-OCCURRENCES COUNT: heatmap (including unknown) (NOT USED IN REPORT !!)
pheatmap(symptoms_co_occurrences, number_format = "%.0f",display_numbers = T)

# CO-OCURRENCES COUNT: correlation plot (excluding unknown) (NOT USED IN REPORT !!)
ggcorrplot(round(symptoms_co_occurrences[2:27,2:27],1), lab = TRUE)

# CO-OCURRENCES COUNT: heatmap (excluding unknown) (NOT USED IN REPORT !!)
pheatmap(symptoms_co_occurrences[2:27,2:27], number_format = "%.0f",display_numbers = T)

# SYMPTOM OCCURRENCE: plot occurrenrce of each symptom separately (with percentages on top)
ggplot(symptoms_count_each[2:27,], aes(x = reorder(Item, -percentage), y = n)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  labs(y= "Count", x = "") +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  geom_text(aes(label = percentage), vjust = -0.7) + 
  theme_minimal()

# CO-OCCURRENCES RELATIVE: heatmap (including unknown)
pheatmap(symptoms_co_occurrences_relative[2:27,2:27], number_format = "%.2f",display_numbers = T,cluster_rows=F, cluster_cols=F)

# SYMPTOM PAIRS: plot number of symptom pairs (NOT USED IN REPORT !!)
pheatmap(symptom_links_presence, cluster_cols = F, cluster_rows = F, number_format = "%.0f",display_numbers = T)

# SYMPTOM PAIRS: plot number of symptom pairs (relative)
pheatmap(symptom_links_presence_relative, cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T)

# SYMPTOM EDGES: average edge weight matrix (over all individuals) (NOT USED IN REPORT !!)
pheatmap(symptom_links_weights_average, cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T)

# SYMPTOM EDGES: relative edge weight matrix (over all individuals which have both symptoms) (NOT USED IN REPORT !!)
pheatmap(symptom_links_weights_relative, cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T)

# SYMPTOM EDGES: relative edge weight matrix (over all individuals which have specific edge)
pheatmap(symptom_links_weights_relative_edge, cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T)

# WEIGHTED LINKS: plot strongest edges (NOT USED IN REPORT !!)
ggplot(strongest_edges, aes(x=factor(max)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  labs(y= "Count", x = "Maximum edge strength") +
  theme_minimal()

ggplot(strongest_edges_max_counts, aes(x = max, y = n)) + 
  geom_bar(stat = "identity",fill="steelblue") +
  labs(y= "Count", x = "Maximum edge strength") +
  geom_text(aes(label = percentage), vjust = 0)+
  theme_minimal()

##############################################################################################################
# 3) FEEDBACK LOOPS
##############################################################################################################
df_feedback_loops_long <- items_feedback_loops[2:27,] %>% pivot_longer(c('perc_part', 'no_fl_person'), names_to='variable', 
                                                                       values_to="value")

# SYMPTOM PRESENCE & CORRESPONDING PROPORTION: plot proportion of individuals who reported symptom in feedback loop
# alongside the average proportion of feedback loops within the individuals that included the symptom (NOT USED IN REPORT !!)
ggplot(df_feedback_loops_long, aes(x=item_names, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(y="Proportion", x= " ") +
  theme_minimal()

# INDIVIDUAL SYMPTOMS: plot individual items involved in feedback loops 
ggplot(items_feedback_loops, aes(x = reorder(item_names, -perc_part), y = average_fl)) +  
  geom_bar(stat = "identity", fill="steelblue") +
  labs(y= "Average number of feedback loops including the symptom per individual", x = "") +
  geom_text(aes(label = perc_part), vjust = -0.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme_minimal()

# FEEDBACK LOOP LENGTH: plot number of all feedback loops (NOT USED IN REPORT !!)
ggplot(df_feedback_length_grouped, aes(x = length, y = n)) +  
  geom_bar(stat = "identity", fill="steelblue") +
  labs(y= "Number of feedback loops", x = "Number of edges") +
  geom_text(aes(label = percentage), vjust = -0.7) + 
  theme_minimal()

##############################################################################################################
# 4) CENTRALITY
##############################################################################################################
# BOXPLOT: plot out-/to in-degree centrality ratio for each symptom 
ggplot(centrality_depressed, aes(x=Item, y=Out_In_ratio, color=Item)) +
  geom_boxplot(color="black") +
  geom_jitter(position=position_dodge(0.75)) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("") + 
  xlab("") + 
  ylab("Out-/In-degree centrality") +
  theme_minimal()

# create a palette function 
pal <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_average_out_in_ratio$out_in), min(df_average_out_in_ratio$out_in)))
df_average_out_in_ratio$hex_from_scales_Out_In <- pal(df_average_out_in_ratio$out_in)

# order
df_average_out_in_ratio_ordered <- df_average_out_in_ratio[match(names[2:17], df_average_out_in_ratio$Item),]

# SUMMARY NETWORK 1: edge width corresponds to average presence of links
summary_network01 <- qgraph(symptom_links_presence_relative[2:27,2:27], 
                            vsize = symptoms_count_each[2:27,]$scaled, 
                            shape = "circle",
                            layout = "spring",
                            theme = "colorblind",
                            color = df_average_out_in_ratio_ordered$hex_from_scales_Out_In,
                            negDashed=TRUE, 
                            colFactor = 1,
                            labels = c("eatless", "noex", "sleeppro", "dayrest", "confl", "hypoc", "troubco", "socmed", "stayh", 
                                       "procrast", "subst", "selfh", "suicid", "eatmore", "compul", "rumin", "worry", "flashb", 
                                       "panic", "pain", "socanx", "sad", "tired", "stress", "bored", "angry"),
                            minimum = 0.5, 
                            pie = symptom_links_presence_relative[1,2:27], 
                            pieColor = "yellow",
                            pieColor2 = "gray88",
                            legend.cex = 0.4,
                            GLratio = 2.5,
                            layoutScale = c(1, 1),
                            layoutOffset = c(0,0),
                            nodeNames = names[2:27]
)

# SUMMARY NETWORK 2: edge width corresponds to average strengths of links 
summary_network02 <- qgraph(symptom_links_weights_relative_edge[2:27,2:27], 
                            vsize = symptoms_count_each[2:27,]$scaled, 
                            shape = "circle", 
                            layout = "spring",
                            theme = "colorblind",
                            color = df_average_out_in_ratio_ordered$hex_from_scales_Out_In,
                            negDashed=TRUE, 
                            colFactor = 1,
                            labels = c("eatless", "noex", "sleeppro", "dayrest", "confl", "hypoc", "troubco", "socmed", "stayh", 
                                       "procrast", "subst", "selfh", "suicid", "eatmore", "compul", "rumin", "worry", "flashb", 
                                       "panic", "pain", "socanx", "sad", "tired", "stress", "bored", "angry"),
                            minimum = 50, 
                            pie = symptom_links_weights_relative_edge[1,2:27]/100, 
                            pieColor = "yellow",
                            pieColor2 = "gray88",
                            legend.cex = 0.4,
                            GLratio = 2.5,
                            layoutScale = c(1, 1),
                            layoutOffset = c(0,0),
                            nodeNames = names[2:27]
)

##############################################################################################################
# 5) INDIVIDUAL NETWORK COMPARISON
##############################################################################################################
# PARTICIPANT 180989: create a palette function 
pal_participant01 <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_participant01$Out_in), min(df_participant01$Out_in)))
df_participant01$hex_from_scales <- pal_participant01(df_participant01$Out_in)

# PARTICIPANT 180989: individual network visualization
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

# PARTICIPANT 200234: create palette function
pal_participant02 <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_participant02$Out_in), min(df_participant02$Out_in)))
df_participant02$hex_from_scales <- pal_participant02(df_participant02$Out_in)

# PARTICIPANT 200234: individual network visualization
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

##############################################################################################################
# 6) COMPARISON BETWEEN AVERAGE PECAN & GROUP-LEVEL MDD NETWORK 
##############################################################################################################
# AVERAGE UNDIRECTED NETWORK (NOT USED IN REPORT !!)
average_network_vis <- qgraph(average_network_undirected[c(2,4,8,14,15,23,24,26),c(2,4,8,14,15,23,24,26)], 
                              negDashed=TRUE, 
                              layout = "spring",
                              colFactor = 1,
                              color = c("deepskyblue4","deepskyblue4","deepskyblue4","firebrick3","deepskyblue4",
                                        "firebrick3","deepskyblue4","firebrick3"),
                              labels = c("eatless", "sleeppro", "troubco", "suicid", "eatmore", "sad", "tired", "bored"),
                              minimum = 0)

