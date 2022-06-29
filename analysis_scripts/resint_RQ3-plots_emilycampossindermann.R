###############################################################################################################
# RQ3 - Analysis
#
# This is the analysis code for research question 3. It is divided into the following sections: 
# 1) Symptom profiles (MDD & Insomnia occurrences within symptom profiles) 
# 2) Symptom relations (Community detection, MDD & Insomnia co-occurrences, MDD & Insomnia relations)
# 3) Centrality
# 4) Summary MDD-Insomnia group networks
# 5) Groups 
###############################################################################################################

# source RQ3 analysis script
source("/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/resint_RQ3-analysis_emilycampossindermann.R")

##############################################################################################################
# 1) MDD-INSOMNIA SYMPTOM PROFILES
##############################################################################################################
# symptom count & percentage over entire sample 
ggplot(df_symptoms_total, aes(x = reorder(Item, -percentage), y = n)) +  # Plot with values on top
  geom_bar(stat = "identity", fill=c("deepskyblue","steelblue", "deepskyblue",rep("steelblue",2),"blue4",
                                     rep("steelblue",10),"blue4",rep("steelblue",6),"deepskyblue",rep("steelblue",3)), width=0.7) +
  labs(y= "Count", x = "") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  geom_text(aes(label = percentage), vjust = -0.7)+
  theme_minimal()

# CO-OCCURRENCES
pheatmap(occurrences_mdd_in, number_format = "%.0f",display_numbers = T) # absolute including unknown
pheatmap(occurrences_mdd_in[1:5,1:5], number_format = "%.0f",display_numbers = T) # absolute excluding unknown

pheatmap(occurrences_mdd_in_relative, number_format = "%.2f",display_numbers = T,cluster_rows=F, cluster_cols=F) # relative
pheatmap(occurrences_mdd_in_relative[1:5,1:5], number_format = "%.2f",display_numbers = T,cluster_rows=F, cluster_cols=F) # without unknown

##############################################################################################################
# 2) MDD-INSOMNIA SYMPTOM RELATIONS
##############################################################################################################
# COMMUNITY DETECTION
ggplot(community_plot_mdd_in, aes(fill=as.factor(infomap), y=n, x=Item)) + 
  geom_col(position=position_dodge(width=0.9)) +
  labs(y= "Count", x = "") +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_discrete(name="Community")+
  geom_text(aes(label = percentage),position = position_dodge(0.9), vjust = -0.7)+
  theme_minimal()

# SYMPTOM LINKS 
# absolute number of symptom links 
pheatmap(symptom_links_count_mdd_in, cluster_cols = F, cluster_rows = F, number_format = "%.0f",display_numbers = T) # including unknown
pheatmap(symptom_links_count_mdd_in[1:5,1:5], cluster_cols = F, cluster_rows = F, number_format = "%.0f",display_numbers = T) # excluding unknown

# plot number of symptom pairs (relative)
pheatmap(symptom_links_count_mdd_in_relative, cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T) # including unknown
pheatmap(symptom_links_count_mdd_in_relative[1:5,1:5], cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T) # excluding unknown

# plot relative edge weight matrix (over all individuals which have specific edge)
pheatmap(symptom_links_weights_mdd_in_relative, cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T) # including unknown
pheatmap(symptom_links_weights_mdd_in_relative[1:5,1:5], cluster_cols = F, cluster_rows = F, number_format = "%.2f",display_numbers = T) # excluding unknown

##############################################################################################################
# 3) CENTRALITY
##############################################################################################################
# Out-degree per item 
ggplot(df_centrality_mdd_in, aes(x=Item, y=Outdegree, color=Item)) +
  geom_boxplot(color="black") +
  geom_jitter(position=position_dodge(0.75)) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("") + 
  xlab("") + 
  ylab("Centrality") +
  theme_minimal()

# In-degree per item
ggplot(df_centrality_mdd_in, aes(x=Item, y=Indegree, color=Item)) +
  geom_boxplot(color="black") +
  geom_jitter(position=position_dodge(0.75)) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("") + 
  xlab("") + 
  ylab("Centrality") +
  theme_minimal()

# Out/In-degree per item
ggplot(df_centrality_mdd_in, aes(x=Item, y=Out_In_ratio, color=Item)) +
  geom_boxplot(color="black") +
  geom_jitter(position=position_dodge(0.75)) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("") + 
  xlab("") + 
  ylab("Out-/In-degree centrality") +
  theme_minimal()

##############################################################################################################
# 4) SUMMARY MDD-INSOMNIA GROUP NETWORKS
##############################################################################################################
# create a palette function
pal_q3 <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_average_cent_mdd_in$Out_In_div), min(df_average_cent_mdd_in$Out_In_div)))
# get palette-generated values from your vector
df_average_cent_mdd_in$hex_from_scales <- pal_q3(df_average_cent_mdd_in$Out_In_div)

df_average_cent_mdd_in_ordered <-df_average_cent_mdd_in[match(mdd_in, df_average_cent_mdd_in$Item),]

df_mdd_in$scaled <- df_mdd_in$n * (6/132) +4

# THEORETICAL EDGE WEIGHT INFO IN EDGE COLOR (HOWEVER NOT USED IN REPORT)
pal_weights <- scales::gradient_n_pal(colours = c("orange","green"),values= c(max(symptom_links_weights_mdd_in_relative[1:5,1:5]), 50))
# get palette-generated values from your vector
edge_colors <- symptom_links_weights_mdd_in_relative[1:5,1:5]
edge_colors[edge_colors < 50] <- 0
edge_colors <- pal_weights(edge_colors)
edge_colors <- edge_colors[edge_colors != "#00FF00"]

# MDD-INSOMNIA SUMMARY NETWORK 1: edge width corresponds to average presence of links
summary_mdd_in_network01 <- qgraph(symptom_links_count_mdd_in_relative[1:5,1:5], 
                                   vsize = df_mdd_in$scaled[1:5],
                                   shape = "circle", 
                                   layout = "spring",
                                   theme = "colorblind",
                                   color = df_average_cent_mdd_in_ordered$hex_from_scales,
                                   colFactor = 1,
                                   labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                                   minimum = 0.5,
                                   pie = symptom_links_count_mdd_in_relative[6,1:5], 
                                   pieColor = "yellow",
                                   pieColor2 = "gray88",
                                   legend.cex = 0.4,
                                   GLratio = 2.5,
                                   layoutScale = c(1, 1),
                                   layoutOffset = c(0,0),
                                   nodeNames = c("Alone/sad", "Bored", "Daytime resting", "Sleep problems", "Tired")
                                   #edge.color = edge_colors
)

# MDD-INSOMNIA SUMMARY NETWORK 2: edge width corresponds to average strengths of links 
summary_mdd_in_network02 <- qgraph(symptom_links_weights_mdd_in_relative[1:5,1:5], 
                                   vsize = df_mdd_in$scaled[1:5],
                                   shape = "circle", 
                                   layout = summary_mdd_in_network01$layout,
                                   theme = "colorblind",
                                   color = df_average_cent_mdd_in_ordered$hex_from_scales,
                                   colFactor = 1,
                                   labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                                   minimum = 50,
                                   pie = symptom_links_weights_mdd_in_relative[6,1:5]/100, 
                                   pieColor = "yellow",
                                   pieColor2 = "gray88",
                                   legend.cex = 0.4,
                                   GLratio = 2.5,
                                   layoutScale = c(1, 1),
                                   layoutOffset = c(0,0),
                                   nodeNames = c("Alone/sad", "Bored", "Daytime resting", "Sleep problems", "Tired")
)

##############################################################################################################
# 5) GROUPS
##############################################################################################################

##############################################################################################################
# MDD causing insomnia group
##############################################################################################################
pal_q3_mdd <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_average_cent_mdd$Out_In_div), min(df_average_cent_mdd$Out_In_div)))

# get palette-generated values from your vector
df_average_cent_mdd$hex_from_scales <- pal_q3_mdd(df_average_cent_mdd$Out_In_div)

df_average_cent_mdd_ordered <-df_average_cent_mdd[match(mdd_in, df_average_cent_mdd$Item),]

influence_ukn_mdd <- symptom_links_weights_mdd_relative[6,1:5]/100

df_mdd <- network_data_TRT %>% filter(Item %in% mdd_in) %>% filter(Presence == 1) %>% filter(ID %in% subset_mdd$ID) %>% group_by(Item) %>% count()
df_mdd$scaled <- df_mdd$n /2.5

# MDD GROUP NETWORK 1: edge width corresponds to average presence of links
mdd_network01 <- qgraph(symptom_links_count_mdd_relative[1:5,1:5], 
                        vsize = df_mdd$scaled[1:5],
                        shape = "circle",
                        layout = "spring",
                        theme = "colorblind",
                        color = df_average_cent_mdd_ordered$hex_from_scales,
                        colFactor = 1,
                        labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                        minimum = 0.5,
                        pie = influence_ukn_mdd, 
                        pieColor = "yellow",
                        pieColor2 = "gray88",
                        legend.cex = 0.4,
                        GLratio = 2.5,
                        layoutScale = c(1, 1),
                        layoutOffset = c(0,0),
                        nodeNames = c("Alone/sad (MDD)", "Bored (MDD)", "Daytime resting (Insomnia)", "Sleep problems (Insomnia)", "Tired (MDD)")
)

# MDD GROUP NETWORK 2: edge width corresponds to average strengths of links  
mdd_network02 <- qgraph(symptom_links_weights_mdd_relative[1:5,1:5], 
                        vsize = df_mdd$scaled[1:5],
                        shape = "circle", 
                        layout = mdd_network01$layout,
                        theme = "colorblind",
                        color = df_average_cent_mdd_ordered$hex_from_scales,
                        colFactor = 1,
                        labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                        minimum = 50,
                        pie = symptom_links_weights_mdd_relative[6,1:5]/100, 
                        pieColor = "yellow",
                        pieColor2 = "gray88",
                        legend.cex = 0.4,
                        GLratio = 2.5,
                        layoutScale = c(1, 1),
                        layoutOffset = c(0,0),
                        nodeNames = c("Alone/sad (MDD)", "Bored (MDD)", "Daytime resting (Insomnia)", "Sleep problems (Insomnia)", "Tired (MDD)")
)

##############################################################################################################
# Insomnia causing MDD group
##############################################################################################################
pal_q3_in <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_average_cent_in$Out_In_div), min(df_average_cent_in$Out_In_div)))
# get palette-generated values from your vector
df_average_cent_in$hex_from_scales <- pal_q3_in(df_average_cent_in$Out_In_div)

df_average_cent_in_ordered <-df_average_cent_in[match(mdd_in, df_average_cent_in$Item),]

influence_ukn_in <- symptom_links_weights_in_relative[6,1:5]/100

df_in <- network_data_TRT %>% filter(Item %in% mdd_in) %>% filter(Presence == 1) %>% filter(ID %in% subset_in$ID) %>% group_by(Item) %>% count()
df_in$scaled <- df_in$n /2.5

# INSOMNIA GROUP NETWORK 1: edge width corresponds to average presence of links
insomnia_network01 <- qgraph(symptom_links_count_in_relative[1:5,1:5], 
                             vsize = df_in$scaled[1:5],
                             shape = "circle", 
                             layout = "spring",
                             theme = "colorblind",
                             color = df_average_cent_in_ordered$hex_from_scales,
                             colFactor = 1,
                             labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                             minimum = 0.5,
                             pie = influence_ukn_in, 
                             pieColor = "yellow",
                             pieColor2 = "gray88",
                             legend.cex = 0.4,
                             GLratio = 2.5,
                             layoutScale = c(1, 1),
                             layoutOffset = c(0,0),
                             nodeNames = c("Alone/sad (MDD)", "Bored (MDD)", "Daytime resting (Insomnia)", "Sleep problems (Insomnia)", "Tired (MDD)")
)

# INSOMNIA GROUP NETWORK 2: edge width corresponds to average strengths of links 
insomnia_network02 <- qgraph(symptom_links_weights_in_relative[1:5,1:5], 
                             vsize = df_in$scaled[1:5],
                             shape = "circle", 
                             layout = insomnia_network01$layout,
                             theme = "colorblind",
                             color = df_average_cent_in_ordered$hex_from_scales,
                             colFactor = 1,
                             labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                             minimum = 50,
                             pie = symptom_links_weights_in_relative[6,1:5]/100, 
                             pieColor = "yellow",
                             pieColor2 = "gray88",
                             legend.cex = 0.4,
                             GLratio = 2.5,
                             layoutScale = c(1, 1),
                             layoutOffset = c(0,0),
                             nodeNames = c("Alone/sad (MDD)", "Bored (MDD)", "Daytime resting (Insomnia)", "Sleep problems (Insomnia)", "Tired (MDD)")
)

##############################################################################################################
# Bidirectional relationship between MDD and insomnia 
##############################################################################################################
pal_q3_bi <- scales::gradient_n_pal(colours = c("red","blue"),values= c(max(df_average_cent_bi$Out_In_div), min(df_average_cent_bi$Out_In_div)))
# get palette-generated values from your vector
df_average_cent_bi$hex_from_scales <- pal_q3_bi(df_average_cent_bi$Out_In_div)

df_average_cent_bi_ordered <-df_average_cent_bi[match(mdd_in, df_average_cent_bi$Item),]

influence_ukn_bi <- symptom_links_weights_bi_relative[6,1:5]/100

df_bi <- network_data_TRT %>% filter(Item %in% mdd_in) %>% filter(Presence == 1) %>% filter(ID %in% subset_bi$ID) %>% group_by(Item) %>% count()
df_bi$scaled <- df_bi$n /2.5

# BIDIRECTIONAL GROUP NETWORK 1: edge width corresponds to average presence of links
bidirectional_network01 <- qgraph(symptom_links_count_bi_relative[1:5,1:5], 
                                  vsize = df_bi$scaled[1:5],
                                  shape = "circle", 
                                  layout = "spring",
                                  theme = "colorblind",
                                  color = df_average_cent_bi_ordered$hex_from_scales,
                                  colFactor = 1,
                                  labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                                  minimum = 0.5,
                                  pie = influence_ukn_bi, 
                                  pieColor = "yellow",
                                  pieColor2 = "gray88",
                                  legend.cex = 0.4,
                                  GLratio = 2.5,
                                  layoutScale = c(1, 1),
                                  layoutOffset = c(0,0),
                                  nodeNames = c("Alone/sad (MDD)", "Bored (MDD)", "Daytime resting (Insomnia)", "Sleep problems (Insomnia)", "Tired (MDD)")
)

# BIDIRECTIONAL GROUP NETWORK 2: edge width corresponds to average strengths of links 
bidirectional_network02 <- qgraph(symptom_links_weights_bi_relative[1:5,1:5], 
                                  vsize = df_bi$scaled[1:5],
                                  shape = "circle", 
                                  layout = bidirectional_network01$layout,
                                  theme = "colorblind",
                                  color = df_average_cent_bi_ordered$hex_from_scales,
                                  colFactor = 1,
                                  labels = c("sad", "bored", "dayrest", "sleeppro", "tired"),
                                  minimum = 50,
                                  pie = symptom_links_weights_bi_relative[6,1:5]/100, 
                                  pieColor = "yellow",
                                  pieColor2 = "gray88",
                                  legend.cex = 0.4,
                                  layoutScale = c(1, 1),
                                  layoutOffset = c(0,0),
                                  nodeNames = c("Alone/sad (MDD)", "Bored (MDD)", "Daytime resting (Insomnia)", "Sleep problems (Insomnia)", "Tired (MDD)")
)
