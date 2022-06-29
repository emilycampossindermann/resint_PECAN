###############################################################################################################
# RQ2 - Plots & Visualizations
#
# This is the visualizations code for research question 2. It contains the correlation plots for:
# 1)a) Spearman correlation: Number of feedback loops - Number of selected symptoms
# 1)b) Spearman correlation: Network density - Number of selected symptoms
# 2)a) Spearman correlation: Number of feedback loops - Depression severity
# 2)b) Spearman correlation: Network density - Depression severity
# 3)a) Spearman correlation: Number of feedback loops - Depression duration
# 3)b) Spearman correlation: Network density - Depression duration
###############################################################################################################

# source RQ2 analysis script
source("/home/emily/Desktop/UvA/Research Internship/Data/resint_analysis/resint_RQ2-analysis_emilycampossindermann.R")

##############################################################################################################
# 1) NUMBER OF SELECTED SYMPTOMS
##############################################################################################################
# a) Number of feedback loops 
ggscatter(df_severity, x = "feedback_loops_rerun", y  = "Prio.Problems",
          add = "reg.line", conf.int = TRUE, 
          add.params = list(color = "deepskyblue3", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number of feedback loops", ylab = "Number of selected symptoms")

# b) Network density (NEW)
ggscatter(df_severity, x = "network_density_new", y = "Prio.Problems", 
          add = "reg.line", conf.int = TRUE, 
          add.params = list(color = "deepskyblue3", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Network density", ylab = "Number of selected symptoms")

##############################################################################################################
# 2) DEPRESSION SEVERITY (PHQ-9 SCORE)
##############################################################################################################
# a) Number of feedback loops
ggscatter(df_severity, x = "feedback_loops_rerun", y = "PHQ.sum",  
          add = "reg.line", conf.int = TRUE, 
          add.params = list(color = "deepskyblue3", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number of feedback loops", ylab = "Severity")

# b) Network density (NEW)
ggscatter(df_severity, x = "network_density_new", y = "PHQ.sum", 
          add = "reg.line", conf.int = TRUE, 
          add.params = list(color = "deepskyblue3", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Network density", ylab = "Severity")

##############################################################################################################
# 2) DEPRESSION CHRONICITY
##############################################################################################################
# a) Number of feedback loops
ggscatter(df_duration, x = "feedback_loops_rerun", y = "Depp.dura",
          add = "reg.line", conf.int = TRUE, 
          add.params = list(color = "deepskyblue3", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman", 
          xlab = "Number of feedback loops", ylab = "Chronicity")

# b) Network density (NEW)
ggscatter(df_duration, x = "network_density_new", y = "Depp.dura",
          add = "reg.line", conf.int = TRUE, 
          add.params = list(color = "deepskyblue3", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman", 
          xlab = "Network density", ylab = "Chronicity")

