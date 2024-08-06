library(mvnormtest)
library(rstatix)
library(tidyverse)
setwd("~/Charite Thesis/CBX3 project/imaging self-branching neurons")

# Import

div7_div4_table <- read.table(
  "SNT_Measurements_DIV7_DIV4.csv",
  fileEncoding = 'latin1',
  sep = ";",
  header = TRUE
)

# Plot

df_long <- div7_div4_table %>% pivot_longer(cols = 3:last_col(),
                                            names_to = "Variable",
                                            values_to = "Value")
ggplot(df_long, aes(x = DIV, y = Value, color = DIV)) +
  geom_boxplot() +
  geom_jitter() +
  theme_minimal() +
  facet_grid(Variable ~ Condition, scales = 'free')

# Import

div7_table <- read.table(
  "SNT_Measurements_DIV7.csv",
  fileEncoding = 'latin1',
  sep = ";",
  header = TRUE
)

summary(div7_table)
div7_table_columns <- colnames(div7_table[, 2:11])

# Checking for outliers

outliers_ci <- identify_outliers(data = div7_table, Complexity_index)
outliers_ci <- outliers_ci[outliers_ci$is.extreme == TRUE, ]$Complexity_index
index_outliers <- which(div7_table$Complexity_index %in% outliers_ci)
div7_table <- div7_table[-c(index_outliers), ]

outliers_sholl <- identify_outliers(data = div7_table, Sholl_Ramification_index)
outliers_sholl <- outliers_sholl[outliers_sholl$is.extreme == TRUE, ]$Sholl_Ramification_index
index_outliers <- which(div7_table$Sholl_Ramification_index %in% outliers_sholl)
div7_table <- div7_table[-c(index_outliers), ]

outliers_path_max <- identify_outliers(data = div7_table, Path_length_Max)
outliers_path_max <- outliers_path_max[outliers_path_max$is.extreme == TRUE, ]$Path_length_Max
index_outliers <- which(div7_table$Path_length_Max %in% outliers_path_max)
div7_table <- div7_table[-c(index_outliers), ]

# Testing normality not ok

div7_table_matrix <- as.matrix(div7_table[, 2:11])
div7_table_matrix <- t(div7_table_matrix[1:56, 1:8])
mshapiro.test(div7_table_matrix)
lapply(div7_table[2:11], shapiro.test)

# We then use Kruskal-Wallis instead of ANOVA

# Plot

df_long_div7 <- div7_table %>% pivot_longer(cols = 2:last_col(),
                                            names_to = "Variable",
                                            values_to = "Value")
ggplot(df_long_div7, aes(x = Condition, y = Value, color = Condition)) +
  geom_boxplot() +
  geom_jitter() +
  theme_minimal() +
  facet_wrap(~ Variable, scales = 'free', ncol = 5)


# Kruskal-Wallis test

list.res.kruskal <- list()
i = 0
for (col in div7_table[, 2:11]) {
  i = i + 1
  list.res.kruskal[[div7_table_columns[i]]] <- div7_table %>% kruskal_test(col ~ Condition)
  print(div7_table_columns[i])
  print(list.res.kruskal[[div7_table_columns[i]]])
}

# Keep Path_length_Min, Path_length_Mean, Nb_primary_branches

list.res.kruskal["Path_length_Min"]
list.res.kruskal["Path_length_Mean"]
list.res.kruskal["Nb_primary_branches"]

# Pairwise comparisons

pwc_path_min <- div7_table %>%
  dunn_test(Path_length_Min ~ Condition, p.adjust.method = "bonferroni")
pwc_path_min

pwc_path_mean <- div7_table %>%
  dunn_test(Path_length_Mean ~ Condition, p.adjust.method = "bonferroni")
pwc_path_mean

pwc_nb_primary <- div7_table %>%
  dunn_test(Nb_primary_branches ~ Condition, p.adjust.method = "bonferroni")
pwc_path_mean



# Kruskal-Wallis for DIV4

# Import

div4_table <- read.table(
  "SNT_Measurements_DIV4.csv",
  fileEncoding = 'latin1',
  sep = ";",
  header = TRUE
)

summary(div4_table)
div4_table_columns <- colnames(div4_table[, 2:11])

# Checking for outliers

outliers_ci <- identify_outliers(data = div4_table, Complexity_index)
outliers_ci <- outliers_ci[outliers_ci$is.extreme == TRUE, ]$Complexity_index
index_outliers <- which(div4_table$Complexity_index %in% outliers_ci)
div4_table <- div4_table[-c(index_outliers), ]

# No extreme outliers
outliers_sholl <- identify_outliers(data = div4_table, Sholl_Ramification_index)
outliers_path_max <- identify_outliers(data = div4_table, Path_length_Max)


# Plot

df_long_div4 <- div4_table %>% pivot_longer(cols = 2:last_col(),
                                            names_to = "Variable",
                                            values_to = "Value")
ggplot(df_long_div4, aes(x = Condition, y = Value, color = Condition)) +
  geom_boxplot() +
  geom_jitter() +
  theme_minimal() +
  facet_wrap( ~ Variable, scales = 'free', ncol = 5)


# Kruskal-Wallis test

list.res.kruskal <- list()
i = 0
for (col in div4_table[, 2:11]) {
  i = i + 1
  list.res.kruskal[[div4_table_columns[i]]] <- div4_table %>% kruskal_test(col ~ Condition)
  print(div4_table_columns[i])
  print(list.res.kruskal[[div4_table_columns[i]]])
}

# No significant measurements
