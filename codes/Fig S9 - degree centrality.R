#degree centrality
data_table <- read.csv("~/Desktop/Cold spots stuffs/degree_centrality.csv", header=TRUE)

colnames(data_table)

# Boxplot
ggplot(data_table, aes(x = global_north_south, y = degree_centrality)) +
  geom_boxplot(aes(color = global_north_south), position = "dodge") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(size = 0.9),
        axis.line.y = element_line(size = 0.9),
        axis.text = element_text(size = 20),
        #axis.text.x = element_text(size = 20, hjust = 0.9),
        axis.title.x = element_text(size = 24, margin = margin(t = 10)),
        axis.title.y = element_text(size = 24, margin = margin(r = 10))) +
  scale_color_manual(values = c("#01431c", "#74c476", "grey")) +
  labs(
    x = "",
    y = "Degree Centrality")

#remove outliers

remove_outliers <- function(data, value_column) {
  data %>%
    group_by(global_north_south) %>%  # Group by global_north_south
    filter(
      .data[[value_column]] > quantile(.data[[value_column]], 0.25, na.rm = TRUE) - 1.5 * IQR(.data[[value_column]], na.rm = TRUE) &
        .data[[value_column]] < quantile(.data[[value_column]], 0.75, na.rm = TRUE) + 1.5 * IQR(.data[[value_column]], na.rm = TRUE)
    ) %>%
    ungroup()  # Remove the grouping
}

# Remove outliers in the degree_centrality column
data_table_no_outliers <- remove_outliers(data_table, "degree_centrality")


# Redo Boxplot
ggplot(data_table_no_outliers, aes(x = global_north_south, y = degree_centrality)) +
  geom_boxplot(aes(color = global_north_south), size = 1.1, position = "dodge") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(size = 0.9),
        axis.line.y = element_line(size = 0.9),
        axis.text = element_text(size = 20),
        #axis.text.x = element_text(size = 20, hjust = 0.9),
        axis.title.x = element_text(size = 24, margin = margin(t = 10)),
        axis.title.y = element_text(size = 24, margin = margin(r = 10))) +
  scale_color_manual(values = c("#01431c", "#74c476", "grey")) +
  labs(
    x = "",
    y = "Degree Centrality")

summary_stats <- data_table_no_outliers %>%
  group_by(global_north_south) %>%
  summarise(
    mean_degree_centrality = mean(degree_centrality, na.rm = TRUE),  # Mean
    se_degree_centrality = sd(degree_centrality, na.rm = TRUE) / sqrt(n())  # Standard Error
  )

# View the summary statistics
print(summary_stats)