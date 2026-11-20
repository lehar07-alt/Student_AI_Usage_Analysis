
# ============================================================
# OBJECTIVE 1: EXPLORATORY DATA ANALYSIS
# ============================================================

library(tidyverse)
library(ggplot2)

students <- readRDS(
  "processed_data/cleaned_students.rds"
)

# 1. Basic dataset information
cat("Number of records:", nrow(students), "\n")
cat("Number of attributes:", ncol(students), "\n")

# 2. Distribution of preferred AI tools
preferred_tool <- students %>%
  count(preferred_ai_tool) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

print(preferred_tool)

ggplot(preferred_tool,
       aes(x = reorder(preferred_ai_tool, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Preferred AI Tools Among Students",
    x = "AI Tool",
    y = "Number of Students"
  ) +
  theme_minimal()

# 3. Most commonly used AI tools
tool_frequency <- read.csv(
  "processed_data/ai_tools_long.csv"
) %>%
  count(ai_tools_used, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

print(tool_frequency)

ggplot(tool_frequency,
       aes(x = reorder(ai_tools_used, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Frequency of AI Tools Used",
    x = "AI Tool",
    y = "Number of Mentions"
  ) +
  theme_minimal()

# 4. Daily AI usage hours
ggplot(students, aes(x = daily_usage_hours)) +
  geom_histogram(
    bins = 15,
    color = "black"
  ) +
  labs(
    title = "Distribution of Daily AI Usage Hours",
    x = "Daily Usage Hours",
    y = "Frequency"
  ) +
  theme_minimal()

# 5. Usage by academic stream
ggplot(students,
       aes(x = stream, y = daily_usage_hours)) +
  geom_boxplot() +
  labs(
    title = "Daily AI Usage Across Academic Streams",
    x = "Academic Stream",
    y = "Daily Usage Hours"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Perceived impact on grades
impact_frequency <- students %>%
  count(impact_on_grades) %>%
  arrange(impact_on_grades)

print(impact_frequency)

ggplot(impact_frequency,
       aes(x = factor(impact_on_grades), y = n)) +
  geom_col() +
  labs(
    title = "Perceived Impact of AI Tools on Grades",
    x = "Impact Scale",
    y = "Number of Students"
  ) +
  theme_minimal()

# 7. Relationship between usage hours and perceived impact
ggplot(students,
       aes(x = daily_usage_hours,
           y = impact_on_grades)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "AI Usage Hours vs Perceived Grade Impact",
    x = "Daily Usage Hours",
    y = "Perceived Grade Impact"
  ) +
  theme_minimal()

# 8. Usage by internet access
ggplot(students,
       aes(x = internet_access,
           y = daily_usage_hours)) +
  geom_boxplot() +
  labs(
    title = "AI Usage by Internet Access Quality",
    x = "Internet Access",
    y = "Daily Usage Hours"
  ) +
  theme_minimal()

# 9. Save summary tables
write.csv(
  preferred_tool,
  "processed_data/preferred_tool_summary.csv",
  row.names = FALSE
)

write.csv(
  tool_frequency,
  "processed_data/tool_frequency.csv",
  row.names = FALSE
)

cat("EDA completed.\n")
