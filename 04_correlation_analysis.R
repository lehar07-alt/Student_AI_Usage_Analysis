
# ============================================================
# OBJECTIVE 5: CORRELATION AND REGRESSION
# ============================================================

library(tidyverse)
library(ggplot2)

students <- readRDS(
  "processed_data/cleaned_students.rds"
)

# 1. Select numeric variables
numeric_data <- students %>%
  select(
    daily_usage_hours,
    trust_in_ai_tools,
    awareness_level,
    number_of_ai_tools,
    impact_on_grades
  )

# 2. Calculate correlation matrix
correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs",
  method = "pearson"
)

print(correlation_matrix)

# 3. Save correlation matrix
write.csv(
  correlation_matrix,
  "processed_data/correlation_matrix.csv"
)

# 4. Visualise correlations
correlation_long <- as.data.frame(
  as.table(correlation_matrix)
)

ggplot(
  correlation_long,
  aes(
    x = Var1,
    y = Var2,
    fill = Freq
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(Freq, 2)),
    size = 4
  ) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Heatmap",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# 5. Correlation between usage hours and impact
cor_test <- cor.test(
  students$daily_usage_hours,
  students$impact_on_grades,
  method = "pearson"
)

print(cor_test)

# 6. Multiple linear regression
regression_model <- lm(
  impact_on_grades ~
    daily_usage_hours +
    trust_in_ai_tools +
    awareness_level +
    number_of_ai_tools +
    year_of_study,
  data = students
)

# 7. View regression results
summary(regression_model)

# 8. Check model diagnostics
par(mfrow = c(2, 2))
plot(regression_model)
par(mfrow = c(1, 1))

# 9. Save regression coefficients
regression_results <- as.data.frame(
  summary(regression_model)$coefficients
)

regression_results$Variable <- rownames(
  regression_results
)

rownames(regression_results) <- NULL

write.csv(
  regression_results,
  "processed_data/regression_results.csv",
  row.names = FALSE
)

cat("Correlation and regression analysis completed.\n")
