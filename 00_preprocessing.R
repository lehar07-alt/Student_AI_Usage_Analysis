
# ============================================================
# STUDENT AI TOOL USAGE ANALYSIS
# FILE 00: DATA PREPROCESSING
# ============================================================

# 1. Install required packages (only needed once)
packages <- c(
  "tidyverse",
  "janitor",
  "arules",
  "arulesViz",
  "rpart",
  "rpart.plot",
  "caret"
)

new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# 2. Load packages
library(tidyverse)
library(janitor)

# 3. Read the dataset
students <- read.csv(
  "Students.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A")
)

# 4. Inspect original data
cat("Original dimensions:", dim(students), "\n")
str(students)
summary(students)

# 5. Standardise column names
students <- students %>%
  clean_names()

# 6. Remove leading/trailing spaces and standardise text
students <- students %>%
  mutate(across(
    where(is.character),
    ~ trimws(.x)
  ))

# Convert empty strings to NA
students <- students %>%
  mutate(across(
    where(is.character),
    ~ na_if(.x, "")
  ))

# 7. Remove exact duplicate rows
duplicate_count <- sum(duplicated(students))

cat("Exact duplicate rows:", duplicate_count, "\n")

students <- students %>%
  distinct()

# 8. Handle missing values
# State has substantial missingness, so preserve records
# and mark missing states as Unknown.

students <- students %>%
  mutate(
    state = replace_na(state, "Unknown")
  )

# 9. Convert relevant columns to appropriate types
students <- students %>%
  mutate(
    year_of_study = as.numeric(year_of_study),
    daily_usage_hours = as.numeric(daily_usage_hours),
    trust_in_ai_tools = as.numeric(trust_in_ai_tools),
    impact_on_grades = as.numeric(impact_on_grades),
    awareness_level = as.numeric(awareness_level),
    
    stream = as.factor(stream),
    year_of_study = as.factor(year_of_study),
    do_professors_allow_use = as.factor(
      do_professors_allow_use
    ),
    preferred_ai_tool = as.factor(preferred_ai_tool),
    awareness_level = as.numeric(awareness_level),
    willing_to_pay_for_access = as.factor(
      willing_to_pay_for_access
    ),
    state = as.factor(state),
    device_used = as.factor(device_used),
    internet_access = as.factor(internet_access)
  )

# 10. Check numeric ranges
cat("\nNumeric summaries after cleaning:\n")
summary(students %>% select(
  daily_usage_hours,
  trust_in_ai_tools,
  impact_on_grades,
  awareness_level
))

# 11. Check missing values
cat("\nMissing values after cleaning:\n")
print(colSums(is.na(students)))

# 12. Create additional useful variables
# Number of AI tools used by each student
students <- students %>%
  mutate(
    number_of_ai_tools = str_count(ai_tools_used, ",") + 1,
    
    usage_category = case_when(
      daily_usage_hours < 2 ~ "Low",
      daily_usage_hours < 4 ~ "Moderate",
      TRUE ~ "High"
    ),
    
    impact_category = case_when(
      impact_on_grades < 0 ~ "Negative",
      impact_on_grades == 0 ~ "Neutral",
      impact_on_grades > 0 ~ "Positive"
    )
  )

# 13. Create a separate long-format AI tool dataset
# This is useful because one student may use multiple tools.

tools_long <- students %>%
  select(ai_tools_used) %>%
  separate_rows(ai_tools_used, sep = ",") %>%
  mutate(ai_tools_used = trimws(ai_tools_used)) %>%
  filter(!is.na(ai_tools_used))

# 14. Create output folder
if (!dir.exists("processed_data")) {
  dir.create("processed_data")
}

# 15. Save processed datasets
write.csv(
  students,
  "processed_data/cleaned_students.csv",
  row.names = FALSE
)

write.csv(
  tools_long,
  "processed_data/ai_tools_long.csv",
  row.names = FALSE
)

saveRDS(
  students,
  "processed_data/cleaned_students.rds"
)

cat("\nPREPROCESSING COMPLETED SUCCESSFULLY\n")
cat("Final dimensions:", dim(students), "\n")
cat("Files saved in processed_data folder.\n")
