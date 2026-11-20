
# ============================================================
# OBJECTIVE 3: APRIORI ASSOCIATION RULE MINING
# ============================================================

library(tidyverse)
library(arules)
library(arulesViz)

students <- readRDS(
  "processed_data/cleaned_students.rds"
)

# 1. Create transaction items for every student
transaction_list <- lapply(
  seq_len(nrow(students)),
  function(i) {
    
    row <- students[i, ]
    
    # Split multiple AI tools
    tools <- strsplit(
      as.character(row$ai_tools_used),
      ","
    )[[1]]
    
    tools <- trimws(tools)
    
    # Split multiple use cases
    cases <- strsplit(
      as.character(row$use_cases),
      ","
    )[[1]]
    
    cases <- trimws(cases)
    
    # Create meaningful categorical items
    items <- c(
      paste0("Tool_", tools),
      paste0("UseCase_", cases),
      paste0("Preferred_", row$preferred_ai_tool),
      paste0("Usage_", row$usage_category),
      paste0("Impact_", row$impact_category),
      paste0("Trust_", row$trust_in_ai_tools),
      paste0("Professor_", row$do_professors_allow_use),
      paste0("Pay_", row$willing_to_pay_for_access),
      paste0("Internet_", row$internet_access),
      paste0("Stream_", row$stream)
    )
    
    items <- unique(items)
    
    items <- items[!is.na(items)]
    
    return(items)
  }
)

# 2. Convert into transactions
transactions <- as(transaction_list, "transactions")

# 3. Inspect transactions
summary(transactions)

# 4. Generate association rules
rules <- apriori(
  transactions,
  parameter = list(
    support = 0.02,
    confidence = 0.50,
    minlen = 2,
    maxlen = 4
  )
)

# 5. Sort rules by lift
rules_sorted <- sort(
  rules,
  by = "lift",
  decreasing = TRUE
)

# 6. Inspect top rules
inspect(head(rules_sorted, 20))

# 7. Remove redundant rules
rules_final <- rules_sorted[
  !is.redundant(rules_sorted)
]

# 8. Save association rules
rules_table <- as(
  rules_final,
  "data.frame"
)

write.csv(
  rules_table,
  "processed_data/apriori_rules.csv",
  row.names = FALSE
)

# 9. Visualise top rules
if (length(rules_final) > 0) {
  
  top_rules <- head(rules_final, 20)
  
  plot(
    top_rules,
    method = "graph",
    engine = "htmlwidget"
  )
  
  plot(
    top_rules,
    method = "grouped"
  )
  
} else {
  cat("No rules found. Try lowering support or confidence.\n")
}

cat("Apriori analysis completed.\n")
