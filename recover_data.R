library(dplyr)
library(readr)
library(lubridate)

# Path configuration
backup_28 <- "/Users/carson/Documents/R_Projects/budgeting_tool_slater/backups/category_budget_20260128-backup.csv"
backup_31 <- "/Users/carson/Documents/R_Projects/budgeting_tool_slater/backups/category_budget_20260131-backup.csv"
output_path <- "/Users/carson/Documents/R_Projects/budgeting_tool_slater/data/category_budget.csv"

# Load backups
df28 <- read_csv(backup_28, show_col_types = FALSE) %>%
    mutate(Source = "Jan28", EffectiveDate = as.Date(EffectiveDate))
df31 <- read_csv(backup_31, show_col_types = FALSE) %>%
    mutate(Source = "Jan31", EffectiveDate = as.Date(EffectiveDate))

# Helper to clean subcategory (defined in app.R but redefined here for standalone script)
clean_sub <- function(x) {
    if (is.null(x)) {
        return("")
    }
    x[is.na(x)] <- ""
    trimws(x)
}

# Merge and basic cleanup
combined <- bind_rows(df28, df31) %>%
    mutate(Subcategory = clean_sub(Subcategory))

# Filter out obvious junk (e.g., Groceries, 500)
combined <- combined %>%
    filter(!(Category == "Groceries" & Subcategory == "500"))

# Deduplication strategy:
# For each Category, Subcategory, EffectiveDate:
# 1. If there's a non-zero limit, prefer it over zero limit.
# 2. If multiple non-zero limits, prefer the one from Jan 31 (later backup).
combined_clean <- combined %>%
    group_by(Category, Subcategory, EffectiveDate) %>%
    arrange(desc(Source), desc(Limit)) %>%
    slice(1) %>%
    ungroup()

# Now handle the 0-limit entries that were created for "history preservation" in Jan 31.
# The user said they wanted to change things and it created a new line and kept the old one.
# Jan 31 has:
# Groceries, Omnibus, 2026-02-01, 500
# Groceries, Omnibus, 2026-01-01, 0
# Groceries, Omnibus, 2020-01-01, 450
#
# If they wanted to update Groceries from 450 to 500 starting Feb 1st,
# then Jan should stay 450, not become 0.
# The 0 entries are likely the ones they want to remove if a non-zero one exists prior.

# Strategy: Remove entries with 0 limit if they were created as a "stop" but there is history.
# Wait, if they actually want to stop a budget, 0 is correct.
# But the user said it was a "horrible experience" and created "redundancies".
# The redundancies are probably the 0 lines that replaced the old ones for the current month.

# Let's see:
# If Category/Subcategory has a 0-limit entry on 2026-01-01 (current month)
# AND it has an older non-zero entry (e.g. 2020-01-01),
# the 0 one is probably the mistake from "archiving" instead of "updating".
mistake_zeros <- combined_clean %>%
    filter(Limit == 0 & EffectiveDate == as.Date("2026-01-01")) %>%
    semi_join(
        combined_clean %>% filter(Limit > 0 & EffectiveDate < as.Date("2026-01-01")),
        by = c("Category", "Subcategory")
    )

combined_no_mistakes <- combined_clean %>%
    anti_join(mistake_zeros, by = c("Category", "Subcategory", "EffectiveDate", "Limit"))

# Calculate ConclusionDate
final_df <- combined_no_mistakes %>%
    select(Category, Subcategory, Limit, Frequency, EffectiveDate) %>%
    arrange(Category, Subcategory, EffectiveDate) %>%
    group_by(Category, Subcategory) %>%
    mutate(
        ConclusionDate = lead(EffectiveDate) - days(1)
    ) %>%
    ungroup() %>%
    arrange(Category, Subcategory, desc(EffectiveDate))

# Write to data directory
write_csv(final_df, output_path, na = "")

cat("Data recovery and cleanup complete. Saved to:", output_path, "\n")
