source("app/app.R")
rep_month <- "2026-02-01"
rep_date <- as.Date(rep_month)

budg <- budgets() %>% filter(EffectiveDate <= rep_date)
cat_sum <- category_summary()

rd <- budg %>% full_join(cat_sum, by=c("Category", "Subcategory")) %>% 
  mutate(Limit = tidyr::replace_na(Limit, 0), Total = tidyr::replace_na(Total, 0), Remaining = Limit - Total)

quarto::quarto_render("app/monthly_report.qmd", output_format="PrettyPDF-pdf",
  execute_params=list(report_month=rep_month, report_data=rd, category_summary=cat_sum, active_budgets=budg))
