library(broom)
library(dplyr)
library(flextable)

# 1. Extract tidy results and filter/clean them explicitly
clean_table <- tidy(final_polr_partial, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    p.value = 2 * (1 - pnorm(abs(statistic))),
    
    # Format the OR and CI cleanly
    OR_CI = case_when(
      is.na(conf.low) ~ sprintf("%.2f (Reference / Scale)", estimate),
      TRUE ~ sprintf("%.2f (%.2f – %.2f)", estimate, conf.low, conf.high)
    ),
    
    # Format p-values
    p_val = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", p.value)
    ),
    
    # Give them clean, readable display names
    Variable = case_when(
      term == "age_cat.L" ~ "Age (Linear trend)",
      term == "age_cat.Q" ~ "Age (Quadratic trend)",
      term == "age_cat.C" ~ "Age (Cubic trend)",
      term == "analgesics_planTypical+ tramadol" ~ "Analgesics Plan: Typical + Tramadol",
      term == "average_expected_pain" ~ "Average Expected Pain",
      term == "Mild|Moderate.procedure_typeOpened" ~ "Procedure Type (Mild vs Mod/Sev)",
      term == "Moderate|Severe.procedure_typeOpened" ~ "Procedure Type (Mild/Mod vs Sev)*",
      term == "Mild|Moderate.smoking_cat.L" ~ "Smoking Status (Mild vs Mod/Sev)",
      term == "Moderate|Severe.smoking_cat.L" ~ "Smoking Status (Mild/Mod vs Sev)",
      TRUE ~ term
    )
  ) %>%
  select(Variable, OR_CI, p_val)

# 2. Render a gorgeous, clean table that looks custom-built
flextable(clean_table) %>%
  set_caption("Table: Partial Proportional Odds Model for Post-Operative Pain") %>%
  theme_booktabs() %>%
  autofit()