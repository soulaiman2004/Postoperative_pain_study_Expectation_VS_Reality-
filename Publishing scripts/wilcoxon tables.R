# this code I'll write to finalise more code at shorter time
# I'll use this script to speed up stuff 
library(tidyverse)
library(furr)

library(tidyverse)

# Use map() to clean and tidy each inner subgroup test result safely
all_subgroup_summary <- map(all_subgroup_wilcoxon, function(subgroup_list) {
  map(subgroup_list, ~ tryCatch(
    tidy(.x),
    error = function(e) {
      return(NULL)
    }
  ))
})
 
#Creating a nested map plan to export all as flextables
library(tidyverse)
library(flextable)

library(tidyverse)
library(flextable)


library(tidyverse)

# Use map() to clean and tidy each inner subgroup test result safely
all_subgroup_summary <- map(all_subgroup_wilcoxon, function(subgroup_list) {
  map(subgroup_list, ~ tryCatch(
    tidy(.x),
    error = function(e) {
      return(NULL)
    }
  ))
})


tables_all_subgroups <- imap(all_subgroup_summary, function(subgroup_list, var_name) {
  imap(subgroup_list, ~ tryCatch(
    {
      if (!is.data.frame(.x)) return(NULL)
      
      ft <- .x |> 
        select(statistic, estimate, conf.low, conf.high, p.value, method) |>
        mutate(
          p_raw = p.value,
          statistic = round(statistic, 3),
          conf.high = round(conf.high, 3),
          conf.low = round(conf.low, 3),
          p.value = case_when(
            p.value < 0.001 ~ "<0.001",
            TRUE ~ as.character(round(p.value, 3))
          )
        ) |>
        select(-p_raw) |>
        flextable() |>
        set_caption(paste0("Wilcoxon test for ", var_name, " (", .y, ")")) |>
        set_header_labels(
          statistic = "Test statistic",
          estimate = "Pseudomedian",
          conf.low = "Lower CI",
          conf.high = "Upper CI",
          method = "Method",
          p.value = "P_value"
        ) |>
        theme_vanilla() |>
        autofit() |>
        fontsize(size = 10) |>
        bold(i = ~ p.value < 0.05, j = "p.value")
      
      # Automatically save each subgroup table to a Word file
      file_path <- paste0("Table_", var_name, "_", .y, ".docx")
      save_as_docx(ft, path = file_path)
      
      return(ft)
    }, 
    error = function(e) {
      message(paste0("Failed to create table for ", var_name, " -> ", .y))
      return(NULL)
    }
  ))
})
