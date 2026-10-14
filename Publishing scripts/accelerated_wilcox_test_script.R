# this a wilcoxon loop paired with multisseion
library(tidyverse)
library(furrr)

# 1. Set up multi-core processing (uses your laptop's CPU cores)
plan(multisession)

group_vars <- c(
  "Surgery_cat", "smoking_cat", "education_level", "anaesthesia", 
  "analgesics_plan", "gender", "age_cat", "current_job", "social_status", 
  "metropolitan_or_rural", "BMI_cat", "any_allergies", "htn_cd", 
  "dm_cd", "asthma_cd", "hypothyroidisim_cd", "ibs_cd", "cancer_cd", 
  "cardiiac_diseases_cd", "htn_fh", "dm_fh", "Using_prescription_drugs", 
  "Presence_of_chronic_disease", "analgesic_use"
)

# 2. Use future_map() instead of map() to run them in parallel
all_subgroup_wilcoxon <- set_names(group_vars) |> 
  future_map(function(var_name) {
    post_operative_df |>
      split(post_operative_df[[var_name]], drop = TRUE) |>
      map(function(df) {
        if(nrow(df) < 10) { 
          return(paste("Not enough data for Wilcoxon test in this subgroup")) 
        } else {
          wilcox.test(
            df$average_expected_pain, 
            df$average_post_operative_pain, 
            paired = TRUE, 
            alternative = "two.sided", 
            exact = TRUE, 
            conf.int = TRUE, 
            conf.level = 0.95
          )
        }
      })
  }, .options = furrr_options(seed = TRUE)) # Ensures reproducible random components if any