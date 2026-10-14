# This script was made specifically for wilcoxon test (primary outcome of the study), it will include some edits to main work we did before
# as sample size needed is 81 if we considered our early study enrollement, so we will do wilcoxon tests for primary outcome
# Now let's do the tests

Global_wilcoxon_test <- wilcox.test(post_operative_df$average_expected_pain, post_operative_df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)

#==================================================================
# 1. Subgroup analysis for Wilcoxon test based on Surgery Category
#==================================================================
Subgroup_wilcoxon_test_surgery_cat <- post_operative_df |>
  split(~ Surgery_cat, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })
#==================================================================
# 2. Subgroup analysis for Wilcoxon test based on Smoking Category
#==================================================================
Subgroup_wilcoxon_test_smoking_cat <- post_operative_df |>
  split(~ smoking_cat, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })  

#==================================================================
# 3. Subgroup analysis for Wilcoxon test based on Education Level
#==================================================================
Subgroup_wilcoxon_test_education_level <- post_operative_df |>
  split(~ education_level, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 4. Subgroup analysis for Wilcoxon test based on Anaesthesia Type
#==================================================================
Subgroup_wilcoxon_test_anaesthesia_type <- post_operative_df |>
  split(~ anaesthesia, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 5. Subgroup analysis for Wilcoxon test based on Analgesics Plan
#==================================================================
Subgroup_wilcoxon_test_analgesics_plan <- post_operative_df |>
  split(~ analgesics_plan, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 6. Subgroup analysis for Wilcoxon test based on Gender
#==================================================================
Subgroup_wilcoxon_test_gender <- post_operative_df |>
  split(~ gender, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 7. Subgroup analysis for Wilcoxon test based on Age Category
#==================================================================
Subgroup_wilcoxon_test_age_cat <- post_operative_df |>
  split(~ age_cat, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 8. Subgroup analysis for Wilcoxon test based on Current Job
#==================================================================
Subgroup_wilcoxon_test_current_job <- post_operative_df |>
  split(~  current_job, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 9. Subgroup analysis for wilcoxon test based on Marital status
#==================================================================
Subgroup_wilcoxon_test_social_status <- post_operative_df |>
  split(~ social_status, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 10. Subgroup analysis for wilcoxon test based on Metropolitan or Rural Area of Residency
#==================================================================
Subgroup_wilcoxon_test_residency_area <- post_operative_df |>
  split(~ metropolitan_or_rural, drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 11. Subgroup analysis for wilcoxon test based on BMI Category
#==================================================================
Subgroup_wilcoxon_test_bmi_category <- post_operative_df |>
  split(~ BMI_cat , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 12. Subgroup analysis for wilcoxon test based on Allergy Presence
#==================================================================
Subgroup_wilcoxon_test_any_allergies <- post_operative_df |>
  split(~ any_allergies , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 13. Subgroup analysis for wilcoxon test based on Hypertension Presence
#==================================================================
Subgroup_wilcoxon_test_htn_cd <- post_operative_df|>
  split(~ htn_cd , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 14. Subgroup analysis for wilcoxon test based on Diabetes Mellitus Presence
#==================================================================
Subgroup_wilcoxon_test_dm_cd <- post_operative_df|>
  split(~ dm_cd , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 15. Subgroup analysis for wilcoxon test based on Asthma Presence
#==================================================================
Subgroup_wilcoxon_test_asthma_cd <- post_operative_df|>
  split(~ asthma_cd , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 16. Subgroup analysis for wilcoxon test based on Hypothyroidism Presence
#==================================================================
Subgroup_wilcoxon_test_hypothyroidism <- post_operative_df|>
  split(~  hypothyroidisim_cd , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 17. Subgroup analysis for wilcoxon test based on Inflammatory Bowel Syndrome Presence
#==================================================================
Subgroup_wilcoxon_test_ibs_cd <- post_operative_df|>
  split(~ ibs_cd , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 18. Subgroup analysis for wilcoxon test based on Cancer Presence
#==================================================================
Subgroup_wilcoxon_test_cancer_cd <- post_operative_df|>
  split(~ cancer_cd , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  })

#==================================================================
# 19. Subgroup analysis for wilcoxon test based on Cardiac Diseases presence
#==================================================================
Subgroup_wilcoxon_test_cardiac_diseases <- post_operative_df|>
  split(~ cardiiac_diseases_cd , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  }) # Won't do the others column because they are rare and it's redundat computationally

#==================================================================
# 20. Subgroup analysis for wilcoxon test based on Familial history of Hypertension
#==================================================================
Subgroup_wilcoxon_test_htn_fh <- post_operative_df|>
  split(~ htn_fh , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  }) # Won't do the others column because they are rare and it's redundat computationally

#==================================================================
# 21. Subgroup analysis for wilcoxon test based on Familial history of Diabetes Mellitus
#==================================================================
Subgroup_wilcoxon_test_dm_fh <- post_operative_df|>
  split(~ dm_fh , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  }) # Won't do the others column because they are rare and it's redundat computationally

#==================================================================
# 22. Subgroup analysis for wilcoxon test based on Prescribed_drugs usage
#==================================================================
Subgroup_wilcoxon_test_prescribed_drugs <- post_operative_df|>
  split(~ Using_prescription_drugs , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  }) # Won't do the others column because they are rare and it's redundat computationally

#====================================================================
# 23. Subgroup analysis for wilcoxon test based on chronic morbidities lumped together
#====================================================================
Subgroup_wilcoxon_test_Chronic_diseases <- post_operative_df|>
  split(~ Presence_of_chronic_disease , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  }) # Won't do the others column because they are rare and it's redundat computationally

#====================================================================
# 24. Subgroup analysis for wilcoxon test based on Analgesics use
#====================================================================
Subgroup_wilcoxon_test_analgesics_use <- post_operative_df|>
  split(~ analgesic_use , drop = TRUE) |>
  map(function(df) {
    if(nrow(df) < 10) { return(paste("Not enough data for Wilcoxon test in this subgroup")) }
    else {
      wilcox.test(df$average_expected_pain, df$average_post_operative_pain, paired = TRUE, alternative = "two.sided", exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    }
  }) # Won't do the others column because they are rare and it's redundat computationally
