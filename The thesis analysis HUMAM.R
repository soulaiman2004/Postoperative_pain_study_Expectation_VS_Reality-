 
library(tidyverse)
library(readxl)
library(nnet)
library(glmnet)
library(gtsummary)
library(janitor)
library(Matrix)
library(ggeffects)
library(flextable)
library(officer)
library(broom)
library(glmulti)
library(brglm2)

post_operative <- read_excel("C:/Users/alialsoulaiman/Downloads/Telegram Desktop/PRE+POST 8 Editon - ANALYZE.xlsx"
                             )|>
  clean_names() |>
  mutate(patients_mood = factor(
    patients_mood , ordered = TRUE,
    levels = c("Very bad", "Bad", "It's ok", "Good", "Excellent")
  ), patients_sleep_quality = factor(
    patients_sleep_quality , ordered = TRUE,
    levels= c("Very bad", "Bad", "Medium", "Good", "Excellent")
  ), anxiety = factor(
    anxiety , ordered = TRUE,
    levels = c("Not at all", "Kinda", "Anxious" ,"Panic")
  ), metropolitan_or_rural = factor(metropolitan_or_rural),
  social_status = factor(social_status),
  gender = factor(gender),
  education_level = factor(education_level,
                           ordered = TRUE,
                           levels = c("Illiterate",
                                      "Primary school",
                                      "Middle school",
                                      "Highschool",
                                      "Post-secondary institute",
                                      "University")),
  smoking_cat= factor(case_when(total_smoking == 0 ~ "Non smoker",total_smoking >0 & total_smoking <= 20 ~ "Light smoker",total_smoking >20 ~ "Heavy smoker"), ordered = TRUE, 
                                      levels = c("Non smoker",
                                                 "Light smoker",
                                                 "Heavy smoker")), age_cat= factor(case_when(age <= 24 ~ "youth",age >25 & age <= 44 ~ "adults",age >45 & age <= 64 ~ "middle-aged",age >65 ~ "elderly"), ordered = TRUE, 
                                                                                       levels = c("youth",
                                                                                                  "adults",
                                                                                                  "middle-aged","elderly" )) )

post_operative_1 <- post_operative |>
  mutate(
    the_surgical_procedure = factor(the_surgical_procedure),
    Gap = average_expected_pain   - average_post_operative_pain,
    Gap_case = case_when(
      Gap > 1 ~ "Overestimator",
      Gap < -1 ~ "Underestimator",
      Gap >= -1 & Gap <= 1 ~ "Accurate estimator"
    ), .after = average_post_operative_pain
  )
post_operative_2 <- post_operative_1 |>
  mutate(
    surgery_category = factor(
      case_when(
        the_surgical_procedure %in% c(
          "Anal dilation",
          "Anal fissure",
          "Axillary Mass Excision",
          "Fistula in surgical wound after cholecystectomy",
          "Hemorrhoids" 
        ) ~ "Minor",
        the_surgical_procedure %in% c(
          "Abdominal Wall hernia",
          "Cholecystectomy",
          "Epigastric hernia",
          "Incisional hernia",
          "endometriosis",
          "Inguinal hernia",
          "Umbilical hernia"
        ) ~ "Moderate", 
        the_surgical_procedure %in% c(
          "Adrenal Cyst Excision",
          "Exploratory laparotomy",
          "Gastric Mass Excision",
          "Heller Myotomy",
          "Hyadatid Cyst",
          "Pelvic Mass",
          "Renal Cyst Excision",
          "Sigmoid colectomy",
          "Spleenectomy",
          "Thyroidectomy","Colostomy",
          "Total Mastectomy"
        ) ~ "Major" ,
        
        TRUE  ~ "Unknown" #correcting for unknown
      ), ordered = TRUE , levels = c("Minor", "Moderate",
                                     "Major")), .after =  age_cat ,
    BMI_cat = factor(
      case_when(
        bmi_kg_m_2 < 18.5 ~ "Underweight",
        bmi_kg_m_2 >= 18.5 & bmi_kg_m_2 < 25  ~ "Normal weight",
        bmi_kg_m_2 >= 25 & bmi_kg_m_2 < 30 ~ "Overweight", 
        bmi_kg_m_2 >= 30 ~ "Obesity"), ordered = TRUE , levels = c(
          "Underweight", "Normal weight", "Overweight", 
          "Obesity"
        )), 
    
           
     surgical_duration_cat = factor(
      case_when(
        surgical_procedure_duration < 60 ~ "class 1 less than 60m",
        surgical_procedure_duration >= 60 & surgical_procedure_duration < 120  ~ "class 2 from 60m to 120m",
        surgical_procedure_duration >= 120 ~ "class 3 more than 120m"), levels = c(
          "class 1 less than 60m", "class 2 from 60m to 120m", "class 3 more than 120m"), ordered = TRUE
      ))
#descriptive stats
#TABLES

gender_table <- tbl_cross(post_operative_1, row =  gender, col = Gap_case, percent = "row")
print(gender_table)

education_level_table <- tbl_cross(post_operative_1, row =  education_level, col = Gap_case, percent = "row")
print(education_level_table)

allergies_table <- tbl_cross(post_operative_1, row =  any_allergies, col = Gap_case, percent = "row")
print(allergies_table)

social_status_table <- tbl_cross(post_operative_1, row =  social_status, col = Gap_case, percent = "row")
print(social_status_table)

analgesic_use_table <- tbl_cross(post_operative_1, row =  analgesic_use, col = Gap_case, percent = "row")
print(analgesic_use_table)

surgical_history_table <- tbl_cross(post_operative_1, row = any_past_surgical_procedures , col = Gap_case, percent = "row")
print(surgical_history_table)

what_are_the_analgesics_table <- tbl_cross(post_operative_1, row =  what_are_the_analgesics, col = Gap_case, percent = "row")
print(what_are_the_analgesics_table)

patients_mood_table <- tbl_cross(post_operative_1, row =  patients_mood, col = Gap_case, percent = "row")
print(patients_mood_table)

patients_sleep_quality_table <- tbl_cross(post_operative_1, row =  patients_sleep_quality, col = Gap_case, percent = "row")
print(patients_sleep_quality_table)

anxiety_table <- tbl_cross(post_operative_1, row = anxiety , col = Gap_case, percent = "row")
print(anxiety_table)

the_surgical_procedure_table <- tbl_cross(post_operative_1, row = the_surgical_procedure , col = Gap_case, percent = "row")
print(the_surgical_procedure_table)

anaesthesia_table <- tbl_cross(post_operative_1, row = anaesthesia , col = Gap_case, percent = "row")
print(anaesthesia_table)

analgesics_plan_table <- tbl_cross(post_operative_1, row = analgesics_plan , col = Gap_case, percent = "row")
print(analgesics_plan_table)

hows_your_pain_in_comparison_to_your_expectations_table <- tbl_cross(post_operative_1, row = hows_your_pain_in_comparison_to_your_expectations , col = Gap_case, percent = "row")
print(hows_your_pain_in_comparison_to_your_expectations_table)

current_job_table <- tbl_cross(post_operative_1, row = current_job , col = Gap_case, percent = "row")
print(current_job_table)

smoking_cat_table <- tbl_cross(post_operative_1, row = smoking_cat , col = Gap_case, percent = "row")
print(smoking_cat_table)

age_cat_table <- tbl_cross(post_operative_1, row = age_cat , col = Gap_case, percent = "row")
print(age_cat_table)

BMI_cat_table <- tbl_cross(post_operative_2, row = BMI_cat , col = Gap_case, percent = "row")
print(BMI_cat_table)

surgical_duration_cat_table <- tbl_cross(post_operative_2, row = surgical_duration_cat , col = Gap_case, percent = "row")
print(surgical_duration_cat_table)

#what_are_the_analgesics_table

what_are_the_analgesics_table <- tbl_cross(post_operative_1, row = what_are_the_analgesics, col = Gap_case, percent = "row")
print(what_are_the_analgesics_table)


procedure_type_table <- tbl_cross(post_operative_2, row = procedure_type , col = Gap_case, percent = "row")
print(procedure_type_table)

family_history_table <- post_operative_2 |> select(htn_fh,dm_fh,others_fh,Gap_case) |> tbl_summary(by=Gap_case, , percent = "row")|> add_overall(last=TRUE)
print(family_history_table)

chronic_diseases_table <- post_operative_2 |> select(htn_cd,dm_cd,asthma_cd,hypothyroidisim_cd,ibs_cd,cancer_cd,cardiiac_diseases_cd,others_cd,Gap_case) |> tbl_summary(by=Gap_case, , percent = "row")|> add_overall(last=TRUE)
print(chronic_diseases_table)

pain_location1_table <- post_operative_2 |> select(drainage_wound_pl,surgical_wound_pl,abdomen_pl,back_pl,others_pl,Gap_case) |> tbl_summary(by=Gap_case, percent = "row")|> add_overall(last=TRUE)
print(pain_location1_table)



chronic_diseases_table <- post_operative_2 |> select(htn_cd,dm_cd,asthma_cd,hypothyroidisim_cd,ibs_cd,cancer_cd,cardiiac_diseases_cd,others_cd,Gap_case) |> tbl_summary(by=Gap_case, , percent = "row")|> add_overall(last=TRUE)
print(chronic_diseases_table)


#EXPORT TO WORD

pain_location1_table |> 
  as_flex_table() |>
  save_as_docx(path = "pain_location_table.docx")





#wilcoxon tests 

global_wilcoxon <- wilcox.test( post_operative_2$average_expected_pain,
                                post_operative_2$average_post_operative_pain,
                                alternative = "two.sided",
                                paired = TRUE,
                                exact = TRUE,
                                conf.int = TRUE,
)
print(global_wilcoxon)



# ==============================================================================
# 1. Subgroup Analysis: smoking_cat
# ==============================================================================
subgroup_smoking_cat <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ smoking_cat, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_smoking_cat)

subgroup_summary_smoking <- subgroup_smoking_cat |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "smoking_cat")

print(subgroup_summary_smoking)

table_smoking <- subgroup_summary_smoking |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_smoking <- read_docx() |> 
  body_add_flextable(value = table_smoking)

print(doc_smoking, target = "Statistical_smoking_cat.docx")


# ==============================================================================
# 2. Subgroup Analysis: any_allergies
# ==============================================================================
subgroup_any_allergies <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ any_allergies, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_any_allergies)

subgroup_summary_allergies <- subgroup_any_allergies |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "any_allergies")

print(subgroup_summary_allergies)

table_allergies <- subgroup_summary_allergies |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_allergies <- read_docx() |> 
  body_add_flextable(value = table_allergies)

print(doc_allergies, target = "Statistical_any_allergies.docx")


# ==============================================================================
# 3. Subgroup Analysis: analgesic_use
# ==============================================================================
subgroup_analgesic_use <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ analgesic_use, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_analgesic_use)

subgroup_summary_analgesic <- subgroup_analgesic_use |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "analgesic_use")

print(subgroup_summary_analgesic)

table_analgesic <- subgroup_summary_analgesic |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_analgesic <- read_docx() |> 
  body_add_flextable(value = table_analgesic)

print(doc_analgesic, target = "Statistical_analgesic_use.docx")


# ==============================================================================
# 4. Subgroup Analysis: any_past_surgical_procedures
# ==============================================================================
subgroup_any_past_surgical_procedures <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ any_past_surgical_procedures, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_any_past_surgical_procedures)

subgroup_summary_past_surg <- subgroup_any_past_surgical_procedures |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "any_past_surgical_procedures")

print(subgroup_summary_past_surg)

table_past_surg <- subgroup_summary_past_surg |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_past_surg <- read_docx() |> 
  body_add_flextable(value = table_past_surg)

print(doc_past_surg, target = "Statistical_any_past_surgical_procedures.docx")


# ==============================================================================
# 5. Subgroup Analysis: what_are_the_analgesics
# ==============================================================================
subgroup_what_are_the_analgesics <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ what_are_the_analgesics, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_what_are_the_analgesics)

subgroup_summary_what_analgesics <- subgroup_what_are_the_analgesics |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "what_are_the_analgesics")

print(subgroup_summary_what_analgesics)

table_what_analgesics <- subgroup_summary_what_analgesics |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_what_analgesics <- read_docx() |> 
  body_add_flextable(value = table_what_analgesics)

print(doc_what_analgesics, target = "Statistical_what_are_the_analgesics.docx")


# ==============================================================================
# 6. Subgroup Analysis: patients_mood
# ==============================================================================
subgroup_patients_mood <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ patients_mood, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_patients_mood)

subgroup_summary_mood <- subgroup_patients_mood |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "patients_mood")

print(subgroup_summary_mood)

table_mood <- subgroup_summary_mood |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_mood <- read_docx() |> 
  body_add_flextable(value = table_mood)

print(doc_mood, target = "Statistical_patients_mood.docx")


# ==============================================================================
# 7. Subgroup Analysis: patients_sleep_quality
# ==============================================================================
subgroup_patients_sleep_quality <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ patients_sleep_quality, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_patients_sleep_quality)

subgroup_summary_sleep <- subgroup_patients_sleep_quality |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "patients_sleep_quality")

print(subgroup_summary_sleep)

table_sleep <- subgroup_summary_sleep |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_sleep <- read_docx() |> 
  body_add_flextable(value = table_sleep)

print(doc_sleep, target = "Statistical_patients_sleep_quality.docx")


# ==============================================================================
# 8. Subgroup Analysis: the_surgical_procedure
# ==============================================================================
subgroup_the_surgical_procedure <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ the_surgical_procedure, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_the_surgical_procedure)

subgroup_summary_procedure <- subgroup_the_surgical_procedure |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "the_surgical_procedure")

print(subgroup_summary_procedure)

table_procedure <- subgroup_summary_procedure |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_procedure <- read_docx() |> 
  body_add_flextable(value = table_procedure)

print(doc_procedure, target = "Statistical_the_surgical_procedure.docx")


# ==============================================================================
# 9. Subgroup Analysis: anaesthesia
# ==============================================================================
subgroup_anaesthesia <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ anaesthesia, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_anaesthesia)

subgroup_summary_anaesthesia <- subgroup_anaesthesia |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "anaesthesia")

print(subgroup_summary_anaesthesia)

table_anaesthesia <- subgroup_summary_anaesthesia |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_anaesthesia <- read_docx() |> 
  body_add_flextable(value = table_anaesthesia)

print(doc_anaesthesia, target = "Statistical_anaesthesia.docx")


# ==============================================================================
# 10. Subgroup Analysis: analgesics_plan
# ==============================================================================
subgroup_analgesics_plan <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ analgesics_plan, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_analgesics_plan)

subgroup_summary_plan <- subgroup_analgesics_plan |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "analgesics_plan")

print(subgroup_summary_plan)

table_plan <- subgroup_summary_plan |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_plan <- read_docx() |> 
  body_add_flextable(value = table_plan)

print(doc_plan, target = "Statistical_analgesics_plan.docx")


# ==============================================================================
# 11. Subgroup Analysis: hows_your_pain_in_comparison_to_your_expectations_table
# ==============================================================================
subgroup_pain_expectations <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ hows_your_pain_in_comparison_to_your_expectations_table, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_pain_expectations)

subgroup_summary_pain_vs_exp <- subgroup_pain_expectations |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "hows_your_pain_in_comparison_to_your_expectations_table")

print(subgroup_summary_pain_vs_exp)

table_pain_vs_exp <- subgroup_summary_pain_vs_exp |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_pain_vs_exp <- read_docx() |> 
  body_add_flextable(value = table_pain_vs_exp)

print(doc_pain_vs_exp, target = "Statistical_hows_your_pain_in_comparison_to_your_expectations_table.docx")


# ==============================================================================
# 12. Subgroup Analysis: surgical_duration_cat
# ==============================================================================
subgroup_surgical_duration_cat <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ surgical_duration_cat, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_surgical_duration_cat)

subgroup_summary_duration <- subgroup_surgical_duration_cat |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "surgical_duration_cat")

print(subgroup_summary_duration)

table_duration <- subgroup_summary_duration |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_duration <- read_docx() |> 
  body_add_flextable(value = table_duration)

print(doc_duration, target = "Statistical_surgical_duration_cat.docx")


# ==============================================================================
# 13. Subgroup Analysis: what_are_the_analgesics (Duplicate Run / Verification)
# ==============================================================================
subgroup_what_are_the_analgesics_run2 <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ what_are_the_analgesics, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_what_are_the_analgesics_run2)

subgroup_summary_what_analgesics_run2 <- subgroup_what_are_the_analgesics_run2 |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "what_are_the_analgesics")

print(subgroup_summary_what_analgesics_run2)

table_what_analgesics_run2 <- subgroup_summary_what_analgesics_run2 |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_what_analgesics_run2 <- read_docx() |> 
  body_add_flextable(value = table_what_analgesics_run2)

print(doc_what_analgesics_run2, target = "Statistical_what_are_the_analgesics_run2.docx")








# ==============================================================================
# 14. Subgroup Analysis: hows_your_pain_in_comparison_to_your_expectations
# ==============================================================================
subgroup_pain_comp <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ hows_your_pain_in_comparison_to_your_expectations, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_pain_comp)

subgroup_summary_pain_comp <- subgroup_pain_comp |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "hows_your_pain_in_comparison_to_your_expectations")

print(subgroup_summary_pain_comp)

table_pain_comp <- subgroup_summary_pain_comp |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_pain_comp <- read_docx() |> 
  body_add_flextable(value = table_pain_comp)

print(doc_pain_comp, target = "Statistical_hows_your_pain_in_comparison_to_your_expectations.docx")


# ==============================================================================
# 15. Subgroup Analysis: anxiety
# ==============================================================================
subgroup_anxiety <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ anxiety, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_anxiety)

subgroup_summary_anxiety <- subgroup_anxiety |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "anxiety")

print(subgroup_summary_anxiety)

table_anxiety <- subgroup_summary_anxiety |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_anxiety <- read_docx() |> 
  body_add_flextable(value = table_anxiety)

print(doc_anxiety, target = "Statistical_anxiety.docx")


# ==============================================================================
# 16. Subgroup Analysis: surgery_category
# ==============================================================================
subgroup_surgery_cat <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ surgery_category, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_surgery_cat)

subgroup_summary_surg_cat <- subgroup_surgery_cat |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "surgery_category")

print(subgroup_summary_surg_cat)

table_surg_cat <- subgroup_summary_surg_cat |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_surg_cat <- read_docx() |> 
  body_add_flextable(value = table_surg_cat)

print(doc_surg_cat, target = "Statistical_surgery_category.docx")


# ==============================================================================
# 17. Subgroup Analysis: surgical_procedure_duration
# ==============================================================================
subgroup_surgical_duration_cat <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ surgical_duration_cat, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_surgical_duration_cat)

subgroup_summary_surgical_duration_cat <- subgroup_surgical_duration_cat |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "surgical_duration_cat")

print(subgroup_summary_surgical_duration_cat)

table_surg_dur <- subgroup_summary_surgical_duration_cat |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_surg_dur <- read_docx() |> 
  body_add_flextable(value = table_surg_dur)

print(doc_surg_dur, target = "Statistical_surgical_duration_cat.docx")


# ==============================================================================
# 18. Subgroup Analysis: current_job
# ==============================================================================
subgroup_current_job <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ current_job, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact =TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_current_job)

subgroup_summary_job <- subgroup_current_job |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "current_job")

print(subgroup_summary_job)

table_job <- subgroup_summary_job |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_job <- read_docx() |> 
  body_add_flextable(value = table_job)

print(doc_job, target = "Statistical_current_job.docx")


# ==============================================================================
# 19. Subgroup Analysis: metropolitan_or_rural
# ==============================================================================
subgroup_metropolitan_or_rural <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ metropolitan_or_rural, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_metropolitan_or_rural)

subgroup_summary_metro <- subgroup_metropolitan_or_rural |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "metropolitan_or_rural")

print(subgroup_summary_metro)

table_metro <- subgroup_summary_metro |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_metro <- read_docx() |> 
  body_add_flextable(value = table_metro)

print(doc_metro, target = "Statistical_metropolitan_or_rural.docx")


# ==============================================================================
# 20. Subgroup Analysis: BMI_cat
# ==============================================================================
subgroup_BMI_cat <- post_operative_2 |>
  filter(!is.na(average_expected_pain), !is.na(average_post_operative_pain)) |>  
  split(~ BMI_cat, drop = TRUE) |>
  map(function(df) {
    if (nrow(df) < 2) {
      return(paste("Skipped: Insufficient data points (n =", nrow(df), ")"))
    } else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_BMI_cat)

subgroup_summary_BMI <- subgroup_BMI_cat |> 
  keep(is.list) |> 
  map_dfr(tidy, .id = "BMI_cat")

print(subgroup_summary_BMI)

table_BMI <- subgroup_summary_BMI |> 
  select(-method) |>        
  flextable() |> 
  colformat_double(digits = 2) |>
  theme_vanilla() |> 
  autofit()

doc_BMI <- read_docx() |> 
  body_add_flextable(value = table_BMI)

print(doc_BMI, target = "Statistical_BMI_cat.docx")


#Building a new dummy frame
Dummified_df <- post_operative_2 |>
  mutate(num_anxiety = as.numeric(anxiety),
         num_sleep = as.numeric(patients_sleep_quality),
         num_mood = as.numeric(patients_mood),
         num_surgery_cat = as.numeric(surgery_category),
         num_education = as.numeric(education_level),
         anaesthesia = factor(anaesthesia),
         analgesics_plan = factor(analgesics_plan),
         num_smoking_cat = as.numeric(smoking_cat)
          )
predictors <- c("num_anxiety"  ,                "num_sleep"         ,     
                "num_mood"  ,                  
                "num_surgery_cat"  ,            "num_education" ,              
                "any_allergies" ,    "analgesic_use" ,         
                "any_past_surgical_procedures"
                 , "htn_cd", "dm_cd", "asthma_cd", "cancer_cd", 
                "ibs_cd", "hypothyroidisim_cd","cardiiac_diseases_cd", "others_cd", 
                "htn_fh", "dm_fh", "others_fh", "any_allergies",
                "surgical_procedure_duration", "analgesics_plan",
                "how_long_it_took_the_patient_to_move_hours"
                ,"age_cat", "smoking_cat",
                "surgery_category"
)
cleaned_dummified <- Dummified_df[complete.cases(Dummified_df[,predictors]),]
cleaned_dummified_1 <-  cleaned_dummified |>
  mutate(how_long_it_took_the_patient_to_move_hours = as.numeric(how_long_it_took_the_patient_to_move_hours))
# building a model

multinom_base <- multinom(Gap_case ~ 1, data = cleaned_dummified_1, Hess = TRUE)

predictors1 <-  c("num_anxiety"  ,                "num_sleep"         ,     
                  "num_mood"  ,                  
                  "num_surgery_cat"  ,            "num_education" ,              
                  "any_allergies" ,    "analgesic_use" ,         
                  "any_past_surgical_procedures"
                  , "htn_cd", "dm_cd", "asthma_cd", "cancer_cd", 
                  "ibs_cd", "hypothyroidisim_cd","cardiiac_diseases_cd", "others_cd", 
                  "htn_fh", "dm_fh", "others_fh", "any_allergies",
                  "surgical_procedure_duration", "analgesics_plan",
                  "how_long_it_took_the_patient_to_move_hours"
                  ,"age_cat", "smoking_cat",
                  "surgery_category"
)

full_formula <- as.formula(paste("Gap_case ~ ", paste(predictors1, collapse = "+")))
multinom_full <- multinom(full_formula, data = cleaned_dummified_1)
print(n = 102,tidy(multinom_full, conf.int = TRUE))
print(tbl_regression(multinom_full, exponentiate = TRUE, conf.int = TRUE))

firth_multi <- function(f, d, ...) brglm2::brmultinom(f, data = d, trace = FALSE, ...)

exhaustive_multi <- glmulti(
  full_formula,
  data = cleaned_dummified_1,
  level = 1,
  fitfunction = "brmultinom" ,
  maxit = 50, 
  trace = FALSE,
  crit = "aic",
  method = "g"
)
summary(exhaustive_multi)
formula_winning <- exhaustive_multi@formulas[[1]]

print(formula_winning)

winning_multinom <- multinom(
 formula_winning,
 data = cleaned_dummified_1,
 Hess = TRUE
)
table_winning <- tbl_regression(winning_multinom, exponentiate = TRUE, conf.int = TRUE)
print(table_winning)
# disposed of vars that are sparse

multinom_better <- multinom(Gap_case ~ 1 + analgesics_plan +num_sleep + num_education +analgesic_use + htn_fh,
                            data =  cleaned_dummified_1,Hess = TRUE)
table_best <- tbl_regression(multinom_better, exponentiate = TRUE, conf.int = TRUE)
print(table_best)
#disposed of htn _fd as it proved redunadant on both AIC and BIC 

multinom_better_1 <- multinom(Gap_case ~ 1 + analgesics_plan +num_sleep + num_education +analgesic_use ,
                                                          data =  cleaned_dummified_1,Hess = TRUE)
multinom_better_1_tbl <- tbl_regression(multinom_better_1, exponentiate = TRUE)
print(multinom_better_1_tbl)

 # These I'll use lasso on to eliminate
# my model barely supports two vars
predictors_1 <- c(
  "num_sleep", "num_mood",
  "num_education", "total_smoking"
)
lasso_prep_df <- cleaned_dummified_1|>
  select(all_of(predictors), Gap_case)|>
  mutate(Gap_case= factor(Gap_case))
formula_X <- as.formula(paste( paste("~",predictors , collapse = "+")))
#forming the matrix for lasso
X <- model.matrix(formula_X, data = lasso_prep_df)[, -1]
Y <- lasso_prep_df$Gap_case #Thus i made the vector for lasso

#now I set seed and go on 
set.seed(320)
las_glmnet <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1,
  family = "multinomial",
  standardize = TRUE
)
plot(las_glmnet)
best_lambda <- las_glmnet$lambda.min
print(best_lambda)

coef_lasso <- coef(las_glmnet, s = best_lambda)
lapply(names(coef_lasso), function(class_name) {
  cat("\n=== Non-Zero Variables for Class:", class_name, "===\n")
  
  # Get the matrix for this specific class
  class_matrix <- coef_lasso[[class_name]]
  
  # Filter out rows where the coefficient equals 0
  surviving_vars <- class_matrix[class_matrix[, 1] != 0, , drop = FALSE]
  
  print(surviving_vars)
})

# Collect all unique main effects selected across your three classes
lasso_selected_vars <- c(
  "analgesic_use", "ibs_cd", "age_cat", 
  "num_sleep", "num_surgery_cat", "hypothyroidisim_cd", 
  "num_anxiety", "num_education", "cardiiac_diseases_cd", 
  "htn_fh", "analgesics_plan", "smoking_cat"
)

# Construct the optimized final formula
final_lasso_formula <- as.formula(
  paste("Gap_case ~", paste(lasso_selected_vars, collapse = " + "))
)


final_lasso_model <- multinom(
  final_lasso_formula, data = cleaned_dummified_1,
  method ="brGLM", maxit = 500
)
final_lasso_tbl <- tbl_regression(final_lasso_model, exponentiate = TRUE)
print(final_lasso_tbl)
#I'll try to fit a two vars model
Multinom_3 <- multinom(Gap_case ~ num_anxiety + analgesic_use + num_mood + surgery_category + num_sleep,
                       data = cleaned_dummified, Hess = TRUE)
tbl_regression_3 <- tbl_regression(Multinom_3, 
                                   exponentiate = TRUE, conf.int = TRUE)
print(tbl_regression_3)
AIC(Multinom_3)
AIC(multinom_model_1)
AIC(multinom_model_2)
BIC(multinom_model_1)
BIC(multinom_model_2)
BIC(Multinom_3)
multinom_4 <- multinom(Gap_case ~ num_sleep + analgesic_use + surgery_category 
                       , data = Dummified_df, Hess = TRUE)
tbl_regression_4 <- tbl_regression(multinom_4, 
                                   exponentiate = TRUE, conf.int = TRUE)
print(tbl_regression_4)

#This works well , I tried to use performance library to get a model from 2 without colinearity
library(performance)
check_collinearity(multinom_model_2) # There's high instability 




#now let's do some plots
num_sleep_pred <- ggemmeans(Multinom_3, terms = "num_sleep")
  sleep_plot <- ggplot(num_sleep_pred,aes(x = x, y = predicted, 
                                          colour = response.level, 
                                          fill = response.level))+
  geom_line(linewidth = 1 )+
  scale_color_brewer(palette = "Set1")+ 
  labs(
    title = "Prediction of Pain vs Sleep",
    subtitle = "The propability of falling into certain category depending on sleep category",
    x = "Patient's sleep quality score",
    y = "Propability of falling into certain estimation category",
    color = "Estimation Category"
   )+
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray85", fill = NA, linewidth = 0.5)
  )

  #making more beutiful plot  for surgical categories
 surgery_cat_pred <- ggemmeans(Multinom_3, terms = "surgery_category")
surgery_category_plot <- ggplot(surgery_cat_pred,
                                aes(x=x , y = predicted ,
                                    color = response.level))+
  geom_point(position = position_dodge(0.3))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.8, position = position_dodge(0.3)) +
  geom_line(linewidth = 1, position = position_dodge(0.3)) +
  scale_color_brewer(palette = "Set1")+
  labs(
    title = " Estimation Category vs Surgical Grade",
    subtitle = "Estimation categories plotted against Surgical Grade",
    x = "surgical Grade",
    Y = "Estimation Category",
    color = "Estimation category"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray85", fill = NA, linewidth = 0.5)
  )
 # this block used to be sth useful till i fixed a problem 
#making the last plot of analgesics use 
  analgesic_use_pred <- ggemmeans(Multinom_3, terms = "analgesic_use ")
  analgesic_use_pred_plot <- ggplot(analgesic_use_pred,
                                    aes(x =as.factor(x), y = predicted ,
                                        color = response.level))+
    geom_point(position = position_dodge(width = 0.5), size = 4)+
    geom_errorbar(aes(ymin = conf.low , ymax = conf.high), linewidth = 0.15, position = position_dodge(width = 0.5))+
    scale_color_brewer(palette = "Set1")+
    scale_x_discrete(labels = c("No Analgesics use", "Analgesics use"))+
    labs(
      title = "Estimation Category vs Analgesics Use",
      subtitle = "Estimation Category Expectation plotted against Analgesics Use",
      x ="#Analgesics use",
      y = "Estimation category prediction",
      color = "Estimation Category"
    )+
    theme_minimal(base_size = 14)+
    theme(
      plot.title = element_text(face = "bold", size = 15, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray85", fill = NA, linewidth = 0.5)
    )

  #now I'll do some Gap violin plots 
Gap_plot_analgesics <- ggplot(data = Dummified_df,
                              aes(x = as.factor(analgesic_use), y = Gap, fill = as.factor(analgesic_use )))+
  geom_violin(trim = FALSE, alpha = 0.5)+
  geom_boxplot(width = 0.1 , color = "white", outlier.shape = NA, alpha = 0.8)+
  scale_fill_brewer(palette = "Set1", labels = c("0" = "NO", "1" = "Yes"))+
  labs(
    title = "Distribution of Gap Score by Analgesics use",
    subtitle = "Violin plots illustrating NRS Gap distribution with medians and interquantile ranges",
    fill = "Analgesiscs use :",
    x = "Analgesics use",
    y = "Gap",
    caption = "This plot shows the distribution of gap values according to analgesics use, 
    \n the gap is calculated as the difference between average expected pain before 
    \n surgery and average post operative pain thoughout the 24 hours after surgery.
    \n Note: The scale was retained as continuous for practical purposes "
  )+
  scale_x_discrete(labels = c("0" = "NO", "1" = "Yes"))+
  scale_y_continuous(limits = c(-10,+10), breaks = seq(-10,+10, by =1))+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b =8)),
    plot.subtitle = element_text(size = 13 ,color = "grey40"),
    plot.caption = element_text(size = 10, margin = margin(t = 10), color = "grey70", hjust = 0.5),
    legend.position = "bottom",
    legend.justification = "left",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray85", fill = NA, linewidth = 0.5)
  )

#another violin for social status



violin_gap_social_status <- ggplot(data = Dummified_df,
                                     aes(x = social_status, y = Gap, fill = social_status))+
  geom_violin(trim = FALSE, alpha = 0.5)+
  geom_boxplot(width = 0.1 , color = "white", outlier.shape = NA, alpha = 0.8)+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10, by =1))+
  labs(
    title = "Distribution of Gap Scores by Marital Status",
    subtitle = "Violin plots overlaid with medians and interquartile ranges",
    caption = "This plot illustrates the distribution of NRS gap scores across marital status groups, including missing data (NA).\nThe derivation of the gap score is detailed in the Methods section.\nNote: The scale is treated as continuous for practical visualization purposes.",
    x = "Marital Status",
    y = "Gap", 
    fill= "Marital status"
  )+
  theme_minimal(base_size = 14)+
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 13, color = "grey40"),
      plot.caption = element_text(size = 10, margin = margin(t = 10), colour = "grey70", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray85", fill = NA , linewidth = 0.5),
      legend.position = "bottom",
      legend.justification = "left"
    )

education_gap_violin <- ggplot(Dummified_df, aes(x = education_level, y = Gap, fill = education_level))+
  geom_violin(trim = FALSE, alpha = 0.5)+
  geom_boxplot(width = 0.1 , color = "white", alpha = 0.8)+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10, by = 1))+
  labs(
    title = "Distribution of Gap Scores by Education Level",
    subtitle = "Violin plots overlaid with median and interquantiles ranges",
    caption = "This plot illustates the distribution of NRS gap score across education level groups, including not available data.\n the derivation of Gap scores is detailed in the Methods section\n Note : The scale is treated as continuous for practical visualisation purposes",
    x = "Education Level",
    y = "Gap",
    fill = "Educaion Level"
  )+
  theme_minimal(base_size = 14)+
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 13, color = "grey40"),
      plot.caption = element_text(size = 10, margin = margin(t = 10), colour = "grey70", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray85", fill = NA , linewidth = 0.5),
      legend.position = "bottom",
      legend.justification = "left"
    )

gender_violin <- ggplot(data = Dummified_df, aes(x =gender , y = Gap, fill = gender))+
  geom_violin(trim = FALSE, alpha = 0.5)+
  geom_boxplot(width = 0.1 , color = "white", alpha = 0.8 )+
  scale_color_brewer(palette = "Set1")+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10, by =1))+
  labs(
    title = "Distribution of Gap scrore by Gender",
    subtitle = "Violin plots with medians and interquantile ranges",
    x = "Gender",
    y = "Gap score",
    fill = "Gender"
  )+ theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, color = "grey40"),
    plot.caption = element_text(size = 10, margin = margin(t = 10), colour = "grey70", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray85", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )

surgery_cat_violin <- ggplot(data = Dummified_df, aes(x = surgery_category, y = Gap, fill = surgery_category))+
  geom_violin(trim = FALSE , alpha = 0.5)+
  geom_boxplot(width = 0.1 , color = "white", alpha = 0.8)+
  scale_color_brewer(palette = "Set1")+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10, by = 1) )+
  labs(
    title = "Distribution of Gap score by Surgery Grade",
    subtitle = "Violin plots with medians and interquantile ranges",
    x = "Surgery Grade",
    y = "Gap Score",
    fill = "Surgery Grade"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, color = "grey40"),
    plot.caption = element_text(size = 10, margin = margin(t = 10), colour = "grey70", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray85", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )

smoking_violin <- ggplot(data = Dummified_df, aes(x = smoking_cat, y = Gap, fill = smoking_cat))+
  geom_violin(trim = FALSE, alpha = 0.5)+
  geom_boxplot(width = 0.1, color = "white", alpha = 0.8)+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "Distribution of Gap Score by Smoking Abuse Category",
    subtitle = "Violin plots with medians and interquantile ranges",
    x = "Smoking Abuse category",
    y = "Gap Score",
    fill = "Smoking Abuse Category"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, color = "grey40"),
    plot.caption = element_text(size = 10, margin = margin(t = 10), colour = "grey70", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray85", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )


#based on multi-best_1 model
predict_analgesics_plan <- ggemmeans(multinom_better_1, terms = "analgesics_plan")
analgesics_plan_plot <- ggplot(predict_analgesics_plan, aes(x = x, y = predicted , color = response.level))+
  geom_point(position = position_dodge(0.5), size = 4)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.15, position = position_dodge(0.5))+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "Prediction of Estimation by Analgesics' Plan",
    subtitle = "The probability of falling into one estimation category depending on analgesics plan",
    x = "Analgesics plan ",
    y = "Prediction of falling into one category", 
    color = "Estimation Category",
    caption = "The propability of falling into state of estimation pain according to analgesics' plan picked.\n
    The points represent the value picked by model while the error bars represent the Confidence interbal 95% of propability"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
print(analgesics_plan_plot)

sleep_num_predict <- ggemmeans(multinom_better_1, terms = "num_sleep")
sleep_better_plot <- ggplot(sleep_num_predict, aes(x =x , y = predicted, color = response.level))+
  geom_line(linewidth = 0.1)+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "Prediction of Estimation by Sleep Quality",
    subtitle = "The propability of falling into one estimation category depending on Sleep Quaity",
    x = "Sleep Quality",
    y = "The probability of falling into one category",
    color = "Estimation Category",
    caption = "The probability of falling into one category according to sleep quality. \n
    The line was picked to show the effect of sleep quality as a continuom rather than discrete, \n
    although the values represented are of the dummy variable used in the model"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
print(sleep_better_plot)

education_predict <- ggemmeans(multinom_better_1, terms = "num_education")
education_predict_plot <- ggplot(education_predict, aes(x = x, y= predicted, color = response.level))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "Prediction of Estimation by Education Level ",
    subtitle = "The probability of falling into one category depending on Education Level",
    x = "Education Level",
    y = "Prediction of falling into one category",
    color = "Estimation category",
    caption = "The probability of falling into one category according to Education level.\n
    The line shows continuity as it's constructed by dummy variable to show continuity,\n
    but a good reader would it reprsents nothing real."
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
print(education_predict_plot)

predict_analgesics_use_2 <- ggemmeans(multinom_better_1, terms = "analgesic_use")
analgesics_pred_plot <- ggplot(predict_analgesics_use_2, aes(x =x , y = predicted, color = response.level))+
  geom_point(size = 4 ,position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.5))+
  scale_x_continuous(breaks = c(0,1), labels = c("No", "Yes"))+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "Prediction of Estimation by Analgesics use",
    subtitle = "The propability of falling into one category depending on Analgesics Use",
    x = "Analgesics use",
    Y = "Prediction of falling into one category",
    color = "Estimation category"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
print(analgesics_pred_plot)

# now I'll go to make violin plots
allergies_violin <- ggplot(post_operative_2, aes(x = as.factor(any_allergies), y = Gap, fill = as.factor(any_allergies)))+
  geom_violin(alpha = 0.5, trim = FALSE)+
  geom_boxplot(alpha = 0.8 , width = 0.1, color = "white")+
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10,10, by = 1))+
  scale_x_discrete(breaks = c(0,1), labels = c("No", "Yes"))+
  scale_fill_brewer(palette = "Set1")+
  labs(
    title = "Distributionof Gap score by Allergies Presence",
    subtitle = "Violin Plots with medians and interquantile ranges",
    x = "Allergy Presence",
    y ="Gap Score",
    fill = "Allergies presence"
    
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
print(allergies_violin)

surgical_history_violin <- ggplot(post_operative_2, aes(x = as.factor(any_past_surgical_procedures) ,y = Gap, fill = as.factor(any_past_surgical_procedures)))+
  geom_violin(trim = FALSE, alpha = 0.5)+
  geom_boxplot(width = 0.1 , color = "white", alpha = 0.8)+
  scale_x_discrete(breaks = c(0,1), labels = c("No", "Yes"))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10, by = 1))+
  scale_fill_brewer(palette = "Set1")+
  labs(
    title = "Distribution of Gap By Surgical History", 
    subtitle = "Violin plots with medians and interquantile ranges",
    x = "Surgical History",
    y = "Gap",
    fill = "Surgical History"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
print(surgical_history_violin)

age_cat_violin <- ggplot(Dummified_df, aes(x = age_cat, y = Gap, fill = age_cat))+
  geom_violin(alpha = 0.5, trim = FALSE)+
  geom_boxplot(width = 0.1, color ="white", alpha = 0.8)+
  scale_x_discrete(breaks = c(1,2,3,4), labels = c("Young Adults", "Adults", "Middle-aged", "Elderly" ))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10, by = 1))+
  scale_fill_brewer(palette = "Set1")+
  labs(
    title = "Disribution of Gap by Age Category",
    subtitle = "Violin plots with medians and interquantile ranges",
    x = "Age Category",
    y = "Gap",
    fill = "Age Category"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
print(age_cat_violin)

current_job_violin <- ggplot(Dummified_df, aes(x = current_job, y= Gap, fill = current_job))+
  geom_violin(trim = FALSE, alpha = 0.5)+
  geom_boxplot(width = 0.1, color = "white",alpha = 0.8)+
  scale_x_discrete(breaks = c(1,2,3,4) , labels = c("High intensity physical job", "Low intensity physical job" , "Medium intensity physical"  
                                                    , "Unemployed"))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10, by = 1))+
  scale_fill_brewer(palette = "Set1")+
  labs(
    title = "Distribution of Gap by Current Job Intensity",
    subtitle = "Violin plots with medians and interquantile ranges",
    x = "Current Job Intensity",
    y = "Gap",
    fill = "Current Job Intensity"
  )+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 10 , margin = margin(t = 10), color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA , linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "left"
  )
 print(current_job_violin)
 
Bmi_cat_violin <- ggplot(Dummified_df)
# forest plot
library(sjPlot)

# Generate a highly polished, minimalist forest plot
minimal_forest_plot <- plot_model(
  multinom_better_1, 
  type = "est", 
  exponentiate = TRUE,        # Automatically converts log-odds to clean Odds Ratios
  show.values = TRUE,         # Prints the exact OR numbers right next to the points
  value.offset = 0.3,         # Subtle breathing room for the text numbers
  value.size = 3.5,           # Font size of your printed OR values
  line.size = 0.8,            # Thinner, sharper confidence lines
  dot.size = 2.5,             # Professional size for your point markers
  vline.color = "gray70"      # Subtle dotted line of no effect at OR = 1
) +
  
  # --- THE MINIMALIST FINISH ---
  theme_minimal(base_size = 12) + # Strips away background grid shapes and gray panels
  theme(
    panel.grid.minor = element_blank(), # Removes unnecessary minor grid lines
    panel.grid.major.y = element_blank(), # Removes horizontal lines so predictors sit cleanly
    axis.title.x = element_text(face = "bold", size = 11, margin = margin(t = 10)),
    axis.text = element_text(color = "black", size = 10),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    legend.position = "bottom"  # Places your multinomial category selectors cleanly at the bottom
  ) +
  
  # Adjust colors to a clean, authoritative academic palette (e.g., Slate/Navy)
  scale_color_brewer(palette = "Set1") +
  
  # Professional labels
  labs(
    title = "Multinomial model final ",
    x = "Predictor Characteristics",
    y = "Odds Ratio (95% Confidence Interval)"
  )

# Display your clean minimalist layout
print(minimal_forest_plot)
