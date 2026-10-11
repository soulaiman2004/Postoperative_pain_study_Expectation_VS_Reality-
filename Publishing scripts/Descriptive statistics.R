# This script will focus solely on descriptive statistics alone,
# without any inferential statistics or hypothesis testing.

library(gtsummary)

Table_1_demographics <- post_operative_df|>
  select(age, gender ,social_status, education_level, current_job, metropolitan_or_rural )|>
  tbl_summary(
    by = gender,
    label = list(
      age ~ "Age (years)",
      social_status ~ "Marital Status",
      education_level ~ "Education Level",
      current_job ~ "Current Job",
      metropolitan_or_rural ~ "Metropolitan or Non-metroplolitan"
    ),     statistic = list(all_continuous() ~ "{mean} ({sd}) ({p25} - {median} - {p75})", all_categorical() ~ "{n} ({p}%)"),
     digits = all_continuous() ~ 1,
     percent = "column") |>
   add_p()
  
Table_2_surgeries_and_related_variables <- post_operative_df|>
  select(gender, the_surgical_procedure, procedure_type, surgical_procedure_duration, anaesthesia, analgesics_plan)|>
  tbl_summary(
    by = gender,
    label = list(
      the_surgical_procedure ~ "Surgical Procedure",
      procedure_type ~ "Procedure Type",
      surgical_procedure_duration ~ "Surgical Procedure Duration (minutes)",
      anaesthesia ~ "Anaesthesia Type",
      analgesics_plan ~ "Analgesics Plan"
    ),     statistic = list(all_continuous() ~ "{mean} ({sd}) ({p25} - {median} - {p75})", all_categorical() ~ "{n} ({p}%)"),
     digits = all_continuous() ~ 1,
     percent = "column") |>
   add_p()

Table_3_NRS_scores <- post_operative_df|>
  select(average_expected_pain, average_post_operative_pain,max_post_operative_pain, min_post_operative_pain, pain_on_movement, pain_on_coughing,
  hows_your_pain_in_comparison_to_your_expectations,drainage_wound_pl, surgical_wound_pl,abdomen_pl, back_pl,others_pl,
length_of_stay, smoking_cat , Estimation_status)|>
  mutate(average_expected_pain = as.factor(average_expected_pain),
          average_post_operative_pain = as.factor(average_post_operative_pain),
          max_post_operative_pain = as.factor(max_post_operative_pain),
          min_post_operative_pain = as.factor(min_post_operative_pain),
          pain_on_movement = as.factor(pain_on_movement),
          pain_on_coughing = as.factor(pain_on_coughing),
          hows_your_pain_in_comparison_to_your_expectations = as.factor(hows_your_pain_in_comparison_to_your_expectations),
          drainage_wound_pl = factor(case_when( 
            drainage_wound_pl == 0 ~ "No Pain",
            drainage_wound_pl == 1 ~  "Pain",
          )),
          surgical_wound_pl = factor(case_when( 
            surgical_wound_pl == 0 ~ "No Pain",
            surgical_wound_pl == 1 ~  "Pain",
          )),
          abdomen_pl = factor(case_when( 
            abdomen_pl == 0 ~ "No Pain",
            abdomen_pl == 1 ~  "Pain",
          )),
          back_pl = factor(case_when( 
            back_pl == 0 ~ "No Pain",
            back_pl == 1 ~  "Pain",
          )),
          others_pl = factor(case_when( 
            others_pl == 0 ~ "No Pain",
            others_pl == 1 ~  "Pain",
          )),
          smoking_cat = as.factor(smoking_cat)
  )|>
  tbl_summary(
    by = Estimation_status,
    label = list(
      average_expected_pain ~ "Average Expected Pain",
      average_post_operative_pain ~ "Average Post-operative Pain",
      max_post_operative_pain ~ "Maximum Post-operative Pain",
      min_post_operative_pain ~ "Minimum Post-operative Pain",
      pain_on_movement ~ "Pain on Movement",
      pain_on_coughing ~ "Pain on Coughing",
      hows_your_pain_in_comparison_to_your_expectations ~ "How's your pain in comparison to your expectations?",
      drainage_wound_pl ~ "Drainage Wound Pain Level",
      surgical_wound_pl ~ "Surgical Wound Pain Level",
      abdomen_pl ~ "Abdomen Pain Level",
      back_pl ~ "Back Pain Level",
      others_pl ~ "Other Pain Level",
      length_of_stay ~ "Length of Stay (days)",
      smoking_cat ~ "Smoking Status"
    ),     statistic = list(all_continuous() ~ "{mean} ({sd}) ({p25} - {median} - {p75})", all_categorical() ~ "{n} ({p}%)"),
     digits = all_continuous() ~ 1,
     percent = "column") |>
    add_p()|>
  bold_labels()
  
