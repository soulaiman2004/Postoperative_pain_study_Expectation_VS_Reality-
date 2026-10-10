#This script is made for making stuff easier to deal as we have 1 script for every step so navigation becomes easier
#We will start by deciding which libraries to use in my environment to clean the data
library(tidyverse)
library(readxl)
library(janitor)
library(mice)
post_operative_df <- read_excel("C:/Users/alialsoulaiman/Documents/Postoperative pain/Postoperative pain script repo folder/Publishing scripts/PRE+POST 8 Editon - ANALYZE (2).xlsx")|>
  clean_names()|> #You should reconfigure the data frame PATH so that it fits your own machine PATH to data spreadsheet
  mutate(patients_mood = factor(
     patients_mood, ordered = TRUE,
    levels = c("Very bad", "Bad", "It's ok", "Good", "Excellent")
  ), patients_sleep_quality = factor(
    patients_sleep_quality , ordered = TRUE,
    levels= c("Very bad", "Bad", "Medium", "Good", "Excellent")
  ), anxiety = factor(
    anxiety , ordered = TRUE,
    levels = c("Not at all", "Kinda", "Anxious" ,"Panic")
  ), metropolitan_or_rural = factor(case_when(metropolitan_or_rural == "Metropolitan"~ "Metropolitan", metropolitan_or_rural %in% c("Regional", "Rural") ~ "Non-Metropolitan")),
  social_status = factor(social_status),
  gender = factor(gender),
  education_level = factor(case_when( education_level == "Illiterate" ~ "Illiterate",
                                      education_level == "Primary school"~ "Primary school",
                                    education_level %in% c("Middle school", "Highschool")~ "Middle or High school" ,
                                  education_level %in% c("Post-secondary institute", "University") ~ "Higher Education"),
                           ordered = TRUE,
                           levels = c("Illiterate",
                                      "Primary school",
                                      "Middle or High school", "Higher Education")),
  smoking_cat= factor(case_when(total_smoking == 0 ~ "Non smoker",total_smoking >0 & total_smoking <= 20 ~ "Light smoker",total_smoking >20 ~ "Heavy smoker"), ordered = TRUE, 
                                      levels = c("Non smoker",
                                                 "Light smoker",
                                                 "Heavy smoker")), age_cat= factor(case_when(age <= 24 ~ "youth",age >25 & age <= 44 ~ "adults",age >45 & age <= 64 ~ "middle-aged",age >65 ~ "elderly"), ordered = TRUE, 
                                                                                       levels = c("youth",
                                                                                                  "adults",
                                                                                                  "middle-aged","elderly" )),
  BMI_cat =factor(case_when(
    bmi_kg_m_2 <18.5 ~"Underweight",
    bmi_kg_m_2 >= 18.5 & bmi_kg_m_2 < 25 ~ "Healthy weight",
    bmi_kg_m_2 >= 25 & bmi_kg_m_2 < 30 ~ "Overweight",
    bmi_kg_m_2 >=30 ~ "Obese"),
    ordered = TRUE, levels = c("Underweight", "Healthy weight", "Overweight", "Obese")),
  Surgery_cat = factor(case_when(
    the_surgical_procedure %in% c(
      "Anal fissure",
      "Axillary Mass Excision",
      "Fistula in surgical wound after cholecystectomy",
      "Hemorrhoids"
    ) ~ "Minor Surgery",
    the_surgical_procedure %in% c(
      "Inguinal hernia",
      "Epigastric hernia",
      "Incisional hernia",
      "Cholecystectomy",
      "Umbilical hernia"
    )~ "Moderate Surgery",
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
      "Thyroidectomy",
      "Colostomy",
      "Total Mastectomy"
    ) ~ "Major Surgery"
  ), ordered = TRUE, levels = c("Minor Surgery", "Moderate Surgery", "Major Surgery")),
     Gap = average_expected_pain - average_post_operative_pain )|>
  mutate (Estimation_status = factor(case_when(Gap > 1 ~ "Overestimator", Gap < -1 ~ "Underestimator", .default = "Accurate Estimator")),
Using_prescription_drugs = factor(case_when(drugs == "None"~ "No" , .default = "Yes")), 
Presence_of_chronic_disease = factor(case_when((htn_cd == 1 | dm_cd ==1| asthma_cd ==1 | hypothyroidisim_cd==1| ibs_cd==1 | cardiiac_diseases_cd ==1 | cancer_cd ==1 | others_cd ==1) ~ "Yes", .default = "No")),
current_job = factor(current_job, ordered = TRUE, levels = c("Unemployed", "Low intensity physical job", "Medium intensity physical job", "High intensity physical job")))

Dummified_df <- post_operative_df |>
  mutate(num_anxiety = as.numeric(anxiety),
         num_sleep = as.numeric(patients_sleep_quality),
         num_mood = as.numeric(patients_mood),
         num_surgery_cat = as.numeric(Surgery_cat),
         num_education = as.numeric(education_level),
         anaesthesia = factor(anaesthesia),
         analgesics_plan = factor(analgesics_plan),
         num_smoking_cat = as.numeric(smoking_cat),
         metropolitan_or_rural = as.factor(metropolitan_or_rural)
         , current_job =as.numeric(current_job), BMI_cat =as.numeric(BMI_cat) )|>
  select(gender,age_cat,education_level,  metropolitan_or_rural,bmi_kg_m_2, any_allergies, analgesic_use, num_anxiety, num_sleep, num_mood, num_surgery_cat,
  anaesthesia, analgesics_plan, num_smoking_cat,surgical_procedure_duration,Gap, Estimation_status,
Presence_of_chronic_disease, Using_prescription_drugs)

pilot_df <- post_operative_df |>
  filter(id >=1000)


#setting seed 
set.seed(5830)
imputed_Dummified_df <- mice(Dummified_df, m = 250, method = "pmm", maxit = 5)

