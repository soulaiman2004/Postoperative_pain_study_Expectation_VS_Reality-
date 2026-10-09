#This script is made for making stuff easier to deal as we have 1 script for every step so navigation becomes easier
#We will start by deciding which libraries to use in my environment to clean the data
library(tidyverse)
library(readxl)
library(janitor)
post_operative_df <- read_excel("C:/Users/alialsoulaiman/Documents/Postoperative pain/Postoperative pain script repo folder/Publishing scripts/PRE+POST 8 Editon - ANALYZE (2).xlsx")|>
  clean_names()|>
  mutate(patients_mood = factor(
     patients_mood, ordered = TRUE,
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
  education_level = factor(case_when( education_level == "Illiterate" ~ "Illiterate",
                                      education_level == "Primary school"~ "Primary school",
                                    education_level %in% c("Middle school", "High school")~ "Middle or High school" ,
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
    ordered = TRUE, levels = c("Underwight", "Healthy weight", "Overweight", "Obese")),
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
      "Thyroidectomy","Colostomy",
      "Total Mastectomy"
    ) ~ "Major Surgery"
  ), ordered = TRUE, levels = c("Minor Surgery", "Moderate Surgery", "Major Surgery")) )

