# This script was build as the final analysis for MD thesis after large trial and error tries
# I plan to start with Descritptive stats then move to A effect size model, then regularise the model using lasso(optional)


#Let's download the libraries we need 
library(tidyverse)
library(readxl)
library(nnet)
library(glmnet)
library(gtsummary)
library(janitor)
library(Matrix)
library(ggeffects)

#Let's read the file
post_operative <- read_excel("PRE+POST 8 Editon - ANALYZE.xlsx"
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
                                      "University")) ,smoking_cat = factor(case_when(
                                        total_smoking == 0 ~ "Non smoker",
                                        total_smoking >0 & total_smoking <= 20 ~ "Light smoker",
                                        total_smoking >20 ~ "Heavy smoker"
                                      ), ordered = TRUE, 
                                      levels = c("Non smoker",
                                                 "Light smoker",
                                                 "Heavy smoker")) )

required_raw_cols <- c(
  "average_expected_pain",
  "average_post_operative_pain",
  "the_surgical_procedure",
  "patients_mood",
  "patients_sleep_quality",
  "anxiety",
  "metropolitan_or_rural",
  "social_status",
  "gender",
  "education_level",
  "total_smoking",
  "bmi_kg_m_2",
  "anaesthesia",
  "analgesics_plan",
  "analgesic_use",
  "any_allergies",
  "any_past_surgical_procedures",
  "age"
)

missing_raw_cols <- setdiff(required_raw_cols, names(post_operative))
if (length(missing_raw_cols) > 0) {
  stop(
    "Missing expected raw columns in the Excel file: ",
    paste(missing_raw_cols, collapse = ", "),
    ". Check the exact spreadsheet column names after cleaning."
  )
}

post_operative_1 <- post_operative |>
  mutate(
    the_surgical_procedure = factor(the_surgical_procedure),
    Gap = average_expected_pain   - average_post_operative_pain,
    Gap_case = case_when(
      Gap >1 ~ "Overestimator",
      Gap < -1 ~ "Underestimator",
      Gap <=1 & Gap >= -1 ~ "Accurate estimator"
    ), .after = average_post_operative_pain
  )

#Let's start with descriptive stats
#I'll try to get a table and piechart for education

Education_table <- tbl_cross(post_operative, row = education_level,
                             col = gender, percent = "cell"
                             )
print(Education_table)

social_status_table <- tbl_cross(
  post_operative,
  row = social_status ,
  col = gender
  , percent = "cell"
)
print(social_status_table)

metropolitan_tbl <- tbl_cross(
  post_operative, 
  row = metropolitan_or_rural,
  col = gender , percent = "cell"
)
print(metropolitan_tbl)

procedure_tbl <- tbl_cross(
  post_operative_1,
  row = the_surgical_procedure ,
  col = gender, percent = "cell"
)
print(procedure_tbl)

# A table of Gap_case against both lumped procedures
#let's make procedures into categorised surgeries 
Surgeries_corrected_frame <- post_operative_1 |>
  mutate(
    surgery_category = factor(
      case_when(
        the_surgical_procedure %in% c(
          "Anal dilation",
          "Anal fissure",
          "Anal fistula",
          "Anal fistula+ Anal fissure",
          "Axillary Mass Excision",
          "Fistula in surgical wound after cholecystectomy",
          "Hemorrhoids" 
        ) ~ "Minor",
        the_surgical_procedure %in% c(
          "Abdominal Wall hernia",
          "Cholecystectomy",
          "Colostomy",
          "colostomy",
          "Colostomy reversal",
          "Epigastric hernia",
          "Incisional Hernia",
          "Incisional hernia",
          "endometriosis",
          "Inguinal hernia",
          "Umbilical hernia"
        ) ~ "Intermediate", 
        the_surgical_procedure %in% c(
        "Adrenal Cyst Excision",
         "Hiatal Hernia",
        "Exploratory laparotomy",
        "Gastric Mass Excision",
        "Heller Myotomy",
        "Hyadatid Cyst",
        "Pelvic Mass",
        "Renal Cyst Excision",
        "Sigmoid colectomy",
        "Spleenectomy",
        "Thyroidectomy",
        "Total Mastectomy"
        ) ~ "Major/Complex" ,
        
        TRUE  ~ "Unknown" #correcting for unknown
      ), ordered = TRUE , levels = c("Minor", "Intermediate",
                                    "Major/Complex")
    ), 
    BMI_cat = factor(
      case_when(
        bmi_kg_m_2 < 18.5 ~ "Underweight",
        bmi_kg_m_2 >= 18.5 & bmi_kg_m_2 < 25  ~ "Normal weight",
        bmi_kg_m_2 >= 25 & bmi_kg_m_2 < 30 ~ "Overweight", 
        bmi_kg_m_2 >= 30 & bmi_kg_m_2 <35 ~ "Obesity class I",
        bmi_kg_m_2 >= 35 & bmi_kg_m_2 < 40 ~ "Obesity class II",
        bmi_kg_m_2 >= 40 ~ "Obesity class III"
      ), ordered = TRUE , levels = c(
        "Underweight", "Normal weight", "Overweight", 
        "Obesity class I", "Obesity class II",
        "Obesity class III"
      )
    ), .after = bmi_kg_m_2
  )

required_analysis_cols <- c(
  "Gap",
  "Gap_case",
  "surgery_category",
  "BMI_cat"
)

missing_analysis_cols <- setdiff(required_analysis_cols, names(Surgeries_corrected_frame))
if (length(missing_analysis_cols) > 0) {
  stop(
    "Missing expected derived columns after grouping: ",
    paste(missing_analysis_cols, collapse = ", "),
    ". Check the mutation logic and column names."
  )
}

surgery_cat_table <- tbl_cross(Surgeries_corrected_frame, row = surgery_category,
                               col = gender,
                               label = list(
                                 gender = "Gender", 
                                 surgery_category = "Surgery Grade"
                               ), percent = "cell")
print(surgery_cat_table)


Obesity_tbl <- tbl_cross(
  data = Surgeries_corrected_frame, row = BMI_cat ,  col = gender,
  label = list(gender = "Gender",
              BMI_cat = "BMI category"), percent = "cell"
)

print(Obesity_tbl)

#here I start with wilcoxon tests 

global_wilcoxon <- wilcox.test( Surgeries_corrected_frame$average_expected_pain,
                                Surgeries_corrected_frame$average_post_operative_pain,
                                alternative = "two.sided",
                                paired = TRUE,
                                exact = TRUE,
                                conf.int = TRUE,
                                )
print(global_wilcoxon)

 subgroup_analysis  <- Surgeries_corrected_frame |>
   filter(!(is.na(Gap_case)))|>
   split(~surgery_category)|>
    map(function(df) {
     # 2. Check if the subgroup slice actually has enough pairs (at least 2)
     if (nrow(df) < 2) {
       return(paste("Skipped: Insufficient non-missing pairs (n =", nrow(df), ")"))
     }
     
     # 3. If it has enough data, run the test safely
     wilcox.test(
       df$average_expected_pain,
       df$average_post_operative_pain,
       alternative = "two.sided",
       paired = TRUE,
       exact = TRUE,
       conf.int = TRUE,
       mu = 0
     )
   })
print(subgroup_analysis)

subgroup_education <- Surgeries_corrected_frame|>
  filter(!is.na(Gap_case))|> #doing this to eliminate NAs
  split(~ education_level)|>
  map(  function(df) {
    if (nrow(df) < 2) {
      return(
        paste("Skipped : Insuffecient number of non misiing pairs (n =",nrow(df),")"))}
  else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0 ,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })
print(subgroup_education)

subgroup_bmi <- Surgeries_corrected_frame |>
  filter(!is.na(Gap_case))|>
  split(~BMI_cat)|>
  map(  function(df) {
    if (nrow(df) < 2) {
      return(
        paste("Skipped : Insuffecient number of non misiing pairs (n =",nrow(df),")"))}
    else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0 ,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_bmi)

subgroup_social_status <- Surgeries_corrected_frame |>
  filter(!is.na(Gap_case))|>
  split(~ social_status)|>
  map( function(df) {
    if (nrow(df) < 2) {
      return(
        paste("Skipped : Insuffecient number of non misiing pairs (n =",nrow(df),")"))}
    else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0 ,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_social_status)

subgroup_gender <- Surgeries_corrected_frame|>
  filter(!is.na(Gap_case))|>
  split(~ gender)|>
  map( function(df) {
    if (nrow(df) < 2) {
      return(
        paste("Skipped : Insuffecient number of non misiing pairs (n =",nrow(df),")"))}
    else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0 ,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_gender)

subgroup_analgesic_use <- Surgeries_corrected_frame|>
  filter(!is.na(Gap_case))|>
  split(~ analgesic_use)|>
  map( function(df) {
    if (nrow(df) < 2) {
      return(
        paste("Skipped : Insuffecient number of non misiing pairs (n =",nrow(df),")"))}
    else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0 ,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_analgesic_use)

#subgroup smoking
subgroup_smoking_cat <- Surgeries_corrected_frame |>
  split(~ smoking_cat)|>
  map( function(df) {
    if (nrow(df) < 2) {
      return(
        paste("Skipped : Insuffecient number of non misiing pairs (n =",nrow(df),")"))}
    else {
      wilcox.test(
        df$average_expected_pain,
        df$average_post_operative_pain,
        mu = 0 ,
        alternative = "two.sided",
        exact = TRUE,
        paired = TRUE,
        conf.int = TRUE
      )
    }
  })

print(subgroup_smoking_cat)

#Building a new dummy frame
Dummified_df <- Surgeries_corrected_frame |>
  mutate(num_anxiety = as.numeric(anxiety),
         num_sleep = as.numeric(patients_sleep_quality),
         num_mood = as.numeric(patients_mood),
         num_surgery_cat = as.numeric(surgery_category),
         num_education = as.numeric(education_level),
         anaesthesia = factor(anaesthesia),
         analgesics_plan = factor(analgesics_plan),
         num_smoking_cat = as.numeric(smoking_cat)
         )

clean_dum <- na.omit(Dummified_df)
# building a model


#buildind model containing everything
multinom_model_1 <- multinom(Gap_case ~ num_anxiety +  num_sleep + num_mood 
                             + surgery_category + num_education + num_smoking_cat 
                             + any_allergies+ any_past_surgical_procedures+
                               as.numeric(BMI_cat) +
                               anaesthesia+ metropolitan_or_rural
                             + num_education + analgesic_use+ gender +age  ,
                             data = Dummified_df, Hess = TRUE)
tbl_regression_1 <- tbl_regression(multinom_model_1,
                                   exponentiate = TRUE, conf.int = TRUE)
print(tbl_regression_1)

predictors <- c("num_anxiety"  ,                "num_sleep"         ,     
                "num_mood"  ,                  
                "num_surgery_cat"  ,            "num_education"   ,      
                "total_smoking" ,              
                 "any_allergies" ,    "analgesic_use" ,         
                  "any_past_surgical_procedures",
                "bmi_kg_m_2"                  
                ) # These I'll use lasso on to eliminate
lasso_prep_df <- clean_dum|>
  select(all_of(predictors), Gap_case)|>
  filter(if_all(all_of(predictors),~ !is.na(.)))|>
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
#It didn't work out well due to lack of numbers

#I'll try to fit a two vars model
Multinom_2 <- multinom(Gap_case ~ num_anxiety + analgesic_use + num_mood + surgery_category + num_sleep,
                       data = Dummified_df, Hess = TRUE)
tbl_regression_2 <- tbl_regression(Multinom_2, 
                                   exponentiate = TRUE, conf.int = TRUE)
print(tbl_regression_2)



multinom_3 <- multinom(Gap_case ~ num_sleep + analgesic_use + surgery_category +num_mood + analgesics_plan +num_anxiety
                       , data = Dummified_df, Hess = TRUE)
tbl_regression_3 <- tbl_regression(multinom_3, 
                                   exponentiate = TRUE, conf.int = TRUE)
print(tbl_regression_3)

#This works well , I tried to use performance library to get a model from 2 without colinearity
library(performance)
check_collinearity(multinom_model_1) # There's high instability 

multinom_safe <- multinom(
       Gap_case ~ num_sleep + num_anxiety + analgesic_use , 
       data = Dummified_df
       , Hess = TRUE)
tbl_safe <- tbl_regression(multinom_safe, exponentiate = TRUE)
print(tbl_safe)


#now let's do some plots
num_sleep_pred <- ggemmeans(multinom_3, terms = "num_sleep")
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
 surgery_cat_pred <- ggemmeans(multinom_3, terms = "surgery_category")
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
  analgesic_use_pred <- ggemmeans(multinom_3, terms = "analgesic_use ")
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
      x ="Analgesics use",
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



anxiety_pred <- ggemmeans(multinom_4, terms = "num_anxiety")
 anxiety_pred_plot <- ggplot(data = anxiety_pred, aes(x = x , y = predicted, color = response.level))+
   geom_line()+
   scale_color_brewer(palette = "Set1")+
   labs(title = "Estimation Category vs Anxiety", subtitle = "Estimation Category Expectation plotted against Anxiety Score", x = "Anxiety" , y = "Estimation category prediction")+
   theme_minimal(base_size = 14)+
   theme_minimal(base_size = 14)+
       theme(
             plot.title = element_text(face = "bold", size = 15, margin = margin(b = 8)),
             plot.subtitle = element_text(size = 11, color = "gray40"),
             legend.position = "bottom",
             panel.grid.minor = element_blank(),
             panel.border = element_rect(color = "gray85", fill = NA, linewidth = 0.5)
         )

