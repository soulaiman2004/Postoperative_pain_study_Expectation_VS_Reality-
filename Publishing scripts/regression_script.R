# we will now fit a lasso model 
library(glmnet)

selected_vars_list <- list()

# now let's build the loop
for(i in 1:40){
  # selecting the imputed data set
  df_complete <- complete(imputed_Dummified_df, action = i)
  x <- model.matrix(Estimation_status ~ . -1, data = df_complete)
  y <- df_complete$Estimation_status

  #run cross-validated lasso
  cv_fit <-cv.glmnet(x, y, family = "multinomial", alpha = 1)
 # 4. Extract coefficients safely (handles matrix or list formats)
  coefs <- coef(cv_fit, s = "lambda.min")
  if (is.list(coefs)) coefs <- coefs[[1]]
  
  # 5. Save surviving variables with non-zero coefficients (excluding intercept)
  surviving <- rownames(coefs)[which(coefs[, 1] != 0)]
  selected_vars_list[[i]] <- setdiff(surviving, "(Intercept)")}

# Check selection frequency across all 40 datasets
table(unlist(selected_vars_list)) / 40

#now I'll build a model with those with coeffecients here
# ==========================================
# Variables >= 0.5 (Selected / Above Threshold)
# ==========================================
# age_cat.C                       : 1.000
# analgesic_use                   : 1.000
# analgesics_planTypical+ tramadol: 1.000
# education_level.C               : 0.875
# metropolitan_or_ruralRegional   : 1.000
# num_anxiety                     : 1.000
# num_surgery_cat                 : 1.000

# ==========================================
# Variables < 0.5 (Filtered Out / Below Threshold)
# ==========================================
# anaesthesiaLumbar anaesthesia   : 0.200
# bmi_kg_m_2                      : 0.175
# num_sleep                       : 0.050
# surgical_procedure_duration     : 0.075
# Using_prescription_drugsYes     : 0.175

library(nnet)


Multinom_post_lasso <- with(imputed_Dummified_df, multinom(Estimation_status ~ age_cat + analgesic_use+
  metropolitan_or_rural +num_anxiety + num_surgery_cat + analgesics_plan + education_level, maxit = 500))

# Let's pool it now
pooled_Multinom <- pool(Multinom_post_lasso)
summary(pooled_Multinom, exponentiate = TRUE, conf.int = TRUE)
