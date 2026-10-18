library(ordinalNet)
library(tidyverse)

# select variables used in the selection 
analysis_df <- post_operative_df_ordinal[
  ,c(
    "ord_post_operative_average",
    "gender",
    "age_cat",
    "smoking_cat",
    "analgesic_use",
    "analgesics_plan",
    "Surgery_cat",
    "Using_prescription_drugs",
    "Presence_of_chronic_disease",
    "procedure_type",
    "average_expected_pain"
  )
]|>
  mutate(procedure_type = factor(procedure_type))|>
  na.omit()

x = model.matrix(ord_post_operative_average ~ . -1, data = analysis_df)
y = analysis_df$ord_post_operative_average

#now starts the cross validation
set.seed(1535)
k <- 15
fold_ids <- sample(1:k , nrow(analysis_df), replace = TRUE)

cv_accuracies <- numeric(k)
global_preds <- c()
global_trues <- c()

for(i in 1:k){
  train_x <- x[fold_ids != i, ,drop = FALSE]
  train_y <- y[fold_ids !=i]

  test_x <- x[fold_ids == i, ,drop = FALSE]
  test_y <- y[fold_ids ==i ]

  # fit the ordinalNet model 
  fit_ordinal <- ordinalNet(
    x = train_x,
    y = train_y,
    family = "cumulative",
    link = "logit",
    alpha = 1
  )
  preds_numeric <- predict(fit_ordinal,newx = test_x, type = "class", criteria = "aic")
  preds_labels <- factor(
    levels(test_y)[preds_numeric],
    levels = levels(test_y), 
    ordered = TRUE
  )
  #calculate fold accuracies 
  cv_accuracies[i] <- mean(preds_labels ==test_y)
  #accumulate for confusion matrix
  global_preds <-c(global_preds, as.character(preds_labels))
  global_trues<- c(global_trues, as.character(test_y))
}
#evalation results 
global_preds <- factor(global_preds, levels = levels(y), ordered = TRUE)
global_trues <- factor(global_trues, levels = levels(y), ordered = TRUE)
#Print Summary Metrics
cat("\n--- Fold-by-Fold Accuracies ---\n")
print(round(cv_accuracies, 3))
cat("Mean CV Accuracy:", round(mean(cv_accuracies) * 100, 2), "%\n")

cat("\n--- Global Confusion Matrix (Rows = True, Cols = Predicted) ---\n")
conf_matrix <- table(True = global_trues, Predicted = global_preds)
print(conf_matrix)

global_accuracy <- mean(global_preds == global_trues)
cat("\nOverall Global Accuracy:", round(global_accuracy * 100, 2), "%\n")

# Fit ordinalNet on the ENTIRE dataset using Lasso (alpha = 1)
final_full_fit <- ordinalNet(
  x = x,
  y = y,
  family = "cumulative",
  link = "logit",
  alpha = 1
)

# Extract coefficients at the AIC-selected penalty level
final_coefs <- coef(final_full_fit, criteria = "aic")

# Filter out zero coefficients and intercept thresholds (which start with 'zeta')
survived_vars <- final_coefs[final_coefs != 0]
survived_vars <- survived_vars[!grepl("^zeta", names(survived_vars))]

cat("\n--- Variables That Survived Lasso Selection ---\n")
print(survived_vars)

#now I'll fit a polr model to using my surviving vars 

library(ordinal)
 final_polr_partial <- clm(
  ord_post_operative_average ~ age_cat  + analgesics_plan  + average_expected_pain ,
  nominal = ~ procedure_type + smoking_cat,
  data = analysis_df,
  Hess = TRUE
 )

publication_table <- tidy(final_polr_partial, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    # Calculate two-tailed p-value from the statistic
    p.value = 2 * (1 - pnorm(abs(statistic))),
    
    # Format p-values nicely for clinical publication (e.g., "< 0.001")
    p_formatted = case_when(
      is.na(p.value) ~ NA_character_, # Keeps scale intercepts clean
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  )

print(publication_table)
