# we will now fit a lasso model 
library(glmnet)

selected_vars_list <- list()

# now let's build the loop
for(i in 1:100){
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
table(unlist(selected_vars_list)) / 100

#now I'll build a model with those with coeffecients here
# Stability Selection Inclusion Frequencies:
# --------------------------------------------------
# analgesic_use                        : 1.00
# analgesics_planTypical+ tramadol     : 0.93
# anxiety.L                            : 0.08
# anxiety.Q                            : 1.00
# bmi_kg_m_2                           : 0.25
# education_level.C                    : 1.00
# metropolitan_or_ruralNon-Metropolitan: 0.01
# patients_mood.C                      : 0.36
# patients_mood.L                      : 0.01
# patients_sleep_quality.C             : 1.00
# patients_sleep_quality^4             : 1.00
# Surgery_cat.L                        : 1.00
# surgical_procedure_duration          : 0.30
# Using_prescription_drugsYes          : 0.05
library(nnet)


Multinom_post_lasso <- with(imputed_Dummified_df, multinom(Estimation_status ~   analgesic_use + analgesics_plan + 
  anxiety + education_level +patients_sleep_quality + Surgery_cat, maxit = 500))

# Let's pool it now
pooled_Multinom <- pool(Multinom_post_lasso)
publication_df_of_model <- as.data.frame(summary(pooled_Multinom, exponentiate = TRUE, conf.int = TRUE))

table_of_model <- publication_df_of_model|>
  mutate(# Combine estimate and 95% CI using sprintf
    aor_ci = sprintf("%.2f (%.2f – %.2f)", estimate, conf.low, conf.high),
    
    # Clean up p-values (e.g., < 0.001 formatting)
    p_formatted = case_when(
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", p.value)
    ),
    
    # Optional: Clean up term names if you want friendlier labels
    term_clean = case_when(
      term == "analgesic_use" ~ "Analgesic Use (Yes vs. No)",
      term == "analgesics_planTypical+ tramadol" ~ "Analgesics Plan (Typical + Tramadol)",
      grepl("anxiety", term) ~ paste0("Anxiety (", sub(".*\\.", "", term), ")"),
      grepl("education_level", term) ~ paste0("Education Level (", sub(".*\\.", "", term), ")"),
      grepl("patients_sleep_quality", term) ~ paste0("Sleep Quality (", sub(".*\\.", "", term), ")"),
      grepl("Surgery_cat", term) ~ paste0("Surgery Category (", sub(".*\\.", "", term), ")"),
      term == "(Intercept)" ~ "Intercept",
      TRUE ~ term
    )
  ) %>%
  select(term_clean, aor_ci, p_formatted) %>%
  rename(
    "Variable / Term" = term_clean,
    "Adjusted Odds Ratio (95% CI)" = aor_ci,
    "p-value" = p_formatted
  )
pub_flextable <- flextable(table_of_model) %>%
  set_caption("Table: Predictors of Post-Operative Pain Expectations (Multinomial Logistic Regression)") %>%
  theme_booktabs() %>%
  autofit()

library(forestplot)
library(dplyr)

# 1. Prepare your data frame directly from your pooled summary
plot_df <- summary(pooled_Multinom, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    # Create clean variable labels
    variable = case_when(
      term == "analgesic_use" ~ "Analgesic Use",
      term == "analgesics_planTypical+ tramadol" ~ "Analgesics Plan (Typical + Tramadol)",
      grepl("anxiety", term) ~ paste0("Anxiety (", sub(".*\\.", "", term), ")"),
      grepl("education_level", term) ~ paste0("Education Level (", sub(".*\\.", "", term), ")"),
      grepl("patients_sleep_quality", term) ~ paste0("Sleep Quality (", sub(".*\\.", "", term), ")"),
      grepl("Surgery_cat", term) ~ paste0("Surgery Category (", sub(".*\\.", "", term), ")"),
      TRUE ~ term
    ),
    # Format text columns for the table side of the forest plot
    or_ci = sprintf("%.2f (%.2f – %.2f)", estimate, conf.low, conf.high)
  )

# 2. Build the text matrix table layout (Labels + Numbers on the left side)
tabletext <- cbind(
  c("Variable", plot_df$variable),
  c("aOR (95% CI)", plot_df$or_ci),
  c("p-value", ifelse(plot_df$p.value < 0.001, "< 0.001", sprintf("%.3f", plot_df$p.value)))
)

# 3. Combine with a dummy row at the top for headers/na values for the graphical side
# forestplot expects the estimate, lower, and upper bounds for the plot points
mean_vals <- c(NA, plot_df$estimate)
lower_vals <- c(NA, plot_df$conf.low)
upper_vals <- c(NA, plot_df$conf.high)

# 4. Render the Forest Plot
forestplot(
  labeltext = tabletext,
  mean = mean_vals,
  lower = lower_vals,
  upper = upper_vals,
  xlog = TRUE,                     # Sets logarithmic scale for Odds Ratios
  zero = 1,                        # Null line reference at 1.0
  boxsize = 0.2,                   # Size of the square markers
  col = fpColors(box = "#1A365D", line = "#4A5568", summary = "black"),
  xlab = "Adjusted Odds Ratio (log scale)"
)
