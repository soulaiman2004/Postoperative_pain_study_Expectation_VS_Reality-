# This file is for sample size calculation recreation for primary outcome
library(effectsize)
library(pwrss)
#I'll use pilot_df from my Df prep script
# I'll run a Wilcoxon signed rank paired test on pilot data
avg_pre_op_pain <- pilot_df$average_expected_pain
avg_post_op_pain <- pilot_df$average_post_operative_pain

pilot_wilcox <- wilcox.test(avg_pre_op_pain, avg_post_op_pain, alternative = "two.sided",
  paired = TRUE)
print(pilot_wilcox)
 r = rank_biserial(pilot_df$average_expected_pain, pilot_df$average_post_operative_pain, paired = TRUE, alternative = "two.sided")
print(paste0("The value of matched-pairs rank-biserial coeffecient: ",round(r$r_rank_biserial , 3)))

sample_size <- power.np.wilcoxon(d = abs(r$r_rank_biserial), power = 0.9, alpha = 0.05, design = "paired" )

