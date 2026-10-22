# The scripts general plan

In Thesis analysis folder and humam edited script of original thesis scripts( which we made together before starting this repo), the script got to around thousands of lines in just short time.
So in order to facilitate navigation, in this folder I divided the workflow into seperate scripts to make the wrok modeular.
Before running any of the scripts, make sure to source **"DF prep script.R"** script first as it contains all datasets used for this study. I avoided writing exportation scripts inside  the scripts so that it's not  replicated accidently in reviewer machine.

## Tests used 

I made 3 Summary tables for the cohort of my study description, I also used wilcoxon signed rank paired test to compare expectation with real average postoperative pain, then for explorartory reasons I did subgroup analysis for this test.
I imputed the data using ***MICE*** , did a lasso model on the imputation to determine variables suited for keeping in the model, then  made a multinomial model on imputed data.

Finally, I did a partial cumulative odds model after selecting variables through ***ordinalNet*** to see which factors affect prediction of post-opertive average pain.
