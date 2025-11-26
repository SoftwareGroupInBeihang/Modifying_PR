library(lmerTest)
library(lme4)
library(MuMIn)
library(dplyr)

# Read dataset
setwd("C:\\dataset")
d <- read.csv("regression_ready_data.csv")

# Take the logarithm of and standardize the numerical variables.
numeric_vars <- c("repo_age", "repo_commits", "author_coding_experience",
                 "open_tasks", "pull_lines", "pull_desc_len",
                 "reviewer_coding_experience", "reviewer_review_experience")

d_processed <- d %>%
  mutate(across(all_of(numeric_vars), ~ log(.x + 0.5))) %>%
  mutate(across(all_of(numeric_vars), ~ as.numeric(scale(.x))))

# Model1 for PR acceptance
model_acceptance <- glmer(merged ~
    edited + # modified_or_not
    repo_age +
    repo_commits +
    author_coding_experience +
    open_tasks +
    pull_lines +
    pull_desc_len +
    reviewer_coding_experience +
    reviewer_review_experience +
    author_role_COLLABORATOR +
    author_role_CONTRIBUTOR +
    author_role_MEMBER +
    author_role_NONE +
    reviewer_role_COLLABORATOR +
    reviewer_role_CONTRIBUTOR +
    reviewer_role_MEMBER +
    reviewer_role_NONE +
    category_Motivation +
    category_Implementation +
    category_Quality +
    category_Problem +
    category_Miscellaneous +
    PR_types_Adaptive_Maintenance +
    PR_types_Code_Formatting +
    PR_types_Corrective_Maintenance_._Fix +
    PR_types_Feature_Addition +
    PR_types_Feature_Improvement +
    PR_types_Language_Compatibility +
    PR_types_Library_Compatibility +
    PR_types_Module_Management +
    PR_types_New_Release +
    PR_types_Non.source_Code_Change +
    PR_types_SCS_Management +
    PR_types_Testing +
    PR_types_Usage_Example +
    (1 | repo),
    data = d_processed,
    family = binomial(link = "logit")
)

print(summary(model_acceptance))
print(r.squaredGLMM(model_acceptance))

print("----------------------------------------")

# Model2 for PR review time
model_review_time <- lmer(log(close_time / 60 / 60 / 24 + 0.5) ~
    edited + # modified_or_not
    repo_age +
    repo_commits +
    author_coding_experience +
    open_tasks +
    pull_lines +
    pull_desc_len +
    reviewer_coding_experience +
    reviewer_review_experience +
    author_role_COLLABORATOR +
    author_role_CONTRIBUTOR +
    author_role_MEMBER +
    author_role_NONE +
    reviewer_role_COLLABORATOR +
    reviewer_role_CONTRIBUTOR +
    reviewer_role_MEMBER +
    reviewer_role_NONE +
    category_Motivation +
    category_Implementation +
    category_Quality +
    category_Problem +
    category_Miscellaneous +
    PR_types_Adaptive_Maintenance +
    PR_types_Code_Formatting +
    PR_types_Corrective_Maintenance_._Fix +
    PR_types_Feature_Addition +
    PR_types_Feature_Improvement +
    PR_types_Language_Compatibility +
    PR_types_Library_Compatibility +
    PR_types_Module_Management +
    PR_types_New_Release +
    PR_types_Non.source_Code_Change +
    PR_types_SCS_Management +
    PR_types_Testing +
    PR_types_Usage_Example +
    (1 | repo),
    data = d_processed
)

print(summary(model_review_time))
print(r.squaredGLMM(model_review_time))