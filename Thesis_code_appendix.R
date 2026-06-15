## ----work directory, include=FALSE-----------------------------------
setwd("C:/Courses/Thesis/R_code")


## ----setup, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
  eval = TRUE,
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = FALSE,
  include = TRUE,
  tidy = TRUE,
  tidy.opts = list(width.cutoff = 60),
  fig.align = "center",
  fig.width = 7,
  fig.height = 5,
  fig.pos = "H"
)
# Important libraries
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(readxl)
library(skimr)
library(naniar)
library(GGally)
library(gt)
library(gtsummary)
library(tidyLPA)
library(mclust)
library(poLCA)
library(nnet)
library(tableone)
library(janitor)
library(caret)
library(broom)
library(mice)
library(glmnet)


## ----data_loading----------------------------------------------------
pride <- read.csv("pride_data.csv",
                  header = TRUE,
                  sep = ";",
                  dec = ",",
                  na.strings = c("NA", ""))


## ----Data preparation------------------------------------------------
## Create types of GDM
analysis_data <- pride %>%
  mutate(
    GDM_type = case_when(
      v2g_SerumGlucose0Mins >= 5.1 & v2g_SerumGlucose120Mins >= 8.5 ~ "type-3GDM",
      v2g_SerumGlucose0Mins >= 5.1 & (is.na(v2g_SerumGlucose120Mins ) | v2g_SerumGlucose120Mins < 8.5) ~ "type-1GDM",
      v2g_SerumGlucose120Mins >= 8.5 & (is.na(v2g_SerumGlucose0Mins ) | v2g_SerumGlucose0Mins < 5.1) ~ "type-2GDM",
      (v2g_SerumGlucose0Mins < 5.1 & v2g_SerumGlucose120Mins < 8.5) | (v2g_SerumGlucose0Mins < 5.1 & (is.na(v2g_SerumGlucose120Mins))) | (v2g_SerumGlucose120Mins < 8.5 & (is.na(v2g_SerumGlucose0Mins))) ~ "Non-GDM",
      
     
    )
  )

# ## Create a dataset containe important varibles only
# analysis_data <- analysis_data %>%
#   dplyr::select(
#     age,
#     BMI,
#     HbA1C_perc,
#     v1r_Inc_FH,
#     V1_gestAgecalcu_new,
#     CHOL,
#     TRL,
#     HDL,
#     parity,
#     ethnicity,
#     v1_EQol5D_score,
#     V1_WEMWBS_Score,
#     V1_WEMWBS_Score_cat,
#     V1_GAD7Anxiety_Score,
#     V1_GAD7Anxiety_Score_cat,
#     V1_PHQ9_depression_Score,
#     V1_PHQ9_depression_Score_cat,
#     v1_total_MET_IPAQ,
#     V1_se_MaritalStatus_new,
#     V1_se_EmploymentStatus_new,
#     V1_se_HouseholdIncome_new,
#     GDM_type
#   )

## Rename variables
analysis_data <- analysis_data %>%
  rename(
    `HbA1c(%)` = HbA1C_perc,
    `Family history` = v1r_Inc_FH,
    `Gestational Age` = V1_gestAgecalcu_new,
    `Previous Births` = parity,
    Ethnicity = ethnicity,
    `QoL Score` = v1_EQol5D_score,
    `Wellbeing Score` = V1_WEMWBS_Score,
    `Wellbeing Cat` = V1_WEMWBS_Score_cat,
    `Anxiety Score` = V1_GAD7Anxiety_Score,
    `Anxiety Cat` = V1_GAD7Anxiety_Score_cat,
    `Depression Score` = V1_PHQ9_depression_Score,
    `Depression Cat` = V1_PHQ9_depression_Score_cat,
    `Physical Activity` = v1_total_MET_IPAQ,
    `Marital Status` = V1_se_MaritalStatus_new,
    Employment = V1_se_EmploymentStatus_new,
    Income = V1_se_HouseholdIncome_new,
    Cholesterol = CHOL,
    `High-Density Lipoprotein` = HDL,
  )
analysis_data <- analysis_data %>%
  mutate(`Family history` = recode(`Family history`, `1` = "yes", `2` = "no")
         )


analysis_data <- analysis_data %>%
  mutate(
    `Previous Births` = factor(
      if_else(`Previous Births` >= 2, "2+", as.character(`Previous Births`)),
      levels = c("1", "2+")
    )
  )



analysis_data <- analysis_data %>%
  mutate(
    QoL_quartile = ntile(`QoL Score`, 4),

    `QoL cat` = factor(
      case_when(
        QoL_quartile == 1 ~ "Low",
        QoL_quartile %in% c(2, 3) ~ "Moderate",
        QoL_quartile == 4 ~ "High",
        TRUE ~ NA_character_
      ),
      levels = c("Low", "Moderate", "High")
    )
  )

analysis_data <- analysis_data %>%
  mutate(
    `Physical Activity1` = log(`Physical Activity` + 1)
  )


analysis_data <- analysis_data %>%
  mutate(
    Triglycerides = log(TRL + 1)
  )

analysis_data <- analysis_data %>%

  filter(!is.na(GDM_type))




## ----Demo_clinical_Obs_GDM, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE----
library(dplyr)
library(gtsummary)
library(kableExtra)

Demo <- analysis_data %>%
  dplyr::filter(!is.na(GDM_type)) %>%
  tbl_strata(
    strata = GDM_type,
    .tbl_fun = ~ tbl_summary(
      .x,
      include = c(
        age,
        BMI,
        Ethnicity,
        `Gestational Age`,
        `HbA1c(%)`,
        `Family history`,
        `Previous Births`
      ),
      missing = "ifany",
      missing_text = "Missing",
      missing_stat = "{N_miss} ({p_miss}%)",
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      )
    )
  ) %>%
  modify_caption(
    "Demographic, Clinical and Obstetric Characteristics by GDM Type"
  ) %>%
  bold_labels()

Demo %>%
  as_kable_extra(
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  )


## ----Biochemical_Metabolic_GDM, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE----
library(dplyr)
library(gtsummary)
library(kableExtra)

Bio <- analysis_data %>%
  filter(!is.na(GDM_type)) %>%
  tbl_strata(
    strata = GDM_type,
    .tbl_fun = ~ tbl_summary(
      .x,
      include = c(
        Cholesterol,
        `High-Density Lipoprotein`,
        Triglycerides,
        `Physical Activity`
      ),
      missing = "ifany",
      missing_text = "Missing",
      missing_stat = "{N_miss} ({p_miss}%)",
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      )
    )
  ) %>%
  modify_caption(
    "Biochemical and Metabolic Measurements by GDM Type"
  ) %>%
  bold_labels()

Bio %>%
  as_kable_extra(
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  )


## ----Psychosocial_Socioeconomic, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE----


Psycho <- analysis_data %>%
  dplyr::filter(!is.na(GDM_type)) %>%
  tbl_strata(
    strata = GDM_type,
    .tbl_fun = ~ tbl_summary(
      .x,
      include = c(
        `QoL cat`,
        `Wellbeing Score`,
        `Anxiety Cat`,
        `Depression Cat`,
        `Marital Status`,
        Employment,
        Income
      ),
      missing = "ifany",
      missing_text = "Missing",
      missing_stat = "{N_miss} ({p_miss}%)",
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      )
    )
  ) %>%
  modify_caption("Psychosocial and Socioeconomic Characteristics by GDM Type") %>%
  bold_labels()

Psycho %>%
  as_kable_extra(
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  )


## ----preparing analysisy, results='hide'-----------------------------
## Preparing data for analysis
library(VarSelLCM)

 # Select variable
 mixed_data <- analysis_data %>%
   dplyr::select(
      age,
      BMI,
     `HbA1c(%)`,
     `Family history`,
     `Gestational Age`,
      Cholesterol,
      Triglycerides,
     `High-Density Lipoprotein`,
     `Previous Births`,
      Ethnicity,
     `QoL cat`,
     `Wellbeing Score`,
     `Anxiety Cat`,
     `Depression Cat`,
     `Physical Activity`,
     `Marital Status`,
      Employment,
      Income
   )

 # Continuous variables
 continuous_vars <- c(
   "age",
   "BMI",
   "HbA1c(%)",
   "Gestational Age",
   "Cholesterol",
   "Triglycerides",
   "High-Density Lipoprotein",
   "Wellbeing Score",
   "Physical Activity"
 )

 # Standardize continuous variables
 mixed_data[continuous_vars] <- scale(mixed_data[continuous_vars])

 # Convert categorical variables to factor
 mixed_data <- mixed_data %>%
   mutate(
     across(
       -all_of(continuous_vars),
       as.factor
     )
   )

 # Check for highly correlation
 library(corrplot)
 cor_mat <- cor(mixed_data[continuous_vars], use = "complete.obs")

 upper_tri <- cor_mat
 upper_tri[lower.tri(upper_tri, diag = TRUE)] <- NA

 high_corr_pairs <- which(abs(upper_tri) > 0.7, arr.ind = TRUE)

 data.frame(
   var1 = rownames(upper_tri)[high_corr_pairs[,1]],
   var2 = colnames(upper_tri)[high_corr_pairs[,2]],
   corr = upper_tri[high_corr_pairs]
 )

cor_mat


## ----final model, results='hide',cache=TRUE--------------------------
## Fit generalized latent class model for 5 classes
set.seed(123)

Model_with <- VarSelCluster(x = mixed_data, gvals = 5, vbleSelec = TRUE)

summary(Model_with)

# Criteria
Model_with@criteria@BIC
Model_with@criteria@AIC
Model_with@criteria@loglikelihood

# Model
Model_with@model@names.irrelevant

# Parameters 
Model_with@param@paramContinuous@mu
Model_with@param@paramCategorical@alpha



# Partitions
table(Model_with@partitions@zMAP)

# Create the subtype variable 
analysis_data$class <- Model_with@partitions@zMAP
table(analysis_data$Transformed_class)



## ----Final-group-size, echo=FALSE, message=FALSE, warning=FALSE------
class_accuracy <- data.frame(
  `Latent Group` = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
   `n` = c(738, 1157, 997, 565, 846),
   `Percentage` = round(c(738, 1157, 997, 565, 846) / 4303 * 100, 1)
  
)

kable(
  class_accuracy,
  caption = "Final group size",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----Mode-selection-table, echo=FALSE, message=FALSE, warning=FALSE----
class_accuracy <- data.frame(
  `Latent Group` = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
  `BIC value` = c(-81534.63, -75651.20, -74680.42, -74123.84, -73695.82)
)

kable(
  class_accuracy,
  caption = "BIC values of Model selection",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----latent-class-labels, echo=FALSE, message=FALSE, warning=FALSE----
library(knitr)
library(kableExtra)

class_labels <- data.frame(
  Group = c(
    "Group 1",
    "Group 2",
    "Group 3",
    "Group 4",
    "Group 5"
  ),
  `Proposed Label` = c(
    "Metabolic and psychosocial risk subtype",
    "Obesity-associated metabolic subtype with favorable psychosocial well-being",
    "Metabolically favorable and psychosocially resilient subtype",
    "Psychosocially vulnerable subtype",
    "Physically active and psychosocially healthy subtype"
  )
)

kable(
  class_labels,
  caption = "Clinical interpretation of the identified latent subtypes",
  booktabs = TRUE,
  align = c("c", "l")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down")
  )



## ----Heatmap, fig.width=7, fig.height=5, fig.cap="Heatmap showing standardized mean values (z-scores) of continuous variables across the five latent classes. Red cells indicate values above the overall sample mean, whereas blue cells indicate values below the overall sample mean.", cache=TRUE----
mu <- as.data.frame(Model_with@param@paramContinuous@mu)

mu$Variable <- rownames(mu)
mu_long <- mu %>%
  tidyr::pivot_longer(
    cols = starts_with("class"),
    names_to = "Class",
    values_to = "Mean"
  )

mu_long$Variable <- factor(
  mu_long$Variable,
  levels = rev(c(
    "age",
    "BMI",
    "HbA1c(%)",
    "Gestational Age",
    "Cholesterol",
    "Triglycerides",
    "High-Density Lipoprotein",
    "Physical Activity",
    "Wellbeing Score"
  ))
)

ggplot(mu_long,
       aes(x = Class,
           y = Variable,
           fill = Mean)) +
  
  geom_tile(color = "white") +
  
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  
  labs(
    title = "Latent group profiles",
    x = "Latent Group",
    y = NULL,
    fill = "z-score"
  ) +
  
  theme_minimal(base_size = 14)


## ----conditional independence, include=FALSE-------------------------
## continues variables
analysis_data %>%
  dplyr::filter(class == 1) %>%
  dplyr::select(`Anxiety Score`, `Depression Score`) %>%
  cor(use = "pairwise.complete.obs")

## Categorical variables
library(lsr)
class1 <- analysis_data %>%
  dplyr::filter(class == 1)

tab <- table(class1$Employment, class1$Income)

cramersV(tab)


## ----GDM type distribution, include=FALSE----------------------------
tab <- table(analysis_data$class,
             analysis_data$GDM_type)

tab

round(prop.table(tab, margin = 1) * 100, 1)

chisq.test(tab)

library(lsr)
cramersV(tab)


## ----Prediction, results='hide', cache=TRUE, include=FALSE-----------
# Create dataset
all_predictors <- c(
  "age",
  "BMI",
  "HbA1c(%)",
  "Gestational Age",
  "Cholesterol",
  "Triglycerides",
  "High-Density Lipoprotein",
  "Wellbeing Score",
  "Physical Activity",
  "Family history",
  "Previous Births",
  "Ethnicity",
  "QoL cat",
  "Anxiety Cat",
  "Depression Cat",
  "Marital Status",
  "Employment",
  "Income"
)

categorical_vars <- c(
  "Family history",
  "Previous Births",
  "Ethnicity",
  "QoL cat",
  "Anxiety Cat",
  "Depression Cat",
  "Marital Status",
  "Employment",
  "Income",
  "class"
)

analysis_data <- analysis_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(categorical_vars),
      as.factor
    )
  )



prediction_data <- analysis_data %>%
  dplyr::select(dplyr::all_of(all_predictors), class) %>%
  dplyr::rename(
    HbA1c = `HbA1c(%)`,
    Gestational_Age = `Gestational Age`,
    Cholesterol = Cholesterol,
    Triglycerides = Triglycerides,
    HDL = `High-Density Lipoprotein`,
    Wellbeing_Score = `Wellbeing Score`,
    Physical_Activity = `Physical Activity`,
    Family_history = `Family history`,
    Previous_Births = `Previous Births`,
    QoL_cat = `QoL cat`,
    Anxiety_Cat = `Anxiety Cat`,
    Depression_Cat = `Depression Cat`,
    Marital_Status = `Marital Status`
  )

# Imputation 
imp <- mice(
  prediction_data,
  m = 5,
  seed = 123
)


## ----Convergence, results='hide', include=FALSE----------------------
# Convergence
plot(imp)

imp$method
summary(imp)

densityplot(imp, ~ HbA1c)
densityplot(imp, ~ Cholesterol)
densityplot(imp, ~ Triglycerides)
densityplot(imp, ~ HDL)
densityplot(imp, ~ Physical_Activity)
densityplot(imp, ~ Wellbeing_Score)


stripplot(imp, QoL_cat ~ .imp)

stripplot(imp, Employment ~ .imp)

stripplot(imp, Income ~ .imp)


## ----Extract imputed data, results='hide', cache=TRUE, include=FALSE----
imputed_datasets <- lapply(
  1:5,
  function(i) complete(imp, i)
)
length(imputed_datasets)


## ----Fit Multinomial LASSO, results='hide', cache=TRUE, include=FALSE----
lasso_models <- lapply(
  imputed_datasets,
  function(dat){

    dat$class <- as.factor(dat$class)

    x <- model.matrix(class ~ ., data = dat)[,-1]
    y <- dat$class

    cv.glmnet(
      x,
      y,
      family = "multinomial",
      alpha = 1,
      nfolds = 10,
      type.measure = "class"
    )
  }
)
class(lasso_models[[1]])

coef(lasso_models[[1]], s = "lambda.1se")


## ----Compare accuracy, results='hide', cache=TRUE, include=FALSE-----
full_cv_accuracy <- sapply(
  lasso_models,
  function(model) {
    idx <- which(model$lambda == model$lambda.1se)
    1 - model$cvm[idx]
  }
)

full_cv_accuracy
mean(full_cv_accuracy)
sd(full_cv_accuracy)


## ----Extraction, results='hide', include=FALSE,cache=TRUE------------
selected_vars <- lapply(
  1:5,
  function(i){

    coefs <- coef(
      lasso_models[[i]],
      s = "lambda.1se"
    )

    unique(unlist(
      lapply(coefs, function(x){

        vars <- rownames(x)[as.numeric(x) != 0]

        vars[vars != "(Intercept)"]
      })
    ))
  }
)

selection_freq <- sort(
  table(unlist(selected_vars)),
  decreasing = TRUE
)

selection_freq


## ----Coefficient magnitude, include=FALSE, results='hide'------------
lasso_models[[1]]$lambda.1se
lasso_models[[2]]$lambda.1se
lasso_models[[3]]$lambda.1se
lasso_models[[4]]$lambda.1se
lasso_models[[5]]$lambda.1se

sapply(lasso_models, function(x) x$lambda.1se)


## ----Accuracy percentage, results='hide', include=FALSE, cache=TRUE----
Mean_accuracy = 85.3
SD = 0.4


## ----Sensitivity analysis, results='hide', include=FALSE, cache=TRUE----
reduced_predictors <- c(
  "age",
  "BMI",
  "HbA1c",
  "Gestational_Age",
  "Cholesterol",
  "Triglycerides",
  "HDL",
  "Family_history",
  "Previous_Births",
  "Ethnicity",
  "Marital_Status",
  "Employment",
  "Income"
)

reduced_data <- prediction_data %>%
  dplyr::select(dplyr::all_of(reduced_predictors), class)

# Imputation 
reduced_imp <- mice(
  reduced_data,
  m = 5,
  seed = 123
)



## ----Reduced congvergence, results='hide', cache=TRUE, include=FALSE----
# Convergence
plot(reduced_imp)

imp$method
summary(reduced_imp)

densityplot(reduced_imp, ~ HbA1c)

stripplot(reduced_imp, Employment ~ .imp)

stripplot(reduced_imp, Income ~ .imp)


## ----extract reduced, results='hide', cache=TRUE, include=FALSE------
reduced_imputed <- lapply(
  1:5,
  function(i) complete(reduced_imp, i)
)
length(reduced_imputed)


## ----reducaed lasso, results='hide', cache=TRUE, include=FALSE-------
reduced_lasso <- lapply(
  reduced_imputed,
  function(dat2){

    dat2$class <- as.factor(dat2$class)

    x <- model.matrix(class ~ ., data = dat2)[,-1]
    y <- dat2$class

    cv.glmnet(
      x,
      y,
      family = "multinomial",
      alpha = 1,
      nfolds = 10,
      type.measure = "class"
    )
  }
)
class(reduced_lasso[[1]])

coef(reduced_lasso[[1]], s = "lambda.1se")


## ----reduced accuracy, results='hide', cache=TRUE, include=FALSE-----
reduced_cv_accuracy <- sapply(
  reduced_lasso,
  function(model) {
    idx <- which(model$lambda == model$lambda.1se)
    1 - model$cvm[idx]
  }
)

reduced_cv_accuracy
mean(reduced_cv_accuracy)
sd(reduced_cv_accuracy)


## ----Confusion matrix, results='hide', cache=TRUE, include=FALSE-----
dat1 <- imputed_datasets[[1]]

x1 <- model.matrix(class ~ ., data = dat1)[,-1]

pred1 <- stats::predict(
  lasso_models[[1]],
  newx = x1,
  s = "lambda.1se",
  type = "class"
)

confusion_matrix <- table(
  Observed = dat1$class,
  Predicted = pred1
)
confusion_matrix


## ----strongest prdictors, results='hide', cache=TRUE, include=FALSE----
coef1 <- coef(lasso_models[[1]], s = "lambda.1se")

as.matrix(coef1[[1]])

tmp <- data.frame(
  Variable = rownames(as.matrix(coef1[[1]])),
  Coefficient = as.numeric(as.matrix(coef1[[1]]))
)

tmp <- tmp[tmp$Variable != "(Intercept)", ]

top10 <- tmp[order(abs(tmp$Coefficient), decreasing = TRUE), ][1:10, ]
print(top10)


## ----important of variable, results='hide', include=FALSE, warning=FALSE, message=FALSE----
importance_all <- lapply(
  1:5,
  function(i){

    coefs <- coef(
      lasso_models[[i]],
      s = "lambda.1se"
    )

    all_coef <- do.call(
      rbind,
      lapply(names(coefs), function(cl){

        tmp <- data.frame(
          Variable = rownames(as.matrix(coefs[[cl]])),
          Coefficient = as.numeric(as.matrix(coefs[[cl]]))
        )

        tmp
      })
    )

    all_coef <- subset(
      all_coef,
      Variable != "(Intercept)"
    )

    aggregate(
      abs(Coefficient) ~ Variable,
      data = all_coef,
      FUN = mean
    )
  }
)

importance_df <- Reduce(
  function(x, y)
    merge(x, y, by = "Variable", all = TRUE),
  importance_all
)

colnames(importance_df) <- c(
  "Variable",
  paste0("Imp", 1:5)
)

importance_df$MeanImportance <- rowMeans(
  importance_df[, -1],
  na.rm = TRUE
)

importance_df <- importance_df[
  order(-importance_df$MeanImportance),
]

print(head(importance_df, 15))


## ----prediction-performance-table, echo=FALSE, message=FALSE, warning=FALSE----
library(knitr)
library(kableExtra)

performance_table <- data.frame(
  Measure = c(
    "Missing predictor values",
    "Number of imputations",
    "Mean 10-fold CV accuracy",
    "SD of CV accuracy"
  ),
  Result = c(
    "7.0%",
    "5",
    "84.4%",
    "0.25%"
  )
)

kable(
  performance_table,
  caption = "Prediction Model Performance",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----lasso-important-predictors-table, echo=FALSE, message=FALSE, warning=FALSE----
top_predictors <- data.frame(
  Rank = 1:10,
  Predictor = c(
    "Triglycerides",
    "Ethnicity: South Asian",
    "Anxiety: moderate-severe",
    "Depression: minimal",
    "Anxiety: severe",
    "Ethnicity: Other",
    "Anxiety: moderate",
    "Quality of life: high",
    "Depression: moderate",
    "Marital status: not stated"
  ),
  `Mean Importance` = c(
    1.19, 1.12, 1.05, 0.83, 0.74,
    0.66, 0.58, 0.57, 0.48, 0.46
  )
)

kable(
  top_predictors,
  caption = "Top Predictors of Latent Subtype Membership from the Multinomial LASSO Model",
  booktabs = TRUE,
  align = "clc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----class-specific-accuracy-table, echo=FALSE, message=FALSE, warning=FALSE----
class_accuracy <- data.frame(
  `Latent Group` = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
  `Classification Accuracy (%)` = c(79.4, 90.8, 89.6, 88.1, 78.1)
)

kable(
  class_accuracy,
  caption = "Class-specific Classification Accuracy of the Multinomial LASSO Model",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----sensitivity-prediction-table, echo=FALSE, message=FALSE, warning=FALSE----
sensitivity_table <- data.frame(
  Model = c(
    "Full model: 18 predictors",
    "Reduced model: clinically available predictors"
  ),
  `Mean 10-fold CV Accuracy (%)` = c(84.4, 58.2),
  `SD (%)` = c(0.25, 0.29)
)

kable(
  sensitivity_table,
  caption = "Sensitivity Analysis of Prediction Model Performance",
  booktabs = TRUE,
  align = "lcc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----Appendix-continuous-profile, echo=FALSE, message=FALSE, warning=FALSE----
continuous_vars <- c(
  "age",
  "BMI",
  "HbA1c(%)",
  "Gestational Age",
  "Cholesterol",
  "Triglycerides",
  "High-Density Lipoprotein",
  "Wellbeing Score",
  "Physical Activity"
)

class_means <- analysis_data %>%
  group_by(class) %>%
  summarise(
    across(
      all_of(continuous_vars),
      ~ mean(.x, na.rm = TRUE)
    )
  )

kable(
  class_means %>%
    pivot_longer(
      -class,
      names_to = "Variable",
      values_to = "Mean"
    ) %>%
    pivot_wider(
      names_from = class,
      values_from = Mean,
      names_prefix = "Group "
    ) %>%
    mutate(
      across(where(is.numeric), ~ round(.x, 2))
    ),
  caption = "Actual mean values of continuous variables across the five identified latent classes.",
  booktabs = TRUE
) %>%
  kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  )


## ----Appendix-categorical-profile, echo=FALSE, message=FALSE, warning=FALSE----
cat_summary_table <- analysis_data %>%
  group_by(class) %>%
  summarise(
    `Family History (%)` =
      round(mean(`Family history` == "yes", na.rm = TRUE) * 100, 1),
    
    `Previous Births >=2 (%)` =
      round(mean(`Previous Births` == "2+", na.rm = TRUE) * 100, 1),
    
    `South Asian Ethnicity (%)` =
      round(mean(Ethnicity == "South Asian", na.rm = TRUE) * 100, 1),
    
    `High Quality of Life (%)` =
      round(mean(`QoL cat` == "High", na.rm = TRUE) * 100, 1),
    
    `Anxiety >= Moderate (%)` =
      round(mean(`Anxiety Cat` %in% c("moderate", "mod_severe", "severe"),
                 na.rm = TRUE) * 100, 1),
    
    `Depression >= Moderate (%)` =
      round(mean(`Depression Cat` %in% c("moderate", "mod_severe", "severe"),
                 na.rm = TRUE) * 100, 1),
    
    `Employed (%)` =
      round(mean(Employment == "Employed", na.rm = TRUE) * 100, 1),
    
    `High Income (%)` =
      round(mean(Income == "High", na.rm = TRUE) * 100, 1)
  ) %>%
  tidyr::pivot_longer(
    -class,
    names_to = "Variable",
    values_to = "Percentage"
  ) %>%
  tidyr::pivot_wider(
    names_from = class,
    values_from = Percentage,
    names_prefix = "Group "
  )

kable(
  cat_summary_table,
  caption = "Summary percentages of categorical variables across the five identified latent classes.",
  booktabs = TRUE
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down")
  )



## ----class-overlap-table, echo=FALSE, message=FALSE, warning=FALSE----
library(knitr)
library(kableExtra)

class_overlap <- data.frame(
  `Original Class` = c(
    "Class 1",
    "Class 2",
    "Class 3",
    "Class 4",
    "Class 5"
  ),
  `Best Matching Transformed Class` = c(
    "Class 2",
    "Class 3",
    "Class 4",
    "Class 2",
    "Class 1"
  ),
  `Overlap (%)` = c(
    51.5,
    75.6,
    91.4,
    50.3,
    33.6
  )
)

kable(
  class_overlap,
  caption = "Overlap between the original and transformed latent class solutions",
  booktabs = TRUE,
  align = c("c", "c", "c")
) %>%
  kable_styling(
    latex_options = c("hold_position")
  )


## ----Sensitivity-group-size, echo=FALSE, message=FALSE, warning=FALSE----
class_accuracy <- data.frame(
  `Latent Group` = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
   `n` = c(615, 744, 1174, 1167, 903),
   `Percentage` = round(c(615, 744, 1174, 1167, 903) / 4303 * 100, 1)
  
)

kable(
  class_accuracy,
  caption = "Sensitivity analysis final group size",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----missing-table, results='hide', include=FALSE--------------------
missing_table <- data.frame(
  Variable = names(prediction_data),
  Missing_n = sapply(prediction_data, function(x) sum(is.na(x))),
  Missing_percent = round(
    sapply(prediction_data, function(x) mean(is.na(x)) * 100),
    1
  )
)

missing_table


## ----missingnes, warning=FALSE, message=FALSE------------------------
kable(
  missing_table,
  caption = "Missingness among candidate predictors prior to multiple imputation",
  booktabs = TRUE
) %>%
  kable_styling(latex_options = "hold_position")


## ----appendix-convergence,echo=FALSE,fig.cap="Convergence diagnostics for the MICE procedure.",fig.width=8,fig.height=6,fig.show='hold',out.width='48%'----

plot(imp)



## ----appendix-densityplots,echo=FALSE,fig.cap="Density plots comparing observed and imputed values for continuous variables included in the prediction model. The distributions of imputed values closely followed those of the observed data, supporting the plausibility of the imputation procedure.",fig.width=10,fig.height=8,fig.show='hold',out.width='48%'----

densityplot(imp, ~ HbA1c, main = "HbA1c")
densityplot(imp, ~ Cholesterol, main = "Cholesterol")
densityplot(imp, ~ Triglycerides, main = "Triglycerides")
densityplot(imp, ~ HDL, main = "HDL")
densityplot(imp, ~ Physical_Activity, main = "Physical Activity")
densityplot(imp, ~ Wellbeing_Score, main = "Well-being Score")



## ----appendix-stripplots,echo=FALSE,fig.cap="Strip plots of imputed values for categorical variables across the five imputed datasets.",fig.width=8,fig.height=6,fig.show='hold',out.width='48%'----

stripplot(imp, Employment ~ .imp)
stripplot(imp, Income ~ .imp)
stripplot(imp, QoL_cat ~ .imp)



## ----appendix-confusion-matrix, echo=FALSE---------------------------
library(knitr)
library(kableExtra)

kable(
  confusion_matrix,
  caption = "Confusion matrix for the final multinomial LASSO prediction model",
  booktabs = TRUE
) %>%
  kable_styling(
    latex_options = c("hold_position")
  )


## ----Predictor-importance-grouped, echo=FALSE, message=FALSE,warning=FALSE----

importance_grouped <- importance_df %>%
  dplyr::mutate(
    Domain = dplyr::case_when(
      grepl("age|Ethnicity|Family_history|Previous_Births|Gestational_Age", Variable) ~ "Demographic / obstetric",
      grepl("BMI|HbA1c|Cholesterol|Triglycerides|HDL", Variable) ~ "Metabolic / biochemical",
      grepl("Wellbeing|QoL|Anxiety|Depression", Variable) ~ "Psychosocial",
      grepl("Income|Marital_Status|Employment", Variable) ~ "Socioeconomic",
      grepl("Physical_Activity", Variable) ~ "Lifestyle",
      TRUE ~ "Other"
    ),
    Variable = recode(
      Variable,
      "age" = "Age",
      "BMI" = "BMI",
      "HbA1c" = "HbA1c",
      "Gestational_Age" = "Gestational age",
      "Cholesterol" = "Cholesterol",
      "Triglycerides" = "Triglycerides",
      "HDL" = "HDL cholesterol",
      "Wellbeing_Score" = "Wellbeing score",
      "Physical_Activity" = "Physical activity",
      "Family_historyyes" = "Family history: yes",
      "Previous_Births2+" = "Previous births: 2+",
      "EthnicityOther" = "Ethnicity: Other",
      "EthnicitySouth Asian" = "Ethnicity: South Asian",
      "QoL_catModerate" = "Quality of life: moderate",
      "QoL_catHigh" = "Quality of life: high",
      "Anxiety_Catmoderate" = "Anxiety: moderate",
      "Anxiety_Catmod_severe" = "Anxiety: moderate/severe",
      "Anxiety_Catsevere" = "Anxiety: severe",
      "Depression_Catminimal" = "Depression: minimal",
      "Depression_Catmoderate" = "Depression: moderate",
      "Depression_Catmod_severe" = "Depression: moderate/severe",
      "Depression_Catsevere" = "Depression: severe",
      "Marital_Statusnotsaid" = "Marital status: not stated",
      "Marital_StatusSingle" = "Marital status: single",
      "Marital_StatusTogether" = "Marital status: together",
      "Marital_StatusWidowed" = "Marital status: widowed",
      "EmploymentOther" = "Employment: other",
      "EmploymentStudent" = "Employment: student",
      "EmploymentUnemployed" = "Employment: unemployed",
      "IncomeLow" = "Income: low",
      "IncomeModerate" = "Income: moderate"
    )
  ) %>%
  dplyr::arrange(
    factor(
      Domain,
      levels = c(
        "Demographic / obstetric",
        "Metabolic / biochemical",
        "Psychosocial",
        "Socioeconomic",
        "Lifestyle",
        "Other"
      )
    ),
    dplyr::desc(MeanImportance)
  ) %>%
  dplyr::select(Domain, Variable, Imp1, Imp2, Imp3, Imp4, Imp5, MeanImportance)

kable(
  importance_grouped %>%
    dplyr::select(Variable, Imp1, Imp2, Imp3, Imp4, Imp5, MeanImportance) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))),
  caption = "Average absolute multinomial LASSO coefficient estimates across the five imputed datasets grouped by predictor domain.",
  booktabs = TRUE,
  col.names = c("Variable", "Imp1", "Imp2", "Imp3", "Imp4", "Imp5", "Mean")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 8
  ) %>%
  group_rows("Demographic factors", 1, 6) %>%
  group_rows("Metabolic factors", 7, 11) %>%
  group_rows("Psychosocial factors", 12, 21) %>%
  group_rows("Socioeconomic factors", 22, 30) %>%
  group_rows("Lifestyle factors", 31, 31)

