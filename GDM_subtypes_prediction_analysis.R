## ----work-directory, include=FALSE--------------------------------------------------------------------------------
setwd("C:/Courses/Thesis/R_code/Second_analysis")


## ----setup, include=FALSE-----------------------------------------------------------------------------------------
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


## ----data_loading-------------------------------------------------------------------------------------------------
pride <- read.csv("pride_data.csv",
                  header = TRUE,
                  sep = ";",
                  dec = ",",
                  na.strings = c("NA", ""))



## ----Data preparation---------------------------------------------------------------------------------------------
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
    `Fasting glucose level` = v2g_SerumGlucose0Mins,
    `Post load glucose level` = v2g_SerumGlucose120Mins,
    
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
    `Physical Activity log` = log(`Physical Activity` + 1)
  )


analysis_data <- analysis_data %>%
  mutate(
    Triglycerides = log(TRL + 1)
  )

 analysis_data <- analysis_data %>%
   filter(!is.na(GDM_type) & GDM_type != "Non-GDM")



## ----Demo_clinical_Obs_GDM, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE----------------------------------
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


## ----Biochemical_Metabolic_GDM, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE------------------------------
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
        `HbA1c(%)`,
        Cholesterol,
        `High-Density Lipoprotein`,
        Triglycerides
        
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


## ----Psychosocial_Socioeconomic, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE-----------------------------


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
        `Physical Activity log`,
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


## ----preparing analysis, echo=FALSE, results='hide'---------------------------------------------------------------
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
     `Physical Activity log`,
     `Marital Status`,
      Employment,
      Income,
      `Fasting glucose level`,
     `Post load glucose level`
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
   "Physical Activity log",
   "Fasting glucose level",
   "Post load glucose level"
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


## ----final model, echo=FALSE, results='hide',cache=TRUE-----------------------------------------------------------
## Fit generalized latent class model for 5 classes
set.seed(123)

Model_with1 <- VarSelCluster(x = mixed_data, gvals = 4, vbleSelec = TRUE)

summary(Model_with1)

# Criteria
Model_with1@criteria@BIC
Model_with1@criteria@AIC
Model_with1@criteria@loglikelihood

# Model
Model_with1@model@names.relevant

# Parameters 
Model_with1@param@paramContinuous@mu
Model_with1@param@paramCategorical@alpha



# Partitions
table(Model_with1@partitions@zMAP)

# Create the subtype variable 
analysis_data$latent_class <- Model_with1@partitions@zMAP




## ----Mode-selection-table, echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------
class_accuracy <- data.frame(
  `Latent Group` = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
  `BIC value` = c(-13266.39 , -13076.45 , -13015.39 , -12987.33 , -13006.54 )
)

kable(
  class_accuracy,
  caption = "BIC values of Model selection",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  )


## ----Final-group-size, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------
n <- c(198, 145, 94, 169)

pct <- round(n / sum(n) * 100, 1)

# Adjust final category so displayed percentages sum to exactly 100
pct[length(pct)] <- 100 - sum(pct[-length(pct)])

class_accuracy <- data.frame(
  `Latent Group` = paste("Group", 1:4),
  n = n,
  Percentage = pct
)


kable(
  class_accuracy,
  caption = "Final group size",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----relevant-mean, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------
relevant_means <- analysis_data %>%
  filter(!is.na(latent_class)) %>%
  group_by(latent_class) %>%
  summarise(
    n = n(),
    HbA1c = mean(`HbA1c(%)`, na.rm = TRUE),
    Gestational_Age = mean(`Gestational Age`, na.rm = TRUE),
    Fasting_Glucose = mean(`Fasting glucose level`, na.rm = TRUE),
    Postload_Glucose = mean(`Post load glucose level`, na.rm = TRUE)
  )




## ----relevant-class-means-table, echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------
library(dplyr)
library(knitr)
library(kableExtra)

relevant_means %>%
  dplyr::mutate(
    latent_class = paste("Group", latent_class),
    HbA1c = round(HbA1c, 2),
    Gestational_Age = round(Gestational_Age, 1),
    Fasting_Glucose = round(Fasting_Glucose, 2),
    Postload_Glucose = round(Postload_Glucose, 2)
  ) %>%
  knitr::kable(
    caption = "Mean values of variables relevant for defining the four latent classes.",
    booktabs = TRUE,
    col.names = c(
      "Latent Group",
      "n",
      "HbA1c",
      "Gestational Age",
      "Fasting Glucose",
      "Post-load Glucose"
    ),
    align = c("l", "c", "c", "c", "c", "c")
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  )


## ----Heatmap, fig.width=7, fig.height=5, fig.cap="Heatmap showing standardized mean values (z-scores) of continuous variables across the five latent classes. Red cells indicate values above the overall sample mean, whereas blue cells indicate values below the overall sample mean.", cache=TRUE----
mu <- as.data.frame(Model_with1@param@paramContinuous@mu)

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
    "HbA1c(%)",
  "Gestational Age",
  "Fasting glucose level",
  "Post load glucose level"
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


## ----conditional independence, echo=FALSE, include=FALSE----------------------------------------------------------
## continues variables
selected_vars <- c(
  "HbA1c(%)",
  "Gestational Age",
  "Fasting glucose level",
  "Post load glucose level"
)

for (g in 1:4) {
  
  cat("\n====================\n")
  cat("Latent Group", g, "\n")
  cat("====================\n")
  
  cor_matrix <- analysis_data %>%
    dplyr::filter(latent_class == g) %>%
    dplyr::select(dplyr::all_of(selected_vars)) %>%
    cor(use = "pairwise.complete.obs")
  
  print(round(cor_matrix, 3))
}


## ----GDM-type-distribution, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------

library(dplyr)
library(knitr)
library(kableExtra)
library(lsr)

# Cross-tabulation
tab <- table(
  analysis_data$latent_class,
  analysis_data$GDM_type
)

# Row percentages
pct <- prop.table(tab, margin = 1) * 100

# Combine n and %
display_tab <- matrix(
  paste0(
    tab,
    " (",
    sprintf("%.1f", pct),
    ")"
  ),
  nrow = nrow(tab),
  dimnames = dimnames(tab)
)

# Convert to data frame
gdm_table <- as.data.frame.matrix(display_tab)

gdm_table <- gdm_table %>%
  tibble::rownames_to_column("Latent Group") %>%
  dplyr::mutate(
    `Latent Group` = paste("Group", `Latent Group`)
  )

# Statistical tests
chi_result <- chisq.test(tab)
cramer_v <- lsr::cramersV(tab)
# Convert to data frame
gdm_table %>%
  knitr::kable(
    caption = "Distribution of GDM Types Across the Four Identified Latent Groups",
    booktabs = TRUE,
    align = "lccc",
    col.names = c(
      "Latent Group",
      "Type 1 GDM",
      "Type 2 GDM",
      "Type 3 GDM"
    )
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE,
    font_size = 9
  ) %>%
  kableExtra::column_spec(1, width = "3.2cm") %>%
  kableExtra::column_spec(2, width = "2.4cm") %>%
  kableExtra::column_spec(3, width = "2.4cm") %>%
  kableExtra::column_spec(4, width = "2.4cm") %>%
  kableExtra::footnote(
    general = paste0(
      "Values are n (%), with percentages calculated within each latent group.",
      "Pearson's chi-square test: p ",
      ifelse(
        chi_result$p.value < 0.001,
        "< 0.001",
        paste0("= ", round(chi_result$p.value, 3))
      ),
      "; Cramer's V = ",
      round(cramer_v, 3),
      "."
    ),
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----BirthOutCome-LatentGroup, echo=FALSE, warning=FALSE, message=FALSE-------------------------------------------
analysis_data <- analysis_data %>%
  dplyr::mutate(
    `GDM treated` = factor(
      GDM_treated_1,
      levels = c(0, 1),
      labels = c("Untreated", "Treated")
    )
  )

stopifnot("GDM treated" %in% names(analysis_data))
library(dplyr)
library(gtsummary)
library(kableExtra)

# ---------------------------------------------------------
# Function to create the birth-outcome table
# ---------------------------------------------------------

Birth_table <- function(data) {

  data %>%
    dplyr::filter(!is.na(latent_class)) %>%
    dplyr::select(
      latent_class,
      LGA_intergrowth,
      SGA_intergrowth,
      preterm,
      Csection
    ) %>%
    dplyr::rename(
       LGA = LGA_intergrowth,
      SGA = SGA_intergrowth
    ) %>%
    dplyr::mutate(
      latent_class = factor(
        latent_class,
        levels = 1:4,
        labels = paste("Group", 1:4)
      )
    ) %>%
    tbl_summary(
      by = latent_class,
      statistic = all_categorical() ~ "{n} ({p})",
      missing = "no"
    ) %>%
    add_p(
      test = list(
        LGA ~ "chisq.test",
        SGA ~ "fisher.test",
        preterm ~ "chisq.test",
        Csection ~ "chisq.test"
      )
    ) %>%
    modify_header(
      label = "**Birth Outcome**"
    ) %>%
    bold_labels() %>%
    modify_table_body(
      ~ .x %>%
        dplyr::filter(
          row_type != "level" | label == "1"
        )
    ) %>%
    modify_footnote(
      update = everything() ~ NA
    )
}


# ---------------------------------------------------------
# 1. ALL WOMEN
# ---------------------------------------------------------

table_all <- Birth_table(
  analysis_data
)


# ---------------------------------------------------------
# 2. TREATED
# Replace `GDM treated` with your actual variable
# ---------------------------------------------------------

table_treated <- analysis_data %>%
  dplyr::filter(`GDM treated` == "Treated") %>%
  Birth_table()


# ---------------------------------------------------------
# 3. UNTREATED
# ---------------------------------------------------------

table_untreated <- analysis_data %>%
  dplyr::filter(`GDM treated` == "Untreated") %>%
  Birth_table()


# ---------------------------------------------------------
# Combine the three tables
# ---------------------------------------------------------

# ---------------------------------------------------------
# Combine the three tables
# ---------------------------------------------------------

invisible(
  capture.output(
    Birth_outcomes_combined <- suppressMessages(
      tbl_stack(
        list(
          table_all,
          table_treated,
          table_untreated
        ),
        group_header = c(
          "All women",
          "Treated",
          "Untreated"
        )
      )
    )
  )
)

Birth_outcomes_combined <- Birth_outcomes_combined %>%
  modify_caption(
    "Birth Outcomes Across the Four Latent Classes by Treatment Status"
  )


# ---------------------------------------------------------
# Produce final PDF table
# ---------------------------------------------------------

Birth_outcomes_combined %>%
  as_kable_extra(
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE,
    position = "center",
    font_size = 8
  ) %>%
  column_spec(1, width = "2.6cm") %>%
  column_spec(2:5, width = "1.55cm") %>%
  column_spec(6, width = "1.2cm") %>%
  kableExtra::footnote(
    general = paste(
      "Values are n (%), with percentages calculated within each latent class.",
      "P-values were obtained using Pearson's chi-square test, except for SGA, for which Fisher's exact test was used."
    ),
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----Sensitivity_Data_preparation---------------------------------------------------------------------------------
## Create types of GDM
analysis_data3 <- pride %>%
  mutate(
    GDM_type = case_when(
      v2g_SerumGlucose0Mins >= 5.1 & v2g_SerumGlucose120Mins >= 8.5 ~ "type-3GDM",
      v2g_SerumGlucose0Mins >= 5.1 & (is.na(v2g_SerumGlucose120Mins ) | v2g_SerumGlucose120Mins < 8.5) ~ "type-1GDM",
      v2g_SerumGlucose120Mins >= 8.5 & (is.na(v2g_SerumGlucose0Mins ) | v2g_SerumGlucose0Mins < 5.1) ~ "type-2GDM",
      (v2g_SerumGlucose0Mins < 5.1 & v2g_SerumGlucose120Mins < 8.5) | (v2g_SerumGlucose0Mins < 5.1 & (is.na(v2g_SerumGlucose120Mins))) | (v2g_SerumGlucose120Mins < 8.5 & (is.na(v2g_SerumGlucose0Mins))) ~ "Non-GDM",
      
     
    )
  )

# ## Create a dataset containe important varibles only
# analysis_data3 <- analysis_data3 %>%
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
analysis_data3 <- analysis_data3 %>%
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
    `Fasting glucose level` = v2g_SerumGlucose0Mins,
    `Post load glucose level` = v2g_SerumGlucose120Mins,
    
    
  )
analysis_data3 <- analysis_data3 %>%
  mutate(`Family history` = recode(`Family history`, `1` = "yes", `2` = "no")
         )


analysis_data3 <- analysis_data3 %>%
  mutate(
    `Previous Births` = factor(
      if_else(`Previous Births` >= 2, "2+", as.character(`Previous Births`)),
      levels = c("1", "2+")
    )
  )



analysis_data3 <- analysis_data3 %>%
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

analysis_data3 <- analysis_data3 %>%
  mutate(
    `Physical Activity1` = log(`Physical Activity` + 1)
  )


analysis_data3 <- analysis_data3 %>%
  mutate(
    Triglycerides = log(TRL + 1)
  )

analysis_data3 <- analysis_data3 %>%
  filter(!is.na(GDM_type) & GDM_type != "Non-GDM")






## ----sensitivity-preparing-analysis, results='hide'---------------------------------------------------------------
## Preparing data for analysis
library(VarSelLCM)

 # Select variable
 mixed_data3 <- analysis_data3 %>%
   dplyr::select(
      age,
      BMI,
     `HbA1c(%)`,
     `Family history`,
      Ethnicity,
      Triglycerides,
      Cholesterol,
     `High-Density Lipoprotein`,
      `Fasting glucose level`,
      `Post load glucose level`,
     
   )

 # Continuous variables
 continuous_vars3 <- c(
   "age",
   "BMI",
   "HbA1c(%)",
   "Cholesterol",
   "Triglycerides",
   "High-Density Lipoprotein",
   "Fasting glucose level",
   "Post load glucose level"
 )

 # Standardize continuous variables
 mixed_data3[continuous_vars3] <- scale(mixed_data3[continuous_vars3])

 # Convert categorical variables to factor
 mixed_data3 <- mixed_data3 %>%
   mutate(
     across(
       -all_of(continuous_vars3),
       as.factor
     )
   )

 # Check for highly correlation
 library(corrplot)
 cor_mat <- cor(mixed_data3[continuous_vars3], use = "complete.obs")

 upper_tri <- cor_mat
 upper_tri[lower.tri(upper_tri, diag = TRUE)] <- NA

 high_corr_pairs <- which(abs(upper_tri) > 0.7, arr.ind = TRUE)

 data.frame(
   var1 = rownames(upper_tri)[high_corr_pairs[,1]],
   var2 = colnames(upper_tri)[high_corr_pairs[,2]],
   corr = upper_tri[high_corr_pairs]
 )

cor_mat




## ----sensitivity_final-model, results='hide',cache=TRUE-----------------------------------------------------------
## Fit generalized latent class model for 5 classes
set.seed(123)

Model3 <- VarSelCluster(x = mixed_data3, gvals = 5, vbleSelec = FALSE)

summary(Model3)

# Criteria
Model3@criteria@BIC
Model3@criteria@AIC
Model3@criteria@loglikelihood

# Model
Model3@model@names.relevant

# Parameters 
Model3@param@paramContinuous@mu
Model3@param@paramCategorical@alpha



# Partitions
table(Model3@partitions@zMAP)

# Create the subtype variable 
analysis_data3$latent_class_sensitivity <- Model3@partitions@zMAP






## ----comparison, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------
comparison_counts <- table(
  primary_class = analysis_data$latent_class,
  sensitivity_class = analysis_data3$latent_class_sensitivity
)

comparison_percent <- prop.table(
  comparison_counts,
  margin = 1
) * 100

comparison_table <- matrix(
  paste0(
    comparison_counts,
    " (",
    round(comparison_percent, 1),
    ")"
  ),
  nrow = nrow(comparison_counts),
  dimnames = dimnames(comparison_counts)
)

comparison_table <- as.data.frame.matrix(
  comparison_table
)

comparison_table <- cbind(
  `Primary Latent Group` = paste0(
    "Group ",
    rownames(comparison_table)
  ),
  comparison_table
)

rownames(comparison_table) <- NULL


# Row percentages
comparison_percent <- prop.table(
  comparison_counts,
  margin = 1
) * 100

# Combine n and row %
comparison_table <- matrix(
  paste0(
    comparison_counts,
    " (",
    round(comparison_percent, 1),
    "%)"
  ),
  nrow = nrow(comparison_counts),
  dimnames = dimnames(comparison_counts)
)

# Convert to data frame
comparison_table <- as.data.frame.matrix(
  comparison_table
)

# Add primary group names
comparison_table <- cbind(
  `Primary Latent Group` = paste0(
    "Group ",
    rownames(comparison_table)
  ),
  comparison_table
)

rownames(comparison_table) <- NULL


## ----Imputation, esults='hide', cache=TRUE, include=FALSE, echo=FALSE---------------------------------------------
prediction_data <- readRDS("prediction_data.rds")
names(prediction_data)

# Predictors to keep
all_predictors <- c(
  "age",
  "BMI",
  "HbA1c(%)",
  "Gestational Age",
  "Cholesterol",
  "Triglycerides",
  "High-Density Lipoprotein",
  "Family history",
  "Ethnicity",
  "Anxiety Cat",
  "Depression Cat"
)

categorical_vars <- c(
  "Family history",
  "Ethnicity",
  "Anxiety Cat",
  "Depression Cat"
)

# Convert categorical variables to factors
prediction_data <- prediction_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(categorical_vars),
      ~ as.factor(.x)
    )
  )

# Keep predictors + outcome and rename
prediction_data <- prediction_data %>%
  dplyr::select(
    dplyr::all_of(all_predictors),
    GDM_type
  ) %>%
  dplyr::rename(
    HbA1c = `HbA1c(%)`,
    Gestational_Age = `Gestational Age`,
    HDL = `High-Density Lipoprotein`,
    Family_history = `Family history`,
    Anxiety_Cat = `Anxiety Cat`,
    Depression_Cat = `Depression Cat`
  ) %>%
  dplyr::mutate(
    GDM_type = as.factor(GDM_type)
  )

# Multiple imputation
imp <- mice::mice(
  prediction_data,
  m = 20,
  maxit = 20,
  seed = 123
)


## ----Prediction, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------

# Class frequencies
class_counts <- table(prediction_data$GDM_type)

# Inverse-frequency class weights
class_weights <- nrow(prediction_data) /
  (length(class_counts) * class_counts)

# Observation-specific weights
obs_weights <- as.numeric(
  class_weights[as.character(prediction_data$GDM_type)]
)


## ----Extract imputed data, results='hide', cache=TRUE, include=FALSE----------------------------------------------
imputed_datasets <- lapply(
  1:20,
  function(i) complete(imp, i)
)
length(imputed_datasets)


## ----Weighted-Multinomia, echo=FALSE, warning=FALSE, message=FALSE, cache=FALSE-----------------------------------
prediction_data$GDM_type <- relevel(
  factor(prediction_data$GDM_type),
  ref = "Non-GDM"
)
library(nnet)

final_model <- nnet::multinom(
  GDM_type ~ age + BMI + HbA1c + Triglycerides,
  data = prediction_data,
  weights = obs_weights,
  trace = FALSE
)



## ----Weighted-Imputed, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------
final_models <- lapply(
  imputed_datasets,
  function(Predction_data) {
    
    Predction_data$GDM_type <- factor(
      Predction_data$GDM_type,
      levels = c(
        "Non-GDM",
        "type-1GDM",
        "type-2GDM",
        "type-3GDM"
      )
    )
    
    class_counts <- table(Predction_data$GDM_type)
    
    class_weights <- nrow(Predction_data) /
      (length(class_counts) * class_counts)
    
    obs_weights <- as.numeric(
      class_weights[as.character(Predction_data$GDM_type)]
    )
    
    nnet::multinom(
      GDM_type ~ age + BMI + HbA1c + Triglycerides,
      data = Predction_data,
      weights = obs_weights,
      trace = FALSE
    )
  }
)




## ----final-coefficients, echo=FALSE, warning=FALSE, message=FALSE, cache=TRUE-------------------------------------
# Extract coefficients
coef_list <- lapply(
  final_models,
  coef
)

# Combine into a 3 x 5 x 20 array
coef_array <- simplify2array(coef_list)

# Mean coefficients across the 20 imputations
mean_coef <- apply(
  coef_array,
  c(1, 2),
  mean
)

round(mean_coef, 4)


## ----BirthOutCome-GDM-Type, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------

library(dplyr)
library(gtsummary)
library(kableExtra)

# ---------------------------------------------------------
# Function to create birth-outcome table by GDM type
# ---------------------------------------------------------

Birth_table2 <- function(data) {

  data %>%
    dplyr::filter(!is.na(GDM_type)) %>%
    dplyr::select(
      GDM_type,
      LGA_intergrowth,
      SGA_intergrowth,
      preterm,
      Csection
    ) %>%
    dplyr::rename(
      LGA = LGA_intergrowth,
      SGA = SGA_intergrowth
    ) %>%
    dplyr::mutate(
      GDM_type = as.factor(GDM_type)
    ) %>%
    tbl_summary(
      by = GDM_type,
      statistic = all_categorical() ~ "{n} ({p})",
      missing = "no"
    ) %>%
    add_p(
      test = list(
        LGA ~ "chisq.test",
        SGA ~ "fisher.test",
        preterm ~ "chisq.test",
        Csection ~ "chisq.test"
      )
    ) %>%
    modify_header(
      label = "**Birth Outcome**"
    ) %>%
    bold_labels() %>%
    modify_table_body(
      ~ .x %>%
        dplyr::filter(
          row_type != "level" | label == "1"
        )
    ) %>%
    modify_footnote(
      update = everything() ~ NA
    )
}


# ---------------------------------------------------------
# 1. ALL WOMEN
# ---------------------------------------------------------

table_all2 <- Birth_table2(
  analysis_data
)


# ---------------------------------------------------------
# 2. TREATED
# ---------------------------------------------------------

table_treated2 <- analysis_data %>%
  dplyr::filter(`GDM treated` == "Treated") %>%
  Birth_table2()


# ---------------------------------------------------------
# 3. UNTREATED
# ---------------------------------------------------------

table_untreated2 <- analysis_data %>%
  dplyr::filter(`GDM treated` == "Untreated") %>%
  Birth_table2()


# ---------------------------------------------------------
# Combine the three tables
# ---------------------------------------------------------

# ---------------------------------------------------------
# Combine the three GDM-type tables
# ---------------------------------------------------------

invisible(
  capture.output(
    Birth_outcomes_combined2 <- suppressMessages(
      tbl_stack(
        list(
          table_all2,
          table_treated2,
          table_untreated2
        ),
        group_header = c(
          "All women",
          "Treated",
          "Untreated"
        )
      )
    )
  )
)

Birth_outcomes_combined2 <- Birth_outcomes_combined2 %>%
  modify_caption(
    "Birth Outcomes Across GDM Types by Treatment Status"
  )


# ---------------------------------------------------------
# Produce final PDF table
# ---------------------------------------------------------

Birth_outcomes_combined2 %>%
  as_kable_extra(
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE,
    position = "center",
    font_size = 8
  ) %>%
  column_spec(1, width = "2.6cm") %>%
  column_spec(2:5, width = "1.55cm") %>%
  column_spec(6, width = "1.2cm") %>%
  kableExtra::footnote(
    general = paste(
      "Values are n (%), with percentages calculated within each GDM type.",
      "P-values were obtained using Pearson's chi-square test, except for SGA, for which Fisher's exact test was used."
    ),
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----Sensitivity-group, echo=FALSE, include=FALSE, message=FALSE, warning=FALSE-----------------------------------
n <- c(179, 69, 69, 166, 123)

pct <- round(n / sum(n) * 100, 1)

# Adjust final category so displayed percentages sum to exactly 100
pct[length(pct)] <- 100 - sum(pct[-length(pct)])

class_accuracy <- data.frame(
  `Latent Group` = paste("Group", 1:5),
  n = n,
  Percentage = pct
)

class_accuracy
kable(
  class_accuracy,
  caption = "Sensitivity group size",
  booktabs = TRUE,
  align = "lc"
) %>%
  kable_styling(latex_options = "hold_position")


## ----sensitivity-preSelected-variables, echo=FALSE, warning=FALSE, message=FALSE----------------------------------

library(dplyr)
library(knitr)
library(kableExtra)

# Add sensitivity class membership
analysis_data$latent_class_sensitivity <- Model3@partitions@zMAP

# Cross-tabulation
comparison_tab <- table(
  Primary = analysis_data$latent_class,
  Sensitivity = analysis_data$latent_class_sensitivity
)

# Row percentages
comparison_pct <- prop.table(comparison_tab, margin = 1) * 100

# Combine count and percentage
display_tab <- matrix(
  paste0(
    comparison_tab,
    " (",
    sprintf("%.1f", comparison_pct),
    ")"
  ),
  nrow = nrow(comparison_tab),
  dimnames = dimnames(comparison_tab)
)

# Convert to data frame
comparison_table %>%
  knitr::kable(
    caption = "Comparison of Latent Class Membership Between the Primary and Sensitivity Analyses",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c", "c"),
    col.names = c(
      "Primary Latent Group",
      "S1",
      "S2",
      "S3",
      "S4",
      "S5"
    )
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  ) %>%
  kableExtra::footnote(
    general = paste(
      "Values are n (row %).",
      "S1-S5 represent Latent Groups 1-5 from the sensitivity analysis."
    ),
    general_title = "Note: ",
    threeparttable = TRUE
    
  )


## ----sensitivity-profile-table, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Sensitivity class membership
analysis_data$latent_class_sensitivity <- Model3@partitions@zMAP


# -----------------------------
# Continuous variables: means
# -----------------------------

continuous_table <- analysis_data %>%
  dplyr::filter(!is.na(latent_class_sensitivity)) %>%
  dplyr::group_by(latent_class_sensitivity) %>%
  dplyr::summarise(
    `Age (year)` = mean(age, na.rm = TRUE),
    `BMI ($kg/m^2$)` = mean(BMI, na.rm = TRUE),
    HbA1c = mean(`HbA1c(%)`, na.rm = TRUE),
    Triglycerides = mean(Triglycerides, na.rm = TRUE),
    Cholesterol = mean(Cholesterol, na.rm = TRUE),
    `High-Density Lipoprotein ($mmol/L$)` = mean(`High-Density Lipoprotein`, na.rm = TRUE),
    `Fasting glucose` = mean(`Fasting glucose level`, na.rm = TRUE),
    `Post-load glucose` = mean(`Post load glucose level`, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = -latent_class_sensitivity,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    latent_class_sensitivity = paste("Group", latent_class_sensitivity),
    Value = round(Value, 2)
  ) %>%
  tidyr::pivot_wider(
    names_from = latent_class_sensitivity,
    values_from = Value
  )


# --------------------------------
# Family history: percentage Yes
# --------------------------------

family_table <- analysis_data %>%
  dplyr::filter(
    !is.na(latent_class_sensitivity),
    !is.na(`Family history`)
  ) %>%
  dplyr::group_by(latent_class_sensitivity) %>%
  dplyr::summarise(
    Value = mean(`Family history` == "yes") * 100,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Variable = "Family history: Yes (%)",
    latent_class_sensitivity = paste("Group", latent_class_sensitivity),
    Value = round(Value, 1)
  ) %>%
  tidyr::pivot_wider(
    names_from = latent_class_sensitivity,
    values_from = Value
  ) %>%
  dplyr::select(Variable, dplyr::everything())


# -----------------------------
# Ethnicity: percentages
# -----------------------------

ethnicity_table <- analysis_data %>%
  dplyr::filter(
    !is.na(latent_class_sensitivity),
    !is.na(Ethnicity)
  ) %>%
  dplyr::count(
    latent_class_sensitivity,
    Ethnicity,
    name = "n"
  ) %>%
  dplyr::group_by(latent_class_sensitivity) %>%
  dplyr::mutate(
    Value = 100 * n / sum(n)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Variable = paste0("Ethnicity: ", Ethnicity, " (%)"),
    latent_class_sensitivity = paste("Group", latent_class_sensitivity),
    Value = round(Value, 1)
  ) %>%
  dplyr::select(
    Variable,
    latent_class_sensitivity,
    Value
  ) %>%
  tidyr::pivot_wider(
    names_from = latent_class_sensitivity,
    values_from = Value
  )


# -----------------------------
# Combine all rows
# -----------------------------

sensitivity_profile_table <- dplyr::bind_rows(
  continuous_table,
  family_table,
  ethnicity_table
)


# -----------------------------
# Produce table
# -----------------------------

sensitivity_profile_table %>%
  knitr::kable(
    caption = "Characteristics of the Five Latent Groups in the Sensitivity Analysis",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c", "c"),
    escape = FALSE
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE,
    font_size = 8
  ) %>%
  kableExtra::column_spec(1, width = "4.0cm") %>%
  kableExtra::column_spec(2:6, width = "1.5cm") %>%
  kableExtra::footnote(
    general = "Continuous variables are presented as class-specific means; categorical",
    "\\\\",
    "variables are presented as percentages within each latent group.",
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----Sensitivity-GDM-type-distribution, echo=FALSE, warning=FALSE, message=FALSE----------------------------------

library(dplyr)
library(knitr)
library(kableExtra)
library(lsr)

# Cross-tabulation
tab <- table(
  analysis_data$latent_class_sensitivity,
  analysis_data$GDM_type
)

# Row percentages
pct <- prop.table(tab, margin = 1) * 100

# Combine n and %
display_tab <- matrix(
  paste0(
    tab,
    " (",
    sprintf("%.1f", pct),
    ")"
  ),
  nrow = nrow(tab),
  dimnames = dimnames(tab)
)

# Convert to data frame
gdm_table <- as.data.frame.matrix(display_tab)

gdm_table <- gdm_table %>%
  tibble::rownames_to_column("Latent Group") %>%
  mutate(
    `Latent Group` = paste("Group", `Latent Group`)
  )

# Statistical tests
chi_result <- chisq.test(tab)
cramer_v <- lsr::cramersV(tab)

# Create table
gdm_table %>%
  knitr::kable(
    caption = "Distribution of GDM Types Across the Five Identified Latent Groups of sensitivity analysis",
    booktabs = TRUE,
    align = c("l", "c", "c", "c"),
    col.names = c(
      "Latent Group",
      "Type 1 GDM",
      "Type 2 GDM",
      "Type 3 GDM"
    )
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  ) %>%
  kableExtra::footnote(
    general = paste0(
      "Values are n (%), with percentages calculated within each latent group. ",
      "\\\\",
      "Pearson's chi-square test: p ",
      ifelse(
        chi_result$p.value < 0.001,
        "< 0.001",
        paste0("= ", round(chi_result$p.value, 3))
      ),
      "; Cramer's V = ",
      round(cramer_v, 3),
      "."
    ),
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----Sensitivity-BirthOutcome, echo=FALSE, warning=FALSE, message=FALSE-------------------------------------------
Birth_outcomes <- analysis_data %>%
  dplyr::filter(!is.na(.data$latent_class_sensitivity)) %>%
  dplyr::select(
    latent_class_sensitivity,
    LGA_intergrowth,
    SGA_intergrowth,
    preterm,
    Csection
     )%>%
    dplyr::rename(
      LGA = LGA_intergrowth,
      SGA = SGA_intergrowth
  ) %>%
  dplyr::mutate(
    latent_class_sensitivity = factor(
      latent_class_sensitivity,
      levels = 1:5,
      labels = paste("Group", 1:5)
    )
  ) %>%
  tbl_summary(
    by = latent_class_sensitivity,
    statistic = all_categorical() ~ "{n} ({p})",
    missing = "no"
  ) %>%
  add_p(
    test = list(
      LGA ~ "chisq.test",
      SGA ~ "fisher.test",
      preterm ~ "chisq.test",
      Csection ~ "chisq.test"
    )
  ) %>%
  modify_header(
    label = "**Birth Outcome**"
  ) %>%
  modify_caption(
    "Birth Outcomes Across the Five Identified Latent Classes"
  ) %>%
  bold_labels() %>%
  modify_table_body(
    ~ .x %>%
      dplyr::filter(row_type != "level" | label == "1")
  ) %>%
  modify_footnote(
    update = everything() ~ NA
  )

Birth_outcomes %>%
  as_kable_extra(
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  ) %>%
  kableExtra::column_spec(1, width = "3cm") %>%
  kableExtra::column_spec(2:6, width = "1.6cm") %>%
  kableExtra::column_spec(7, width = "1.4cm") %>%
  kableExtra::footnote(
    general = paste(
      "Values are n (%), with percentages calculated within each latent class.",
      "P-values were obtained using Pearson's chi-square test, except for SGA_intergrowth, for which Fisher's exact test was used."
    ),
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----Imputation-density, echo=FALSE, message=FALSE, warning=FALSE,fig.width=10, fig.height=7, fig.width=8, fig.cap="Density plots comparing observed and imputed distributions for the continuous variables."----

library(gridExtra)

p1 <- densityplot(imp, ~ HbA1c)
p2 <- densityplot(imp, ~ Cholesterol)
p3 <- densityplot(imp, ~ Triglycerides)
p4 <- densityplot(imp, ~ HDL)

gridExtra::grid.arrange(
  p1, p2,
  p3, p4,
  ncol = 2
)


## ----predictor-selection-table, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------

library(dplyr)
library(knitr)
library(kableExtra)

selection_table <- data.frame(
  Predictor = c(
    "HbA1c",
    "BMI",
    "Triglycerides",
    "Age",
    "HDL cholesterol",
    "Family history",
    "Depression: moderate",
    "Anxiety: moderate",
    "Anxiety: severe",
    "Depression: severe",
    "Ethnicity: South Asian",
    "Cholesterol",
    "Gestational age"
  ),
  `Selected n/20` = c(
    20, 19, 19, 18, 12, 10, 6, 4, 3, 3, 2, 1, 1
  ),
  `Selection frequency (%)` = c(
    100, 95, 95, 90, 60, 50, 30, 20, 15, 15, 10, 5, 5
  )
)

selection_table %>%
  knitr::kable(
    caption = "Predictor Selection Stability in the Class-Weighted Multinomial LASSO Model",
    booktabs = TRUE,
    align = c("l", "c", "c")
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  ) %>%
  kableExtra::column_spec(1, width = "5cm") %>%
  kableExtra::column_spec(2:3, width = "3cm") %>%
  kableExtra::footnote(
    general = "Selection frequency represents the number of multiply imputed datasets in which the predictor had at least one non-zero multinomial LASSO coefficient at lambda.1se.",
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----weighted-vs-unweighted-table, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------

performance_compare <- data.frame(
  Measure = c(
    "Overall accuracy (%)",
    "Macro-F1",
    "Recall: Non-GDM (%)",
    "Recall: Type 1 GDM (%)",
    "Recall: Type 2 GDM (%)",
    "Recall: Type 3 GDM (%)"
  ),
  `Unweighted LASSO` = c(
    85.9,
    0.231,
    100.0,
    0.0,
    0.0,
    0.0
  ),
  `Class-weighted LASSO` = c(
    57.2,
    0.270,
    63.0,
    13.7,
    22.1,
    54.5
  )
)

performance_compare %>%
  knitr::kable(
    caption = "Comparison of Predictive Performance Between Unweighted and Class-Weighted Multinomial LASSO Models",
    booktabs = TRUE,
    align = c("l", "c", "c")
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  ) %>%
  kableExtra::footnote(
    general = "Performance was evaluated using out-of-fold predictions from 10-fold cross-validation across 20 multiply imputed datasets.",
    general_title = "Note: ",
    threeparttable = TRUE
  )


## ----final-model-performance-table, echo=FALSE, message=FALSE, warning=FALSE--------------------------------------

final_performance <- data.frame(
  Measure = c(
    "Mean 10-fold CV accuracy (%)",
    "SD of accuracy (%)",
    "Mean Macro-F1",
    "SD of Macro-F1",
    "Recall: Non-GDM (%)",
    "Recall: Type 1 GDM (%)",
    "Recall: Type 2 GDM (%)",
    "Recall: Type 3 GDM (%)",
    "F1: Non-GDM",
    "F1: Type 1 GDM",
    "F1: Type 2 GDM",
    "F1: Type 3 GDM"
  ),
  Result = c(
    51.2,
    0.4,
    0.268,
    0.004,
    55.2,
    20.1,
    30.8,
    49.5,
    0.692,
    0.137,
    0.080,
    0.163
  )
)

final_performance %>%
  knitr::kable(
    caption = "Cross-Validated Performance of the Final Four-Predictor Weighted Multinomial Model",
    booktabs = TRUE,
    align = c("l", "c")
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  ) %>%
  kableExtra::footnote(
    general = "The final model included age, BMI, HbA1c, and triglycerides. Performance was averaged across 20 multiply imputed datasets.",
    general_title = "Note: ",
    threeparttable = TRUE
  )

