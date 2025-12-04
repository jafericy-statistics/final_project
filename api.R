#initial settings
library(tidyverse)
library(tidymodels)
library(plumber)
tidymodels_prefer()

#load data
diab_raw <- read_csv(
  "diabetes_binary_health_indicators_BRFSS2015.csv",
  show_col_types = FALSE
)

#Need response factor with labels, this changes the data
diab <- diab_raw |>
  mutate(
    Diabetes_binary = factor(
      Diabetes_binary,
      levels = c(0, 1),
      labels = c("No", "Diabetes")
    )
  )

#define the pred var names for the model
predictors <- c(
  "HighBP", "HighChol", "BMI", "Smoker",
  "PhysActivity", "GenHlth", "Age", "Income"
)

#define the recipe for data preprocessing
diab_rec <- recipe(
  Diabetes_binary ~ HighBP + HighChol + BMI + Smoker +
    PhysActivity + GenHlth + Age + Income,
  data = diab
) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

#tuned model hyperparameters from modeling.qmd file that we discussed
best_mtry  <- 3
best_min_n <- 40

#RF model spec
rf_best_spec <- rand_forest(
  mode  = "classification",
  mtry  = best_mtry,
  min_n = best_min_n,
  trees = 500
) |>
  set_engine("ranger", importance = "impurity")

#wf combining recipe and model for robust model pipeline
rf_best_wf <- workflow() |>
  add_model(rf_best_spec) |>
  add_recipe(diab_rec)

#let's fit the model on full dataset
set.seed(321)
rf_best_fit <- fit(rf_best_wf, data = diab)

#need to compute preds on full data for confusion matrix
diab_preds <- predict(rf_best_fit, new_data = diab, type = "class") |>
  bind_cols(
    predict(rf_best_fit, new_data = diab, type = "prob"),
    diab |> select(Diabetes_binary)
  ) |>
  rename(pred_class = .pred_class)

#helper func to get vector mode
mode_val <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#num and cat preds for downstream
numeric_predictors <- c("BMI")
categorical_predictors <- setdiff(predictors, numeric_predictors)

#default vals for num preds -mean
num_defaults_tbl <- diab |>
  summarize(across(all_of(numeric_predictors), ~ mean(.x, na.rm = TRUE)))

#default vals for cat preds - mode
cat_defaults_tbl <- diab |>
  summarize(across(all_of(categorical_predictors), ~ mode_val(.x)))

#combine as list
defaults <- bind_cols(num_defaults_tbl, cat_defaults_tbl) |>
  slice(1) |>
  as.list()

#* PROJECT INFORMATION
#* @get /info
function() {
  list(
    name = "Jacob A. Fericy",
    github_pages = "https://jafericy-statistics.github.io/final_project/",
    model_type = sprintf(
      "Random forest with mtry = %d, min_n = %d, trees = %d",
      best_mtry, best_min_n, 500
    ),
    predictors = predictors
  )
}

#* predict diabetes api probability
#* @param HighBP High blood pressure indicator (0/1)
#* @param HighChol High cholesterol indicator (0/1)
#* @param BMI Body Mass Index (numeric)
#* @param Smoker Smoking history indicator (0/1)
#* @param PhysActivity Recent physical activity (0/1)
#* @param GenHlth General health (1–5)
#* @param Age Age group code (integer category)
#* @param Income Income category (integer category)
#* @get /pred
function(
    HighBP       = defaults$HighBP,
    HighChol     = defaults$HighChol,
    BMI          = defaults$BMI,
    Smoker       = defaults$Smoker,
    PhysActivity = defaults$PhysActivity,
    GenHlth      = defaults$GenHlth,
    Age          = defaults$Age,
    Income       = defaults$Income
) {
  
  #one-row data frame from query params
  new_data <- tibble(
    HighBP       = as.numeric(HighBP),
    HighChol     = as.numeric(HighChol),
    BMI          = as.numeric(BMI),
    Smoker       = as.numeric(Smoker),
    PhysActivity = as.numeric(PhysActivity),
    GenHlth      = as.numeric(GenHlth),
    Age          = as.numeric(Age),
    Income       = as.numeric(Income)
  )
  
  #need pred probs and class
  prob  <- predict(rf_best_fit, new_data = new_data, type = "prob")
  class <- predict(rf_best_fit, new_data = new_data, type = "class")
  
  #output as list
  list(
    input = new_data,
    prediction = list(
      class            = class$.pred_class[[1]],
      prob_no_diabetes = prob$.pred_No[[1]],
      prob_diabetes    = prob$.pred_Diabetes[[1]]
    )
  )
}

#* confusion matrix heatmap for the full-data model
#* @serializer png
#* @get /confusion
function() {
  cm <- conf_mat(
    diab_preds,
    truth = Diabetes_binary,
    estimate = pred_class
  )
  
  p <- autoplot(cm, type = "heatmap")
  
  print(p)
}