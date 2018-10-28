library(sparklyr)
library(dplyr)
library(tidyr)
library(titanic)
library(ggplot2)
library(purrr)

# Connect to spark load data
sc <- spark_connect(master = "local", version = "2.0.0")
spark_read_parquet(sc, name = "titanic", path = "C:\\Users\\chait\\Desktop\\VIT\\PDC\\Project\\titanic-parquet")
titanic_tbl <- tbl(sc, "titanic")

# Spark SQL API
titanic2_tbl <- titanic_tbl %>% 
  mutate(Family_Size = SibSp + Parch + 1L) %>% 
  mutate(Pclass = as.character(Pclass)) %>%
  filter(!is.na(Embarked)) %>%
  mutate(Age = if_else(is.na(Age), mean(Age), Age)) %>%
  sdf_register("titanic2")

# Spark ML API
titanic_final_tbl <- titanic2_tbl %>%
  mutate(Family_Size = as.numeric(Family_size)) %>%
  sdf_mutate(
    Family_Sizes = ft_bucketizer(Family_Size, splits = c(1,2,5,12))
  ) %>%
  mutate(Family_Sizes = as.character(as.integer(Family_Sizes))) %>%
  sdf_register("titanic_final")

# Partition the data
partition <- titanic_final_tbl %>% 
  mutate(Survived = as.numeric(Survived), SibSp = as.numeric(SibSp), Parch = as.numeric(Parch)) %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Family_Sizes) %>%
  sdf_partition(train = 0.75, test = 0.25, seed = 8585)

# Create table references
train_tbl <- partition$train
test_tbl <- partition$test

##Logistic Regression

# Model survival as a function of several predictors
ml_formula <- formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Sizes)

# Train a logistic regression model
(ml_log <- ml_logistic_regression(train_tbl, ml_formula))

## Decision Tree
ml_dt <- ml_decision_tree(train_tbl, ml_formula)

## Random Forest
ml_rf <- ml_random_forest(train_tbl, ml_formula)

## Gradient Boosted Tree
ml_gbt <- ml_gradient_boosted_trees(train_tbl, ml_formula)

## Naive Bayes
ml_nb <- ml_naive_bayes(train_tbl, ml_formula)

## Neural Network
ml_nn <- ml_multilayer_perceptron(train_tbl, ml_formula, layers = c(11,15,2))

# Bundle the modelss into a single list object
ml_models <- list(
  "Logistic" = ml_log,
  "Decision Tree" = ml_dt,
  "Random Forest" = ml_rf,
  "Gradient Boosted Trees" = ml_gbt,
  "Naive Bayes" = ml_nb,
  "Neural Net" = ml_nn
)

# Create a function for scoring
score_test_data <- function(model, data=test_tbl){
  pred <- sdf_predict(model, data)
  select(pred, Survived, prediction)
}

# Score all the models
ml_score <- lapply(ml_models, score_test_data)

# Function for calculating accuracy
calc_accuracy <- function(data, cutpoint = 0.5){
  data %>% 
    mutate(prediction = if_else(prediction > cutpoint, 1.0, 0.0)) %>%
    ml_classification_eval("prediction", "Survived", "accuracy")
}

# Calculate AUC and accuracy
perf_metrics <- data.frame(
  model = names(ml_score),
  AUC = 100 * sapply(ml_score, ml_binary_classification_eval, "Survived", "prediction"),
  Accuracy = 100 * sapply(ml_score, calc_accuracy),
  row.names = NULL, stringsAsFactors = FALSE)

# Plot results
gather(perf_metrics, metric, value, AUC, Accuracy) %>%
  ggplot(aes(reorder(model, value), value, fill = metric)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  xlab("") +
  ylab("Percent") +
  ggtitle("Performance Metrics")

# Number of reps per model
n <- 10

# Format model formula as character
format_as_character <- function(x){
  x <- paste(deparse(x), collapse = "")
  x <- gsub("\\s+", " ", paste(x, collapse = ""))
  x
}

# Create model statements with timers
format_statements <- function(y){
  y <- format_as_character(y[[".call"]])
  y <- gsub('ml_formula', ml_formula_char, y)
  y <- paste0("system.time(", y, ")")
  y
}

# Convert model formula to character
ml_formula_char <- format_as_character(ml_formula)

# Create n replicates of each model statements with timers
all_statements <- sapply(ml_models, format_statements) %>%
  rep(., n) %>%
  parse(text = .)

# Evaluate all model statements
res  <- map(all_statements, eval)

# Compile results
result <- data.frame(model = rep(names(ml_models), n),
                     time = sapply(res, function(x){as.numeric(x["elapsed"])})) 

# Plot
result %>% ggplot(aes(time, reorder(model, time))) + 
  geom_boxplot() + 
  geom_jitter(width = 0.4, aes(colour = model)) +
  scale_colour_discrete(guide = FALSE) +
  xlab("Seconds") +
  ylab("") +
  ggtitle("Model training times")