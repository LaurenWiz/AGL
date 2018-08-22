library(tfestimators)
library(tidyverse)
library(titanic)

cols <- feature_columns(
  column_categorical_with_vocabulary_list("Sex", vocabulary_list = list("male", "female")),
  column_categorical_with_vocabulary_list("Embarked", vocabulary_list = list("S", "C", "Q", "")),
  column_numeric("Pclass")
)
model <- linear_classifier(feature_columns = cols)

titanic_set <- titanic_train %>%
  filter(!is.na(Age))

glimpse(titanic_set)
indices <- sample(1:nrow(titanic_set), size = 0.80 * nrow(titanic_set))
train <- titanic_set[indices, ]
test  <- titanic_set[-indices, ]
titanic_input_fn <- function(data) {
  input_fn(data, 
           features = c("Sex",
                        "Pclass",
                        "Embarked"), 
           response = "Survived")
}

train(model, titanic_input_fn(train))
model_eval <- evaluate(model, titanic_input_fn(test))
model_eval %>%
  flatten() %>%
  as_tibble() %>%
  glimpse()

tensorboard(model$estimator$model_dir, launch_browser = TRUE)

model_predict <- predict(model, titanic_input_fn(test))
tidy_model <- model_predict %>%
  map(~ .x %>%
        map(~.x[[1]]) %>%
        flatten() %>% 
        as_tibble()) %>%
  bind_rows() %>%
  bind_cols(test)

tidy_model
