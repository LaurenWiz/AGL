library(tfestimators)
library(tidyverse)
library(titanic)

cols <- feature_columns( #we need to have an idea of what columns are likely going to be important BEFORE we design the model
  column_categorical_with_vocabulary_list("Sex", vocabulary_list = list("male", "female")),
  column_categorical_with_vocabulary_list("Embarked", vocabulary_list = list("S", "C", "Q", "")),
  column_numeric("Pclass"),
  column_numeric("Age") #I added age, but it made the model's accuracy worse!
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
                        "Embarked",
                        "Age"), 
           response = "Survived")
}

train(model, titanic_input_fn(train))
model_eval <- evaluate(model, titanic_input_fn(test))


model_eval %>%
  flatten() %>%
  as_tibble() %>%
  glimpse()

#tensorboard(model$estimator$model_dir, launch_browser = TRUE)

model_predict <- predict(model, titanic_input_fn(test))
saved_model_dir<-model_dir(model)

loaded_model <- linear_regressor(feature_columns = cols,
                                 model_dir = saved_model_dir)
loaded_model

tidy_model <- model_predict %>%
  map(~ .x %>%
        map(~.x[[1]]) %>%
        flatten() %>% 
        as_tibble()) %>%
  bind_rows() %>%
  bind_cols(test)

tidy_model
