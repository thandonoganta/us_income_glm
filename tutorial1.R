# setup -------------------------------------------------------------------
# The objective is to predict which people will have an annual income exceeding 50.000

setwd("~/Quants/Fitting GLM/Tutorial1")

library(dplyr)
library(janitor)
library(ggplot2)
library(GGally)
library(ROCR)

# get data ----------------------------------------------------------------

data_adult <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/adult.csv")

# clean data --------------------------------------------------------------

data_clean <- data_adult %>% clean_names()
glimpse(data_clean)

# check continous variables -----------------------------------------------

cont_var <- select_if(.tbl = data_clean, .predicate = is.numeric)
summary(cont_var)


# delving deeper into the continous variables
# we see that hours_per_week has a large interquartile range
# we plot it to pick up any outliers


# Histogram with density curve
ggplot(data = cont_var, mapping = aes(x = hours_per_week)) +
  geom_density(alpha=0.2, fill="#FF6666") +
  theme_minimal()

# we see that the dbn is not well-defined and there are in fact numerous outliers
# to remedy this we shave the top 0.01 percent of the hours_per_week

top_one_per <- quantile(x = cont_var$hours_per_week, probs = 0.99)
# tells us that 1% of people work more than 80 hours per week
# synonymous with 99% of people working under 80 hours per week

# dropping the 1% from our data set
data_clean_drop <- data_clean %>% 
                   filter(hours_per_week < top_one_per)

dim(data_clean)[1] - dim(data_clean_drop)[1]
# tells us that 528 records were dropped because they belonged to the top 1%

# plot the new data set without the outliers
ggplot(data = select_if(.tbl = data_clean_drop, .predicate = is.numeric), mapping = aes(x = hours_per_week)) +
  geom_density(alpha=0.2, fill="#FF6666") +
  theme_minimal()

# looks significantly better

# standardizing the continous variables because the scale is different
data_clean_scale <- data_clean_drop %>% 
                    mutate_if(.tbl = ., .predicate = is.numeric, .funs = funs(as.numeric(scale(x = .))))

head(data_clean_scale)
# factor variables --------------------------------------------------------
# selecting the categorical variables

factor_cols <- data.frame(select_if(.tbl = data_clean_scale, .predicate = is.factor))

# we plot each of the factor variables
factor_chart <- lapply(names(factor_cols)
                         , function(x) 
                              ggplot(data = factor_cols, aes(x = get(x))) +
                              geom_bar() +
                              theme(axis.text.x = element_text(angle = 90)
                                    , panel.background = element_rect(fill = "white")
                                    , panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                    colour = "black")
                                    , panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                    colour = "black")))
# working class
wc_chart <- factor_chart[1]

# education
edu_chart <- factor_chart[2]

# maritul_status
marital_chart <- factor_chart[3]

# race
race_chart <- factor_chart[4]

# gender
gender_chart <- factor_chart[5]

# income
income_chart <- factor_chart[6]


# Feature Engineering -----------------------------------------------------
# recasting some factors to different levels to extract more info
# Education
# Low level of education will be converted in dropout. 
# Higher levels of education will be changed to master.

factor_cols$education %>% levels() # 16 levels

recast_data <- data_clean_scale %>% 
               select(-x) %>% 
               mutate(education = factor(ifelse(test = education %in% c("12th", "11th", "10th", "9th", "7th-8th", "5th-6th", "1st-4th", "Preschool")
                                                , yes = "Dropout"
                                                , no = ifelse(test = education == "HS-grad"
                                                              , yes = "HighGrad"
                                                              , no = ifelse(test = education %in% c("Some-college", "Assoc-acdm", "Assoc-voc")
                                                                            , yes = "Community"
                                                                            , no = ifelse(test = education == "Bachelors"
                                                                                          , yes = "Bachelors"
                                                                                          , no = ifelse(test = education %in% c("Masters", "Prof-school")
                                                                                                        , yes = "Masters"
                                                                                                        , no = ifelse(test = education == "Doctorate"
                                                                                                                      , yes = "PhD"
                                                                                                                      , no = "Unknown"))))))))
# checking the numbers of people in each education group
table(recast_data$education)

# checking on ave how long it takes to reach each education level
recast_data %>% 
  group_by(education) %>% 
  summarize(ave_edu_year = mean(educational_num), count = n()) %>% 
  arrange(ave_edu_year)


# marriage
recast_data <- recast_data %>% 
               mutate(marital_status = ifelse(test = marital_status %in% c("Never-married", "Married-spouse-absent")
                                               ,yes = "not_married"
                                               , no = ifelse(test = marital_status %in% c("Married-civ-spouse", "Married-AF-spouse")
                                                             , yes = "married"
                                                             , no = ifelse(test = marital_status %in% c("Separated", "Divorced")
                                                                           , yes = "seperated"
                                                                           , no = ifelse(test = marital_status == "Widowed"
                                                                                         , yes = "widowed"
                                                                                         , no = "none")))))
# gives us the number of individuals in each group
table(recast_data$marital_status)


# summary statistics ------------------------------------------------------
# plot gender income
gender_income_chart <- ggplot(data = recast_data, mapping = aes(x = gender, fill = income)) +
                       geom_bar(position = "fill") +
                       theme_minimal()

# plot race income
race_income_chart <- ggplot(data = recast_data, mapping = aes(x = race, fill = income)) +
                     geom_bar(position = "fill") +
                     theme_minimal() + 
                     theme(axis.text.x = element_text(angle = 90))

# box plot gender working time
gender_time <- ggplot(data = recast_data, mapping = aes(x = gender, y = hours_per_week)) +
               geom_boxplot() +
               stat_summary(fun = mean, geom = "point", size = 3, color = "steelblue") +
               theme_minimal()

# plot distribution of working time by education
time_edu <- ggplot(data = recast_data, mapping = aes(x = hours_per_week)) +
            geom_density(mapping = aes(color = education), alpha = 0.5) +
            theme_minimal()



# to test the hypothesis of education vs hours of work per week
# i run a one-way Anova test
anova_one <- aov(formula = hours_per_week ~ education, data = recast_data)
summary(anova_one)
# the test confirms that there is a significant relationship between
# education level and hours of work per week

# checking non-linearity between working hours and age
hours_age_chart <- 
ggplot(data = recast_data, mapping = aes(x = age, y = hours_per_week)) +
geom_point(mapping = aes(color = income), size = 0.5) +
stat_smooth(method = "lm", formula = y~poly(x = x, degree = 2), se = TRUE, mapping = aes(color = income)) +
theme_minimal()  

# checking correlation
# convert the data to numeric
corr <- data.frame(lapply(recast_data, as.integer))
# plot the correlation chart
ggcorr(data = corr, method = c("pairwise", "spearman")
       , nbreaks = 6
       , label = TRUE
       , hjust = 0.8
       , label_size = 3
       , color = "grey50")


# train/ test set ---------------------------------------------------------
# all supervised ml tasks need data to be split between a 
# train and a test data set

create_train_set <- function(data, size = 0.8, train = TRUE){
  n_row <- nrow(data)
  total_row <- size * n_row
  train_sample <- 1:total_row
  if (train == TRUE){
    return(data[train_sample, ])
  } else {
      return(data[-train_sample, ])
  }
    
}

data_train <- create_train_set(data = recast_data, size = 0.8, train = TRUE)
data_test <- create_train_set(data = recast_data, size = 0.8, train = FALSE)

dim(data_train)
dim(data_test)


# build the model ---------------------------------------------------------

formula <- income ~ .
logit <- glm(formula = formula, family = "binomial", data = recast_data)
summary(logit)

# the summary of a logit model can be evaluated using a few metrics
logit$coefficients
logit$aic # akaike information criteria


# assessing model performance ---------------------------------------------

# confusion matrix
# counts the number of times True instances are classified as False
# to compute the confusion matrix I need to have a set of predictions for the test data
# that can then be compared to the actual targets
predict_one <- predict(object = logit, newdata = data_test, type = "response")
table_mat <- table(data_test$income, predict_one > 0.5)

# accuracy test
# is the ratio of correct predictions over total no. of cases
# sum of true positives + true negatives divided by tn+fn+tp+fp
accuracy_test <- sum(diag(table_mat))/sum(table_mat)

# precision test
# calculates the accuracy of the positive prediction
precision <- function(matrix) {
  # True Positive
  tp <- matrix[2, 2]
  # False Positive
  fp <- matrix[1, 2]
  return(tp/ (tp + fp))
}

prec <- precision(table_mat)

# the model can claim (correctly) that individuals earn >= 50K
# 71% of the time

# recall test
# is the ratio of positive instances that are correctly detected by 
# the classifier
recall <- function(matrix){
  # True Positive
  tp <- matrix[2, 2] # False Positive
  fn <- matrix[2, 1]
  return(tp/ (tp + fn))
}

rec <- recall(table_mat)
# when the model says the individual's income >= 50K, it is
# correct only 53% of the time.

# creating an F1 score based on the precision and recall
# the F1 is the harmonic mean of these two metrics
f1 <- 2 * ((prec * rec)/(prec + rec))

# it is impossible to have both a high recall and a high precision
# if we increase the precision, the correct individual will be
# predicted but we would miss a lot of them (poor recall)
# In some situations though, we would prefer higher recall than
# precision. There is an inverse relationship between recall
# and precision.


# ROC Curve - Receiver Operating Characteristics
# Another common tool used with binary classification
# Shows the true positive rate (i.e. recall) vs the false
# positive rate. The FP rte is the ratio of negative instances
# that are incorrectly classified as positive = 1-(true negative rate)
# which is called Specifity whilst recall is called Sensitivity

rocr_pred <- prediction(predictions = predict, labels = data_test$income)
rocr_perf <- performance(prediction.obj = rocr_pred, measure = "tpr", x.measure = "fpr")
plot(x = rocr_perf, colorize = TRUE, text.adj = c(-0.2, 1.7))


# Improving the model -----------------------------------------------------
# Adding two interaction terms - gender: hours_per_week
# and age: hours_per_week 

formula_2 <- income ~ age: hours_per_week + gender: hours_per_week + .
logit_2 <- glm(formula = formula_2, family = "binomial", data = data_train)
summary(logit_2)
predict_2 <- predict(object = logit_2, newdata = data_test, type = "response")
table_mat_2 <- table(data_test$income, predict_2 > 0.5)
precision_2 <- precision(table_mat_2)
recall_2 <- recall(matrix = table_mat_2)

# new f1 test
f1_2 <- 2 * ((precision_2 * recall_2) / (precision_2 + recall_2))
f1_2
