library(dplyr)
library(lmtest)
library(car)
library(whitestrap)

# Read the table from CSV file
df <- read.csv('Airfare.csv')

# Remove leading or trailing space characters if present
df <- apply(df, 2, function(x) if(is.character(x)) trimws(x) else x)

# Convert the matrix or array to a data frame
df <- as.data.frame(df)

# Convert numeric columns from string to numeric format
df <- df %>%
  mutate_at(vars(COUPON, NEW, HI, S_POP, E_POP, DISTANCE, PAX), as.numeric)

# Replace missing data represented by * with NA
df <- df %>% mutate_all(~ifelse(. == "*", NA, .))

# Drop the last column which is empty
df <- df[, -ncol(df)]

# Check for NA values in all columns
na_counts <- colSums(is.na(df))

# Display the counts of NA values
print(na_counts)

# Since many city codes in source and destination are missing and fares do not differ
# we can delete the columns of the source and destination city codes as only City names are sufficient
df <- select(df, -S_CODE, -E_CODE)

# Convert the 3 columns from string format to numerical columns
df$S_INCOME <- as.numeric(gsub("[\\$,]", "", df$S_INCOME))
df$E_INCOME <- as.numeric(gsub("[\\$,]", "", df$E_INCOME))
df$FARE <- as.numeric(gsub("[\\$,]", "", df$FARE))

# Convert Yes/No columns to binary (0/1)
# 0 - No
# 1 - Yes
df$VACATION <- ifelse(df$VACATION == 'Yes', 1, 0)
df$SW <- ifelse(df$SW == 'Yes', 1, 0)

# Convert Free/Controlled columns to (0/1)
# 0 - Controlled
# 1 - Free
df <- df %>%
  mutate(SLOT_F = ifelse(SLOT == 'Free', 1, 0),
         GATE_F = ifelse(GATE == 'Free', 1, 0)) %>%
  select(-SLOT, -GATE)

# Save as a new cleaned CSV file
write.csv(df, file = 'Airfare_filtered.csv', row.names = FALSE)

# Train/Test Split ratio
train_ratio <- 0.6

# Ignoring City columns as they cannot be converted to dummy columns
df <- select(df, -S_CITY, -E_CITY)

# ------------------------------------------------------------------------------
# 1. LINEAR REGRESSION
# ------------------------------------------------------------------------------

# Train/Test Split
set.seed(42)
df_train <- sample_frac(df, train_ratio)
train_idx <- as.numeric(rownames(df_train))
df_test <- df[-train_idx,]

# Fit the linear regression model
model1 <- lm(FARE ~ ., data = df_train)

# Display model summary
print(summary(model1))

# Predictions
y_train_pred <- predict(model1, newdata = df_train)
y_test_pred <- predict(model1, newdata = df_test)

# Calculate Mean Squared Error
train_mse <- mean((df_train$FARE - y_train_pred)^2)
test_mse <- mean((df_test$FARE - y_test_pred)^2)

cat("TRAIN MSE: ", train_mse, "\n")
cat("TEST MSE: ", test_mse, "\n")

par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))


# ------------------------------------------------------------------------------
# 1.b LINEAR REGRESSION (Considering only significant variables)
# removed Coupon, s_income and New - not significant acc to previous model
# ------------------------------------------------------------------------------

# Fit the linear regression model
model1b <- lm(FARE ~ . - S_INCOME - COUPON - NEW, data = df_train)

# Display model summary
print(summary(model1b))

# Predictions
y_train_pred <- predict(model1b, newdata = df_train)
y_test_pred <- predict(model1b, newdata = df_test)

# Calculate Mean Squared Error
train_mse <- mean((df_train$FARE - y_train_pred)^2)
test_mse <- mean((df_test$FARE - y_test_pred)^2)

cat("TRAIN MSE: ", train_mse, "\n")
cat("TEST MSE: ", test_mse, "\n")

par(mfrow=c(2,2))
plot(model1b)
par(mfrow=c(1,1))


# ------------------------------------------------------------------------------
# 2. LOG-LINEAR REGRESSION
# ------------------------------------------------------------------------------

# Train/Test Split
X_train <- df_train %>%
  select(-FARE)
X_test <- df_test %>%
  select(-FARE)
y_train <- df_train$FARE
y_test <- df_test$FARE

# Fit the log-linear regression model
model2 <- lm(log(y_train) ~ ., data = X_train)

# Display model summary
print(summary(model2))

# Predictions
y_train_pred <- exp(predict(model2, newdata = X_train))
y_test_pred <- exp(predict(model2, newdata = X_test))

# Calculate Mean Squared Error
train_mse <- mean((y_train - y_train_pred)^2)
test_mse <- mean((y_test - y_test_pred)^2)

cat("TRAIN MSE: ", train_mse, "\n")
cat("TEST MSE: ", test_mse, "\n")

par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

# ------------------------------------------------------------------------------
# 3. LINEAR-LOG REGRESSION
# ------------------------------------------------------------------------------

# Train/Test Split
X_train <- df_train %>%
  select(-FARE)
X_test <- df_test %>%
  select(-FARE)
y_train <- df_train$FARE
y_test <- df_test$FARE

X_train$COUPON = log(X_train$COUPON)
X_train$NEW = log(X_train$NEW + 1)
X_train$HI = log(X_train$HI)
X_train$S_INCOME = log(X_train$S_INCOME)
X_train$E_INCOME = log(X_train$E_INCOME)
X_train$S_POP = log(X_train$S_POP)
X_train$E_POP = log(X_train$E_POP)
X_train$DISTANCE = log(X_train$DISTANCE)
X_train$PAX = log(X_train$PAX)


X_test$COUPON = log(X_test$COUPON)
X_test$NEW = log(X_test$NEW + 1)
X_test$HI = log(X_test$HI)
X_test$S_INCOME = log(X_test$S_INCOME)
X_test$E_INCOME = log(X_test$E_INCOME)
X_test$S_POP = log(X_test$S_POP)
X_test$E_POP = log(X_test$E_POP)
X_test$DISTANCE = log(X_test$DISTANCE)
X_test$PAX = log(X_test$PAX)


# Fit the log-linear regression model
model3 <- lm(y_train ~ ., data = X_train)

# Display model summary
print(summary(model3))

# Predictions
y_train_pred <- predict(model3, newdata = X_train)
y_test_pred <- predict(model3, newdata = X_test)

# Calculate Mean Squared Error
train_mse <- mean((y_train - y_train_pred)^2)
test_mse <- mean((y_test - y_test_pred)^2)

cat("TRAIN MSE: ", train_mse, "\n")
cat("TEST MSE: ", test_mse, "\n")

par(mfrow=c(2,2))
plot(model3)
par(mfrow=c(1,1))


# ------------------------------------------------------------------------------
# 4. LOG-LOG REGRESSION
# ------------------------------------------------------------------------------

# Train/Test Split
X_train <- df_train %>%
  select(-FARE)
X_test <- df_test %>%
  select(-FARE)
y_train <- df_train$FARE
y_test <- df_test$FARE

X_train$COUPON = log(X_train$COUPON)
X_train$NEW = log(X_train$NEW + 1)
X_train$HI = log(X_train$HI)
X_train$S_INCOME = log(X_train$S_INCOME)
X_train$E_INCOME = log(X_train$E_INCOME)
X_train$S_POP = log(X_train$S_POP)
X_train$E_POP = log(X_train$E_POP)
X_train$DISTANCE = log(X_train$DISTANCE)
X_train$PAX = log(X_train$PAX)


X_test$COUPON = log(X_test$COUPON)
X_test$NEW = log(X_test$NEW + 1)
X_test$HI = log(X_test$HI)
X_test$S_INCOME = log(X_test$S_INCOME)
X_test$E_INCOME = log(X_test$E_INCOME)
X_test$S_POP = log(X_test$S_POP)
X_test$E_POP = log(X_test$E_POP)
X_test$DISTANCE = log(X_test$DISTANCE)
X_test$PAX = log(X_test$PAX)


# Fit the log-linear regression model
model4 <- lm(log(y_train) ~ ., data = X_train)

# Display model summary
print(summary(model4))

# Predictions
y_train_pred <- exp(predict(model4, newdata = X_train))
y_test_pred <- exp(predict(model4, newdata = X_test))

# Calculate Mean Squared Error
train_mse <- mean((y_train - y_train_pred)^2)
test_mse <- mean((y_test - y_test_pred)^2)

cat("TRAIN MSE: ", train_mse, "\n")
cat("TEST MSE: ", test_mse, "\n")

par(mfrow=c(2,2))
plot(model4)
par(mfrow=c(1,1))


# ------------------------------------------------------------------------------
# 4.b LOG-LOG REGRESSION (Considering only significant variables)
# removed Coupon and New - not significant
# ------------------------------------------------------------------------------

# Train/Test Split
df_train <- select(df_train, -COUPON, -NEW)
df_test <- select(df_test, -COUPON, -NEW)

X_train <- df_train %>%
  select(-FARE)
X_test <- df_test %>%
  select(-FARE)
y_train <- df_train$FARE
y_test <- df_test$FARE

X_train$HI = log(X_train$HI)
X_train$S_INCOME = log(X_train$S_INCOME)
X_train$E_INCOME = log(X_train$E_INCOME)
X_train$S_POP = log(X_train$S_POP)
X_train$E_POP = log(X_train$E_POP)
X_train$DISTANCE = log(X_train$DISTANCE)
X_train$PAX = log(X_train$PAX)


X_test$HI = log(X_test$HI)
X_test$S_INCOME = log(X_test$S_INCOME)
X_test$E_INCOME = log(X_test$E_INCOME)
X_test$S_POP = log(X_test$S_POP)
X_test$E_POP = log(X_test$E_POP)
X_test$DISTANCE = log(X_test$DISTANCE)
X_test$PAX = log(X_test$PAX)


# Fit the log-linear regression model
model4b <- lm(log(y_train) ~ ., data = X_train)

# Display model summary
print(summary(model4b))

# Predictions
y_train_pred <- exp(predict(model4b, newdata = X_train))
y_test_pred <- exp(predict(model4b, newdata = X_test))

# Calculate Mean Squared Error
train_mse <- mean((y_train - y_train_pred)^2)
test_mse <- mean((y_test - y_test_pred)^2)

cat("TRAIN MSE: ", train_mse, "\n")
cat("TEST MSE: ", test_mse, "\n")

par(mfrow=c(2,2))
plot(model4b)
par(mfrow=c(1,1))