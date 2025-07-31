#Clean canvas ----
#clears console
cat("\014")
#clears the variables
rm(list=ls())
#clears the plots
dev.off() 


library(lubridate)
library(dplyr)
library(magrittr)
library(caret)
library(e1071)
library(pROC) 
library(corrplot)
library(ggplot2)

## Using read.csv to read the AmesHousing.csv file
flight <- read.csv("f1.csv")

str(flight)

filtered_month <- flight[flight$MONTH %in% c(12), ] #update according to the selected month


##EDA

variables_to_remove <- c('TAXI_OUT', 'TAXI_IN', 'WHEELS_ON', 'WHEELS_OFF', 'YEAR', 
                         'MONTH', 'DAY', 'DAY_OF_WEEK', 'DATE',
                       'SECURITY_DELAY', 'AIRLINE_DELAY', 'LATE_AIRCRAFT_DELAY', 'AIR_SYSTEM_DELAY',
                      'WEATHER_DELAY', 'DIVERTED', 'CANCELLATION_REASON',
                     'FLIGHT_NUMBER', 'TAIL_NUMBER', 'AIR_TIME')

cl_flight <- filtered_month[, !names(filtered_month) %in% variables_to_remove]


missing_values1 <- colSums(is.na(cl_flight))
missing_df1 <- data.frame(variable = names(missing_values1), 
                         missing_values1 = missing_values1,
                         filling_factor = ((nrow(cl_flight) - missing_values1) / nrow(cl_flight)) * 100)
missing_df1 <- missing_df1[order(missing_df1$filling_factor), ]
rownames(missing_df1) <- NULL


cl_flight <- na.omit(cl_flight)

airlines_names<- read.csv('airlines.csv')
airlines_names


companies <- setNames(airlines_names$AIRLINE, airlines_names$IATA_CODE)
companies

log_delays <- log(cl_flight$DEPARTURE_DELAY + 1)


cl_flight$log_delays <- log(cl_flight1$DEPARTURE_DELAY + 1)  # Log-transform; adding 1 to avoid log(0)

# Load the ggplot2 package if it isn't already loaded
library(ggplot2)

# Generate the Q-Q plot
ggplot(cl_flight, aes(sample = log_delays)) +  # Ensure 'log_delays' is the correct column name
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "Q-Q Plot of Log-Transformed Departure Delays",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

data_vector <- rnorm(100)  # Normally distributed sample for demonstration

# Remove NA values if necessary
data_vector <- na.omit(data_vector)

# Generate the Q-Q plot
qqnorm(data_vector, main = "Q-Q Plot of Data", ylab = "Quantiles of Data", xlab = "Theoretical Quantiles")
qqline(data_vector, col = "red", lwd = 2)  # Adds a reference line



# Plot histogram of the log-transformed data
ggplot(cl_flight, aes(x = log_delays)) +  # Ensure 'log_delays' is the correct column name
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +  # Set number of bins and colors
  labs(title = "Histogram of Log-Transformed Departure Delays",
       x = "Departure Delay",
       y = "Frequency") +
  theme_minimal() 

get_stats <- function(group) {
  return(data.frame(count = length(group), mean = mean(group),min = min(group), max = max(group),
                    # Name the columns explicitly
                    stringsAsFactors = FALSE))  # Avoid factor conversion (optional)
}

# Group by airline and calculate statistics
airline_stats <- cl_flight %>%
  group_by(AIRLINE) %>% 
  summarise_at(vars(DEPARTURE_DELAY), get_stats) %>% 
  tidyr::unnest(cols = everything())


# Sort by count (descending) and display the results
airline_stats <- airline_stats[order(airline_stats$count), ]
airline_stats

# Visualization 1 ----
# Selecting the columns 'AIRLINE' and 'DEPARTURE_DELAY' and creating a new dataframe df2
df2 <- cl_flight[, c('AIRLINE', 'DEPARTURE_DELAY')]

# Replace IATA codes with airline names using the named vector companies
df2$AIRLINE <- companies[df2$AIRLINE]

colors <- c('firebrick', 'gold', 'lightcoral', 'aquamarine', 'cyan', 'yellowgreen', 'grey',
            'seagreen', 'tomato', 'violet', 'wheat', 'chartreuse', 'lightskyblue', 'royalblue')



library(ggplot2)

# Define explicit x-axis limits based on your knowledge of the data
max_delay <- max(df2$DEPARTURE_DELAY, na.rm = TRUE)

# Adjust the plot
ggplot(data = df2, aes(x = DEPARTURE_DELAY, y = AIRLINE, fill = AIRLINE, color = AIRLINE)) +
  geom_jitter(shape = 20, size = 1, stroke = 2, alpha = 0.7) +  # Use shape 21 for filled circles with borders
  scale_fill_manual(values = colors) +  # Fill color
  scale_color_manual(values = colors) +  # Border color
  labs(title = "Departure Delay vs Airline (April)", x = "Departure Delay", y = "Airline") +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +  # Adjust the margins
  scale_x_continuous(
    labels = function(x) sprintf("%2.0fh%2.0fm", floor(x / 60), x %% 60),
    limits = c(0, max_delay + 30)  # Adjust limits
  )

# Visualization 2: ----
# Categorize the delay into groups
df <- df2 %>%
  mutate(Delay_Category = case_when(
    DEPARTURE_DELAY <= 5 ~ "on time",
    DEPARTURE_DELAY > 5 & DEPARTURE_DELAY <= 45 ~ "small delay",
    DEPARTURE_DELAY > 45 ~ "large delay"
  ))

# Count the number of flights in each category for each airline
delay_summary <- df %>%
  group_by(AIRLINE, Delay_Category) %>%
  summarise(Count = n(), .groups = 'drop')


# Create the bar plot
ggplot(delay_summary, aes(x = Count, y = reorder(AIRLINE, -Count), fill = Delay_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Flight Delay Categories by Airline (April)",
       x = "Flight count",
       y = "Airline",
       fill = "Delay Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Adjust the x-axis labels for better readability
  scale_fill_manual(values = c("on time" = "skyblue", "small delay" = "orange", "large delay" = "red"))





# Split the data into training and testing sets based on the date
df_train <- cl_flight %>% 
  filter(as.Date(SCHEDULED_DEPARTURE) < as.Date("2022-12-23"))

df_test <- cl_flight %>% 
  filter(as.Date(SCHEDULED_DEPARTURE) > as.Date("2022-12-23"))

# Continue working with the training data
train_data <- df_train

carrier <- 'AA'
# Define a function to compute statistics, assuming you need mean and count
get_stats <- function(x) {
  data.frame(
    mean = mean(x, na.rm = TRUE),
    count = length(x)
  )
}

# Filter the DataFrame for the specified carrier, then calculate statistics per airport
check_airports <- cl_flight %>%
  filter(AIRLINE == carrier) %>%
  group_by(ORIGIN_AIRPORT) %>%
  summarise(
    mean_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count))


# Get the last 5 entries
check_airports_bottom5 <- tail(check_airports, 5)


###linear regression
cl_flight <- left_join(cl_flight, airlines_names, by = c("AIRLINE" = "IATA_CODE"))

cl_flight <- cl_flight %>%
  mutate(WeekOfMonth = week(SCHEDULED_DEPARTURE) - min(week(SCHEDULED_DEPARTURE)) + 1)

# Filter data for Southwest Airlines
southwest_flights <- cl_flight %>%
  filter(AIRLINE.y == "Southwest Airlines Co.")

# Split data into training (weeks 1-3) and testing (week 4)
train_data <- southwest_flights %>% filter(WeekOfMonth <= 3)
test_data <- southwest_flights %>% filter(WeekOfMonth == 4)

# Prepare the model data
#train_data1 <- dplyr::select(train_data, DISTANCE, DEPARTURE_DELAY)
#test_data1 <- dplyr::select(test_data, DISTANCE, DEPARTURE_DELAY)

# Fit a linear regression model
#model <- lm(DEPARTURE_DELAY ~ DISTANCE, data = train_data1)

# Predict and evaluate the model
#predictions <- predict(model, test_data1)
#rmse <- sqrt(mean((test_data$DEPARTURE_DELAY - predictions)^2))

# Output RMSE
#print(paste("Root Mean Squared Error:", rmse))

train_data1 <- dplyr::select(train_data, DISTANCE, DEPARTURE_DELAY,SCHEDULED_TIME)
test_data1 <- dplyr::select(train_data, DISTANCE, DEPARTURE_DELAY,SCHEDULED_TIME)
multi_linear_model <- lm(DEPARTURE_DELAY ~ DISTANCE + SCHEDULED_TIME, data = train_data1)
summary(multi_linear_model)
# Predict using the test dataset
multi_linear_predictions <- predict(multi_linear_model, test_data1)
summary(multi_linear_predictions)
# Calculate RMSE for the multi-linear regression model
multi_linear_rmse <- sqrt(mean((test_data$DEPARTURE_DELAY - multi_linear_predictions)^2))
print(paste("Multi-linear Regression RMSE:", multi_linear_rmse))

#poly_model <- lm(DEPARTURE_DELAY ~ DISTANCE + I(DISTANCE^2), data = train_data1)

# Predict using the test dataset
#poly_predictions <- predict(poly_model, test_data1)

# Calculate RMSE for the polynomial regression model
#poly_rmse <- sqrt(mean((test_data$DEPARTURE_DELAY - poly_predictions)^2))
#print(paste("Polynomial Regression RMSE:", poly_rmse))

######Logistics Model 


missing_values <- colSums(is.na(filtered_month))
missing_df <- data.frame(variable = names(missing_values), 
                         missing_values = missing_values,
                         filling_factor = ((nrow(filtered_month) - missing_values) / nrow(filtered_month)) * 100)
missing_df <- missing_df[order(missing_df$filling_factor), ]
rownames(missing_df) <- NULL


cl_flight1 <- na.omit(filtered_month)

# Assuming your data frame is named 'data_frame'
# Add a new column 'flight_delay'
cl_flight1 <- cl_flight1 %>%
  mutate(flight_delay = ifelse(DEPARTURE_DELAY > 0, 1, 0))

# View the first few rows of the updated data frame to confirm the new column
head(data_frame)

# Assuming 'cl_flight1' is your dataset
data_subset <- cl_flight1 %>% 
  dplyr::select(DEPARTURE_DELAY,DISTANCE, SCHEDULED_TIME, AIR_SYSTEM_DELAY, SECURITY_DELAY, 
                AIRLINE_DELAY, WEATHER_DELAY, LATE_AIRCRAFT_DELAY)

# Compute correlation matrix
cor_matrix <- cor(data_subset, use = "complete.obs")  # Handling missing data if necessary

# Plotting the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Model Variables")

# Transform correlation matrix to 'long' format for ggplot
cor_data <- as.data.frame(as.table(cor_matrix))
colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
cor_data <- cor_data %>%
  filter(Variable1 != Variable2)  # Optionally, remove self-correlations for cleaner plot

# Plot with ggplot2
ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix of Model Variables", fill = "Correlation")



# Add the binary 'flight_delay' column
cl_flight1 <- cl_flight1 %>%
  mutate(flight_delay = ifelse(DEPARTURE_DELAY > 0, 1, 0))

# Ensure 'flight_delay' is a factor
cl_flight1$flight_delay <- factor(cl_flight1$flight_delay, levels = c(0, 1))

# Running a logistic regression model
logistic_model <- glm(flight_delay ~ DISTANCE + SCHEDULED_TIME+AIR_SYSTEM_DELAY+SECURITY_DELAY+AIR_SYSTEM_DELAY+AIRLINE_DELAY+WEATHER_DELAY+LATE_AIRCRAFT_DELAY, data = cl_flight1, family = binomial())

# Summarize the model to view the results
summary(logistic_model)

# Making predictions
predicted_probabilities <- predict(logistic_model, type = "response")

# Convert predicted probabilities to binary outcomes
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Ensure the predicted classes are factors with the same levels as flight_delay
predicted_classes <- factor(predicted_classes, levels = c(0, 1))

# Evaluate the model performance
conf_matrix <- confusionMatrix(data = predicted_classes, reference = cl_flight1$flight_delay)
print(conf_matrix)

# Precision, Recall, and F1-Score
precision <- posPredValue(predicted_classes, cl_flight1$flight_delay, positive = "1")
recall <- sensitivity(predicted_classes, cl_flight1$flight_delay, positive = "1")
f1_score <- (2 * precision * recall) / (precision + recall)

# Print metrics
print(paste("Precision:", precision))
print(paste("Recall (Sensitivity):", recall))
print(paste("F1-Score:", f1_score))

# AUC-ROC
roc_curve <- roc(response = cl_flight1$flight_delay, predictor = as.numeric(predicted_probabilities))
auc_value <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve", col = "#1c61b6", lwd = 4)
print(paste("AUC-ROC:", auc_value))

##############
  # For ROC curve

# Assuming 'cl_flight1' is already loaded and prepared with the necessary columns
set.seed(123)  # for reproducibility
index <- createDataPartition(cl_flight1$flight_delay, p = 0.80, list = FALSE)
train_data <- cl_flight1[index,]
test_data <- cl_flight1[-index,]

# Logistic Regression Model
logistic_model <- glm(flight_delay ~ DISTANCE + SCHEDULED_TIME + AIR_SYSTEM_DELAY +
                        SECURITY_DELAY + AIRLINE_DELAY + WEATHER_DELAY + LATE_AIRCRAFT_DELAY,
                      data = train_data, family = binomial())



# Predictions from Logistic Regression
logistic_predictions <- predict(logistic_model, test_data, type = "response")
logistic_predicted_classes <- ifelse(logistic_predictions > 0.5, 1, 0)




# Evaluate Logistic Regression
logistic_conf_matrix <- confusionMatrix(as.factor(logistic_predicted_classes), as.factor(test_data$flight_delay))
cat("Logistic Regression Evaluation:\n")
print(logistic_conf_matrix)



# ROC and AUC for Logistic Regression
logistic_roc <- roc(as.numeric(test_data$flight_delay), as.numeric(logistic_predictions))
logistic_auc <- auc(logistic_roc)
cat("Logistic Regression AUC:", logistic_auc, "\n")

# SVM Model
svm_model <- svm(flight_delay ~ DISTANCE + SCHEDULED_TIME + AIR_SYSTEM_DELAY +
                   SECURITY_DELAY + AIRLINE_DELAY + WEATHER_DELAY + LATE_AIRCRAFT_DELAY,
                 data = train_data, type = "C-classification", kernel = "radial")
# Predictions from SVM
svm_predicted_classes <- predict(svm_model, test_data)

# Evaluate SVM
svm_conf_matrix <- confusionMatrix(as.factor(svm_predicted_classes), as.factor(test_data$flight_delay))
cat("SVM Evaluation:\n")
print(svm_conf_matrix)

# ROC and AUC for SVM
svm_roc <- roc(as.numeric(test_data$flight_delay), as.numeric(as.integer(svm_predicted_classes)-1))
svm_auc <- auc(svm_roc)
cat("SVM AUC:", svm_auc, "\n")

# Extracting performance metrics from the confusion matrix
precision <- svm_conf_matrix$byClass['Positive Predictive Value']
recall <- svm_conf_matrix$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)

# Printing the precision, recall, and F1 score
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


library(pROC)

# Plotting the ROC curve
plot(svm_roc, main="ROC Curve for SVM Model", col = "#1c61b6")
# Adding AUC to the plot
auc_text <- paste("AUC =", format(svm_auc, digits=4))
legend("bottomright", legend=auc_text, box.lty=1, box.lwd=1, cex=0.8)

# The ROC curve graph will display the sensitivity vs. specificity trade-off,
# and the AUC value will provide a single measure of overall performance of the classifier.



