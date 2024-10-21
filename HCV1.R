


install.packages("corrplot")


# Load required libraries
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(gridExtra)

# Load the dataset

hcv <- read_csv("C:/Users/Lib 003/Desktop/Kaggle Website Research/Hepatitis Blood test/hcv.csv")

######Remove the column containing the patient ID

hcv <- hcv %>% select(-1)

# Inspect the dataset

str(hcv)
summary(hcv)

######################summary sta
num_cols <- sapply(hcv, is.numeric)
summary(hcv[, num_cols])


# Check for missing values

missing_values <- colSums(is.na(hcv))
missing_values

############## # Remove rows with NA in any column

library(dplyr)


hcv <- hcv %>% na.omit()

# Check the cleaned dataset

summary(hcv[,num_cols])

# Summary statistics of numerical columns

num_cols <- sapply(hcv, is.numeric)
summary(hcv[, num_cols])

# Distribution of categorical variables

cat_cols <- sapply(hcv, is.factor)
cat_cols<-(hcv[, cat_cols])


# Histograms for numerical variables

num_plots <- lapply(names(hcv)[num_cols], function(col) {
  ggplot(hcv, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal()
})


# Split plots into two sets of 10
plots_set1 <- num_plots[1:6]
plots_set2 <- num_plots[7:12]

# Calculate the number of rows and columns for the grid
n_cols <- 2
n_rows <- ceiling(length(plots_set1) / n_cols)

# Arrange and display the first set of plots in a grid layout
do.call(grid.arrange, c(plots_set1, ncol = n_cols))

# Arrange and display the second set of plots in a grid layout
do.call(grid.arrange, c(plots_set2, ncol = n_cols))



# Box plots for numerical variables

box_plots <- lapply(names(hcv)[num_cols], function(col) {
  ggplot(hcv, aes_string(x = "Category", y = col)) +
    geom_boxplot(fill = "orange", alpha = 0.7, cex=0.5) +
    labs(title = paste("Box plot of", col, "by Category"), x = "Category", y = col) +
    theme_minimal()
})


# Split plots into two sets of 10
plots_set1 <- box_plots[1:4]
plots_set2 <- box_plots[5:8]
plots_set3 <-box_plots[9:12]

# Calculate the number of rows and columns for the grid
n_cols <- 2
n_rows <- ceiling(length(plots_set1) / n_cols)

# Arrange and display the first set of plots in a grid layout
do.call(grid.arrange, c(plots_set1, ncol = n_cols))

# Arrange and display the second set of plots in a grid layout
do.call(grid.arrange, c(plots_set2, ncol = n_cols))

# Arrange and display the third set of plots in a grid layout
do.call(grid.arrange, c(plots_set3, ncol = n_cols))



# Plot bar plot for a categorical variable

ggplot(hcv, aes(x = Category)) + 
  geom_bar(fill = "green", color = "black", alpha=0.7) + 
  theme_minimal() + 
  labs(title = "Bar Plot of Category", x = "Category", y = "Count")


###### plot bar plot for Sex variable

ggplot(hcv, aes(x = Sex)) + 
  geom_bar(fill = "skyblue", color = "black", alpha=0.7) + 
  theme_minimal() + 
  labs(title = "Bar Plot of Sex", x = "Sex", y = "Count")



# Calculate the counts and convert to a data frame
category_counts <- hcv %>%
  count(Category) %>%
  mutate(perc = n / sum(n) * 100)

# Plot pie chart of category
ggplot(category_counts, aes(x = "", y = perc, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Pie Chart of Category", fill = "Category") + 
  geom_text(aes(label = sprintf("%.2f%%", perc)), 
            position = position_stack(vjust = 0.7))



# Plot pie chart of Sex


# Calculate the counts and convert to a data frame
Sex_counts <- hcv %>%
  count(Sex) %>%
  mutate(perc = n / sum(n) * 100)


ggplot(Sex_counts, aes(x = "", y = perc, fill = Sex)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Pie Chart of Sex", fill = "Sex") + 
  geom_text(aes(label = sprintf("%.2f%%", perc)), 
            position = position_stack(vjust = 0.7))



################To check the uniqueness of the data


# Load necessary library
library(dplyr)

unique_categories <- unique(hcv$Category)

# Print the unique categories
print(unique_categories)



##########Now I want to transform the category and sex column into numerical value



# Load necessary library
library(dplyr)


hcv <- data.frame(
  Category = c('0=Blood Donor', '0s=suspect Blood Donor', '1=Hepatitis', '2=Fibrosis', '3=Cirrhosis', '0=Blood Donor', '1=Hepatitis'),
  Sex = c('m', 'f')
)


hcv <- hcv %>%
  mutate(
    Category = recode(Category,
                      `0=Blood Donor` = 0,
                      `0s=suspect Blood Donor` = 0,
                      `1=Hepatitis` = 1,
                      `2=Fibrosis` = 1,
                      `3=Cirrhosis` = 1
    ),
    Sex = recode(Sex,
                 `m` = 1,
                 `f` = 2
    )
  )

# Print the transformed data frame
print(hcv)


# Correlation matrix for numerical variables
# Calculate the correlation matrix
# Install and load ggcorrplot

install.packages("ggcorrplot")
library(ggcorrplot)




# Calculate the correlation matrix
correlation_matrix <- cor(hcv, use = "complete.obs")

# Create the lower triangular correlation matrix heatmap
ggcorrplot(correlation_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3, colors = c("green", "white", "orange"), title = "Correlation Heap Matrix", ggtheme = theme_minimal() )





# Define the columns to plot

library(ggplot2)
library(dplyr)

columns_to_plot <- c("Age", "Sex", "ALB", "ALP", "ALT", "AST", "BIL", "CHE", "CHOL", "CREA", "GGT")

# Create the plots
plot_list <- list()
for (col in columns_to_plot) {
  p <- ggplot(df, aes_string(x = col, fill = "factor(Category)")) +
    geom_histogram(aes(y = after_stat(density)), alpha = 0.6, position = 'identity') +
    geom_density(alpha = 0.2) +
    scale_fill_viridis_d() +
    labs(x = col, y = "Density", fill = "Category") +
    theme_minimal(base_size = 15)
  plot_list[[col]] <- p
}

# Arrange the plots in a grid
library(gridExtra)
do.call(grid.arrange, c(plot_list, ncol = 2))


################### check the relationship for other non-binary number

# Load necessary libraries
library(ggplot2)
library(dplyr)

# List of features to analyze

features <- c("Age", "Sex", "ALB", "ALP", "ALT", "AST", "BIL", "CHE", "CHOL", "CREA", "GGT", "PROT")
  


################## RElationship between variables and category column I want Density a diiferent color


for (feature in features) {
  p <- ggplot(hcv, aes(x = .data[[feature]], fill = as.factor(Category))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of", feature, "by Category"),
         x = feature,
         y = "Density") +
    theme_minimal() +
    scale_fill_manual(values = c("0" = "green", "1" = "pink"))
  
  print(p)
}



################## RElationship between variables and category column I want bar chart a diiferent color
########### change the 'y' to the variables of your choice


library(ggplot2)

# Create a bar plot in R
ggplot(hcv, aes(x = as.factor(Category), y = PROT, fill = as.factor(Category))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "plasma", name = "Category") +
  labs(
    title = "Category vs PROT",
    x = "Category",
    y = "PROT"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )



########################Build model

# Load required packages

library(dplyr)
library(caret)
library(data.table)



# Drop the 'Category' column for X
X <- hcv %>% select(-Category)

# Extract the 'Category' column for y

y <- hcv$Category

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Check the lengths of the training and testing sets
length(X_train)
length(X_test)
length(y_train)
length(y_test)

#######Random Forest model

# Load necessary libraries

library(randomForest)
library(caret)
library(ggplot2)
library(pROC)
library(e1071)

# Assuming X_train, y_train, X_test, y_test have been defined as before

# Train Random Forest Classifier
set.seed(42)
rfc <- randomForest(x = X_train, y = as.factor(y_train), ntree = 100)

# Predict on the test set
y_pred_rfc <- predict(rfc, X_test)

# Calculate accuracy
accuracy <- sum(y_pred_rfc == y_test) / length(y_test)
print(paste("Accuracy obtained by Random Forest Classifier:", accuracy * 100))

# Generate confusion matrix
cf_matrix <- table(Predicted = y_pred_rfc, Actual = y_test)

# Plot confusion matrix
cfm_plot <- as.data.frame(cf_matrix)
colnames(cfm_plot) <- c("Predicted", "Actual", "Freq")
ggplot(data = cfm_plot, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for Random Forest Classifier", x = "Predicted", y = "Actual") +
  theme_minimal()

# Generate classification report
classification_report <- confusionMatrix(as.factor(y_pred_rfc), as.factor(y_test))
print(classification_report)




##############Decision Tree Classifier model

# Load necessary libraries
library(gbm)
library(caret)
library(ggplot2)
library(dplyr)





install.packages("rpart")
install.packages("caret")
install.packages("pROC")
install.packages("pheatmap")

library(rpart)
library(caret)
library(pROC)
library(pheatmap)


# Example of splitting data (replace 'target' with your actual target variable name)
set.seed(123) # For reproducibility
splitIndex <- createDataPartition(hcv$Category, p = 0.8, list = FALSE)
train_data <- hcv[splitIndex, ]
test_data <- hcv[-splitIndex, ]

X_train <- train_data[, -which(names(train_data) == "Category")]
y_train <- train_data$Category
X_test <- test_data[, -which(names(test_data) == "Category")]
y_test <- test_data$Category



tree_model <- rpart(Category ~ ., data = train_data, method = "class")


y_pred_tree <- predict(tree_model, test_data, type = "class")

# Accuracy
accuracy <- sum(y_pred_tree == y_test) / length(y_test)
print(paste("Accuracy obtained by Decision Tree Classifier:", accuracy * 100))

# Confusion Matrix
confusion <- confusionMatrix(as.factor(y_pred_tree), as.factor(y_test))
print(confusion)

# Confusion Matrix Heatmap
cm <- confusion$table
pheatmap(cm, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         main = "Confusion Matrix for Decision Tree Classifier")

# Classification Report
print(confusion$byClass)


