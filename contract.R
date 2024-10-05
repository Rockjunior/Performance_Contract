# Load necessary library
library(dplyr)

# Sample data
data <- data.frame(
  Employee_ID = rep(c('E001', 'E002', 'E003', 'E004', 'E005'), each = 10),
  Month = rep(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'), 5),
  Monthly_Revenue = c(105000, 98000, 120000, 93000, 110000, 102000, 99000, 118000, 95000, 112000,
                      108000, 97000, 121000, 92000, 115000, 107000, 96000, 119000, 94000, 113000,
                      104000, 95000, 122000, 91000, 116000, 106000, 94000, 123000, 93000, 117000,
                      109000, 96000, 124000, 92000, 114000, 105000, 97000, 125000, 94000, 118000,
                      107000, 95000, 121000, 93000, 116000, 106000, 96000, 122000, 92000, 115000),
  New_Clients = c(12, 8, 15, 9, 11, 10, 7, 13, 10, 12,
                  11, 9, 14, 8, 13, 12, 8, 16, 9, 14,
                  10, 9, 15, 8, 13, 11, 8, 14, 7, 12,
                  13, 9, 16, 8, 14, 12, 7, 15, 10, 13,
                  11, 9, 14, 8, 12, 10, 8, 13, 7, 11),
  Satisfaction_Score = c(87, 82, 90, 85, 88, 86, 80, 91, 83, 89,
                         84, 78, 92, 82, 90, 85, 79, 89, 84, 87,
                         88, 81, 90, 86, 91, 89, 82, 93, 85, 88,
                         87, 83, 91, 80, 89, 86, 81, 92, 87, 90,
                         85, 84, 90, 83, 89, 88, 82, 91, 80, 87)
)

# Define weights
weights <- c(Revenue = 0.50, Clients = 0.30, Satisfaction = 0.20)

# Normalize the data
data <- data %>%
  mutate(
    Norm_Revenue = (Monthly_Revenue - min(Monthly_Revenue)) / (max(Monthly_Revenue) - min(Monthly_Revenue)),
    Norm_Clients = (New_Clients - min(New_Clients)) / (max(New_Clients) - min(New_Clients)),
    Norm_Satisfaction = (Satisfaction_Score - min(Satisfaction_Score)) / (max(Satisfaction_Score) - min(Satisfaction_Score))
  )

# Calculate total score
data <- data %>%
  mutate(
    Total_Score = (Norm_Revenue * weights["Revenue"]) +
                  (Norm_Clients * weights["Clients"]) +
                  (Norm_Satisfaction * weights["Satisfaction"])
  )

# Define grading function
assign_grade <- function(score) {
  if (score >= 0.90) {
    return('A')
  } else if (score >= 0.80) {
    return('B')
  } else if (score >= 0.70) {
    return('C')
  } else if (score >= 0.60) {
    return('D')
  } else {
    return('F')
  }
}

# Assign grades based on total score
data <- data %>%
  mutate(Grade = sapply(Total_Score, assign_grade))

# Print the first few rows of the data
print(head(data, 10))

