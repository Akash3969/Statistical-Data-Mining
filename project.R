attach(Candidate_final)

c <- Candidate_final
c$candidate_zip <- NULL
c$order_zip <- NULL
c$candidate_city <- NULL
c$order_city <- NULL
c$avg_primary_skill_expmonths <- NULL
c$avg_secondary_skill_expmonths <- NULL
c$vendorcandidate <- NULL
c$suitableforvendorcandidates <- NULL
#----------------------------------------------------------------------------------
#checking for outliers


# Calculate Z-scores
z_scores <- scale(c$years_work_exp)

# Set a threshold for outlier detection (e.g., Z > 2 or Z < -2)
outlier_threshold <- 3

# Identify and remove outliers
outliers <- abs(z_scores) > outlier_threshold
cleaned_data <- c$years_work_exp[!outliers]

# Print the Z-scores and outliers
cat("Z-scores:\n")
print(z_scores)

cat("\nOutliers:\n")
print(which(outliers))

cat("\nCleaned Data:\n")
print(cleaned_data)
#----------------------------------------------------------------------------------------------
# Visualize the data
library(ggplot2)

ggplot(c, aes(x = total_offers_placements, y = job_order_count)) +
  geom_line() +
  labs(title = "Line plot", x = "number of job offers", y = "number job orders")

# Choose the number of clusters (k)
k <- 3

# Run k-means clustering
kmeans_result <- kmeans(c$yearsofworkexperience, centers = k, nstart = 25)

# Add cluster assignments to the original data
cluster <- as.factor(kmeans_result$cluster)

# Visualize the clustered data
ggplot(c, aes(x = total_offers_placements, y = job_order_count, color = cluster)) +
  geom_point() +
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = total_offers_placements, y = job_order_count, color = cluster), color = "black", size = 3, shape = 4) +
  ggtitle("K-Means Clustering Results")

# Display cluster centers
cat("Cluster Centers:\n")
print(kmeans_result$centers)
#-----------------------------------------------------------------------------------------

temp <- c[, c(51:54,64, 67:69)]
library(PerformanceAnalytics)
chart.Correlation(temp)










#---------------------------------------------------------------------------------------
#Preprocessing
#Replacing Annual salry empty rows based on average industry type
# Calculate the average salary by industrytype, excluding 0 values
average_salaries <- c %>%
  filter(annualsalary != 0) %>%
  group_by(industrytype) %>%
  summarize(avg_salary = mean(annualsalary))

# Merge the average salaries back into the original data
c <- c %>%
  left_join(average_salaries, by = "industrytype")

# Replace 0 values with the corresponding average salary
c <- c %>%
  mutate(annualsalary = ifelse(annualsalary == 0, avg_salary, annualsalary)) %>%
  select(-avg_salary)  # Remove the temporary column

c$annualsalary

#Taking records which are active and placed
library(dplyr)
c <- c %>%
  filter(candidate_statusid %in% c("Active", "Placed"))

#formating the dates columns
c$resumeupdated <- as.Date(c$resumeupdated)
c$activitydate <- as.Date(c$activitydate)
c$order_createdate <- as.Date(c$order_createdate)
c$order_updatedate <- as.Date(c$order_updatedate)
c1$candidate_updatedate <- as.Date(sub(" .*", "", c1$candidate_updatedate))

library(lubridate)
c1$candidate_updatedate <- mdy(c1$candidate_updatedate)
c1$candidate_createdate <- mdy(c1$candidate_createdate)
c1$order_statusdate <- mdy(c1$order_statusdate)


c1$update_days <- as.numeric(difftime(c1$candidate_updatedate, c1$candidate_createdate, units = "days"))


c1$candidate_updatedate <- format(c1$candidate_updatedate, format = "%m-%d-%Y")
c1$candidate_createdate <- format(c1$candidate_createdate, format = "%m-%d-%Y")
c1$order_createdate <- format(c1$order_createdate, format = "%m-%d-%Y")
c1$order_updatedate <- format(c1$order_updatedate, format = "%m-%d-%Y")
c1$order_statusdate <- format(c1$order_statusdate, format = "%m-%d-%Y")
c<- c1

#factors and relevels
c$candidate_statusid <- factor(c$candidate_statusid)
levels(c$candidate_statusid)
table(c$candidate_statusid)


c$candidate_statusid <- factor(c$candidate_statusid) 
levels(c$candidate_statusid)
table(c$candidate_statusid)

c$stagename <- factor(c$stagename)
levels(c$stagename)
table(c$stagename)
c$stagename <- relevel(c$stagename,"Offer")

c$industrysectorname <- factor(c$industrysectorname)
levels(c$industrysectorname)

colSums(is.na(c))
c <- c[complete.cases(c$annualsalary), ]
c$total_offers_placements
c <- c[complete.cases(c$total_offers_placements), ]
c1 <- c1[complete.cases(c1$years_work_exp), ]

#--------------------------------------------------------------------------------
#visualization
hist(log(c$total_offers_placements))
hist(c$monthsofworkexperience)
hist(c$job_order_count)


library(ggplot2)

ggplot(c, aes(x = total_offers_placements, y = job_order_count)) +
  geom_line() +
  labs(title = "Line plot", x = "number of job offers", y = "number job orders")




#-----------------------------------------------------------------------------------------
#basic modelling
m1 <- lm(total_offers_placements ~ years_work_exp + bachelor_deg + job_order_count + primary_skills, data = c)

library(lme4)
m2 <- glmer(total_offers_placements ~ (1 |industrysectorname) + years_work_exp + bachelor_deg + job_order_count + primary_skills, data = c, family = poisson(link = "log"))
m3 <- glmer(total_offers_placements ~ (1 |ordertypename) + years_work_exp + master_deg + total_tech_jobs + primary_skills + desiredsalary + maxsubmissions, data = c, family = poisson(link = "log"))
m4 <- glmer(total_offers_placements ~ (1 |jobpriorityname) + years_work_exp + master_deg + total_tech_jobs + primary_skills + desiredsalary + maxsubmissions, data = c, family = poisson(link = "log"))

m5 <- glm(offer_placement ~ years_work_exp + master_deg + total_tech_jobs + primary_skills + desiredsalary + maxsubmissions, data = c, family = quasipoisson(link = "log"))
m6 <- glm(offer_placement ~ years_work_exp + master_deg + total_tech_jobs + primary_skills + desiredsalary + maxsubmissions, data = c, family = poisson(link = "log"))
summary(m6)

logit <- glm(offer_placement ~ years_work_exp + master_deg + total_tech_jobs + primary_skills + desiredsalary + maxsubmissions,family=binomial (link="logit"), data=c)
probit <- glm(offer_placement ~ years_work_exp + master_deg + total_tech_jobs + primary_skills + desiredsalary + maxsubmissions,family=binomial (link="probit"), data=c)



library(stargazer)
stargazer(logit,probit, type = "text", single.row = TRUE)
stargazer(m5,m6, type = "text", single.row = TRUE)

#----------------------------------------------------------------------------------
#survival analysis

# Filter rows where offer_placement is equal to 1
c1 <- c1[c1$offer_placement == 1, ]
hist(c1$job_order_count)
#relevelling job order counts for simpliying
# If you want to include 5 in the lower group (0-5), start the breaks at -1
breaks <- seq(-1, 85, by = 5)

# Use cut with right = TRUE
c1$job_bins <- as.numeric(cut(c1$job_order_count, breaks = breaks, labels = FALSE, right = TRUE))
c1$job_bins

library(survival)
y <- Surv(c1$update_days, c1$offer_placement)
km <- survfit(y ~ years_work_exp + job_order_count, data=c1)
km1 <- survfit(y ~ job_bins, data=c1)

summary(km1)

library(survminer)
ggsurvplot(fit=km1, xlab="years + order count", ylab="Probability", data = c1)


#' Cox proportional hazard model 
cox <- coxph(y ~ years_work_exp + bachelor_deg + job_order_count + primary_skills, data=c1, method="breslow")
summary(cox)

#' Exponential, Weibull, and log-logistic parametric models
exp <- survreg(y ~ years_work_exp + bachelor_deg + job_order_count + primary_skills, data=c1, dist="exponential")

weibull <- survreg(y ~ years_work_exp + master_deg + tech_major_deg + total_tech_jobs+ openings+ tempordirect + industrysectorname + jobpriorityname + primary_skills, data=c1, dist="weibull")

loglogistic <- survreg(y ~ yrblt + tileroof + sqft + listprice, data=c1, dist="loglogistic")

library(stargazer)
stargazer(cox, exp,weibull, type="text", single.row=TRUE)

#--------------------------------------------------------------------------------------------------------------------
c2 <- c
c2 <- c %>%
  mutate(placement = ifelse(categoryname == "Placement / Assigned (System)", 1, 0))
c2$placement
colSums(is.na(c2))
c2 <- c2[complete.cases(c2$jobpriorityname),]

set.seed(3969)
 trainIndex = sample(1:nrow(c2), size=round(0.75*nrow(c2)), replace=FALSE)
 train <- c2[trainIndex,]
 test <- c2[-trainIndex,]

 library(lme4)
 g1 <- glmer(placement ~ (1 | jobpriorityname) + yearsofworkexperience, data = train, family = poisson(link = "log"))
 
 predictions <- predict(g1, newdata = test, type = "response")

 threshold <- 0.04
 binary_predictions <- ifelse(predictions >= threshold, 1, 0)

 
 binary_predictions <- factor(binary_predictions, levels = c(0, 1))
 
 # Convert test$placement to a factor with levels 0 and 1
 test$placement <- factor(test$placement, levels = c(0, 1))
 
 library(caret)
 confusion <- confusionMatrix(binary_predictions, test$placement) 

 print(confusion) 
#----------------------------------------------------------------------------------------------------------
 
 library(lme4)
 g2 <- glmer(placement ~ (1 | jobpriorityname) + yearsofworkexperience + job_order_count + tempordirect + yearsofmanagementexperience, data = train, family = poisson(link = "log"))
 
 predictions1 <- predict(g2, newdata = test, type = "response")
 
 threshold <- 0.04
 binary_predictions1 <- ifelse(predictions1 >= threshold, 1, 0)
 
 
 binary_predictions1 <- factor(binary_predictions1, levels = c(0, 1))
 
 # Convert test$placement to a factor with levels 0 and 1
 test$placement <- factor(test$placement, levels = c(0, 1))
 
 library(caret)
 confusion <- confusionMatrix(binary_predictions1, test$placement) 
 
 print(confusion) 
 #install.packages("yardstick")
 library(yardstick)
 # Extract components of the confusion matrix
 TP <- confusion$table[2, 2]  # True Positives
 TN <- confusion$table[1, 1]  # True Negatives
 FP <- confusion$table[1, 2]  # False Positives
 FN <- confusion$table[2, 1]  # False Negatives
 
 # Calculate precision, accuracy, recall, and F1 score
 precision_score <- TP / (TP + FP)
 accuracy_score <- (TP + TN) / (TP + TN + FP + FN)
 recall_score <- TP / (TP + FN)
 f1_score <- 2 * (precision_score * recall_score) / (precision_score + recall_score)
 
 # Print the metrics
 cat("Precision:", precision_score, "\n")
 cat("Accuracy:", accuracy_score, "\n")
 cat("Recall:", recall_score, "\n")
 cat("F1 Score:", f1_score, "\n")
 
#-----------------------------------------------------------------------------------------
#install.packages("forecast")
 library(forecast)
 library(dplyr)
 library(lubridate)
 
 # Aggregate the data to a monthly level for forecasting
 monthly_hires <- c1 %>%
   mutate(candidate_createdate = as.Date(candidate_createdate)) %>%  # Convert to date
   group_by(month = floor_date(candidate_createdate, "month")) %>%
   summarize(total_hires = sum(candidate_statusid == 'Placed', na.rm = TRUE))
 
 
 start_year <- year(min(monthly_hires$month, na.rm = TRUE))
 start_month <- month(min(monthly_hires$month, na.rm = TRUE))
 
 ts_data <- ts(monthly_hires$total_hires, start = c(start_year, start_month), frequency = 12)
 
 # Fit an ARIMA model
 fit <- auto.arima(ts_data)
 
 forecasted_values <- forecast(fit, h = 12)
 forecasted_values
 
 print(forecasted_values)
 
 plot(forecasted_values)

 
 
 
 
  
