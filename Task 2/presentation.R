library(dplyr)
library(ggplot2)
library(readxl)

transactions <- read_xlsx("KPMG_VI_New_raw_data_update_final.xlsx", sheet = 2, skip=1)
demographic <- read_xlsx("KPMG_VI_New_raw_data_update_final.xlsx", sheet = 4, skip=1)
address <- read_xlsx("KPMG_VI_New_raw_data_update_final.xlsx", sheet = 5, skip=1)
newcustomers <- read_xlsx("KPMG_VI_New_raw_data_update_final.xlsx", sheet = 3, skip=1)


#calculate age
demographic$year <- substr(demographic$DOB, start = 1, stop = 4)
demographic$year[demographic$year>2020 | demographic$year<1900] <- NA
demographic$age <- 2020-as.numeric(demographic$year)

newcustomers$year <- substr(newcustomers$DOB, start = 1, stop = 4)
newcustomers$year[newcustomers$year>2020 | newcustomers$year<1900] <- NA
newcustomers$age <- 2020-as.

#calculate profit and aggregate by customer 
transactions$profit <- transactions$list_price - transactions$standard_cost
trans_agg <- aggregate(profit ~ customer_id, data = transactions, FUN=sum)

df <- merge(trans_agg, demographic)
df <- merge(df, address)

#recode as factors
df$state <- as.factor(df$state)
df$gender <- as.factor(df$gender)
df$postcode <- as.factor(df$postcode)
df$wealth_segment <- as.factor(df$wealth_segment)
df$owns_car <- as.factor(df$owns_car)
df$job_industry_category <- as.factor(df$job_industry_category)
df$job_title <- as.factor(df$job_title)

#exploratory analyses
range(df$profit)
hist(df$profit, breaks=10, main = "Profit per Customer", xlab = "Profit Margin")
abline(v=quantile(df$profit, probs = .9), col="blue")
text(x=quantile(df$profit, probs = .9), y=800, labels = "90th percentile")


#remove unique factor levels from postcode b/c no variance
unique_codes <- c("2028", "2128", "2292", "2454", "2463", "2546", "2582", "2714", "2773", "2779", "2783", "2821", "2869", "3033", "3036", "3044", "3093", "3114", 
                  "3125", "3126", "3128", "3555", "3585", "3644", "3803", "4068", "4106", "4110", "4660", "4799")
df_new <- df[df$postcode %in% unique_codes == F,]
#model
model <- lm(profit ~ state + postcode + property_valuation + gender + past_3_years_bike_related_purchases + wealth_segment + tenure + owns_car + job_industry_category + job_title, data = df_new)
summary(model)
plot(model)
#prediction
pred <- predict(model, df_new)
1 - sum((pred - df_new$profit)^2, na.rm = T)/sum((df_new$profit - mean(df_new$profit, na.rm = T))^2, na.rm = T) # R² = 44%

df_p <- cbind(df_new, pred)
pl <- ggplot(df_p, aes(x=profit , y=pred))
pl + geom_point(shape=1) + geom_smooth() + labs(title="Training Data, R² = .44") + xlab("Actual Profit") + ylab("Predicted Profit")


#predict new customer profit

#recode as factors
newcustomers$state <- as.factor(newcustomers$state)
newcustomers$gender <- as.factor(newcustomers$gender)
newcustomers$postcode <- as.factor(newcustomers$postcode)
newcustomers$wealth_segment <- as.factor(newcustomers$wealth_segment)
newcustomers$owns_car <- as.factor(newcustomers$owns_car)
newcustomers$job_industry_category <- as.factor(newcustomers$job_industry_category)
newcustomers$job_title <- as.factor(newcustomers$job_title)
newcustomers$property_valuation <- as.numeric(newcustomers$property_valuation)
newcustomers$past_3_years_bike_related_purchases <- as.numeric(newcustomers$past_3_years_bike_related_purchases)

unique_codes2 <- as.character(c(2024, 2028, 2082, 2103, 2128, 2140, 2257, 2258, 2292, 2344, 2390, 2420, 2422, 2429, 2454, 2456, 2479, 2546, 2574, 2582, 2643, 2720, 2782, 2783, 3047, 3078, 3167, 3200, 3206, 3235, 3277, 3335, 3579, 3585, 3687, 3741, 3753, 3758, 3759, 3777, 3782, 3803, 3814, 3816, 3842, 3851, 3858, 3860, 3919, 3981, 4000, 4021, 4036, 4065, 4068, 4129, 4158, 4172, 4183, 4307, 4342, 4408, 4516, 4520, 4561, 4575, 4600, 4605, 4659, 4703, 4710, 4717, 4852))
newcustomers <- newcustomers[newcustomers$postcode %in% unique_codes2 == F,]



pred_new <- predict(model, newcustomers)
summary(pred_new)

hist(pred_new, breaks=10, main = "Profit per Customer", xlab = "Predicted Profit Margin")
abline(v=quantile(pred_new, probs = .9, na.rm = T), col="blue")
text(x=quantile(pred_new, probs = .9, na.rm = T), y=280, labels = "90th percentile")


which.max(pred_new)
newcustomers[424,]

out <- cbind(newcustomers, pred_new)
