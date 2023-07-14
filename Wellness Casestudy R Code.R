# Load libraries
library(tidyverse)
library(openxlsx)

# Read data from files
activity <- read_csv("~/Desktop/Wellness Casestudy/dailyActivity_merged.csv")
sleep <- read_csv("~/Desktop/Wellness Casestudy/sleepDay_merged.csv")
weight <- read_csv("~/Desktop/Wellness Casestudy/weightLogInfo_merged.csv")

# Convert Id to character data type
# Convert Day to date format
# Rename various dates to Day
activity <- activity %>%
  mutate(across(Id, as.character),
         across(ActivityDate, as.Date, format = "%m/%d/%y"),
         Day = ActivityDate)

sleep <- sleep %>%
  mutate(across(Id, as.character),
         across(SleepDay, as.Date, format = "%m/%d/%y"),
         Day = SleepDay)

weight <- weight %>%
  mutate(across(c(Id, LogId), as.character),
         across(Date, as.Date, format = "%m/%d/%y"),
         Day = Date)

# Combine data frames and add day of the week
combined_data <- sleep %>%
  right_join(activity, by = c("Id", "Day")) %>%
  left_join(weight, by = c("Id", "Day")) %>%
  mutate(Weekday = weekdays(as.Date(Day, "m/%d/%Y")))

# Find and remove duplicate rows; count NAs and distinct Ids
combined_data <- combined_data[!duplicated(combined_data), ]
sum(is.na(combined_data))
n_distinct(combined_data$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# Order the days of the week
combined_data$Weekday <- factor(combined_data$Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Select summary statistics
summary_stats <- combined_data %>%
  select(TotalMinutesAsleep, TotalSteps, TotalDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories, WeightKg, Fat, BMI, IsManualReport) %>%
  summary()

# Total steps by day
steps_plot <- ggplot(data = combined_data, aes(x = Weekday, y = TotalSteps)) +
  geom_bar(stat = "identity", fill = "#fa8072") +
  labs(title = "Steps by Day", y = "Total Steps")

# Minutes of moderate activity per day
activity_plot <- ggplot(data = combined_data, aes(x = Weekday, y = FairlyActiveMinutes)) +
  geom_bar(stat = "identity", fill = "#fa8072") +
  labs(title = "Fairly Active Minutes by Day", y = "Minutes")

# Logged Activities Distance
distance_plot <- ggplot(data = combined_data, aes(x = Weekday, y = LoggedActivitiesDistance)) +
  geom_bar(stat = "identity", fill = "#fa8072") +
  labs(title = "Logged Activity Distance by Day", y = "Logged Activity Distance")

# Distribution of sleep time
sleep_plot <- ggplot(combined_data, aes(TotalMinutesAsleep)) +
  geom_histogram(bins = 10, na.rm = TRUE, color = "#000000", fill = "#fa8072") +
  labs(title = "Distribution of Total Time Asleep", x = "Total Time Asleep (minutes)")

# Total minutes Asleep vs Calories
calories_plot <- ggplot(combined_data) +
  geom_point(mapping = aes(x = TotalMinutesAsleep / 60, y = Calories), na.rm = TRUE, color = "#fa8072") +
  labs(title = "Calories vs Time Slept", x = "Time Asleep (Hours)", y = "Calories")

# Display the plots
print(steps_plot)
print(activity_plot)
print(distance_plot)
print(sleep_plot)
print(calories_plot)