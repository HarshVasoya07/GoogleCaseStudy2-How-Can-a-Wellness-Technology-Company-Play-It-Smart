<h1>[Google] Case Study 2: How Can a Wellness Technology Company Play It Smart?</h1>

<h2>Business Objective/Task</h2>
The business objective of this case study is to analyze smart device usage data to gain insights into how consumers are using non-Bellabeat smart devices. The analysis aims to identify trends in smart device usage, understand how these trends can apply to Bellabeat customers, and determine how these insights can influence Bellabeat's marketing strategy. The ultimate goal is to leverage the analysis findings to unlock new growth opportunities for Bellabeat and make informed marketing decisions.

<br />
<h2>Ask</h2>

- As a junior data analyst at Bellabeat, my task is to analyze smart device usage data and gain insights into how consumers are using non-Bellabeat smart devices. This analysis will help us understand the trends in smart device usage, how these trends apply to Bellabeat customers, and how they can influence Bellabeat's marketing strategy.
<br />

<h2>Prepare</h2>

- To conduct the analysis, I will utilize the FitBit Fitness Tracker Data, a publicly available dataset from Kaggle. This dataset contains personal fitness tracker data from thirty Fitbit users, including information on daily activity, steps, heart rate, and sleep monitoring. I will download and store the data appropriately for analysis.

```r
# Load libraries
library(tidyverse)
library(openxlsx)

# Read data from files
activity <- read_csv("~/Desktop/Wellness Casestudy/dailyActivity_merged.csv")
sleep <- read_csv("~/Desktop/Wellness Casestudy/sleepDay_merged.csv")
weight <- read_csv("~/Desktop/Wellness Casestudy/weightLogInfo_merged.csv")

```
  
<br />

<h2>Process</h2>

- Once the data is obtained, I will check its organization and format. I will ensure that it is in a suitable format for analysis, such as a structured format with relevant columns and consistent data types. I will also address any issues of bias, credibility, or data quality that may arise from using the FitBit dataset. Additionally, I will verify the data's integrity and document any cleaning or manipulation steps taken.

```r
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

```
<br />

<h2>Analyze</h2>

- In the analysis phase, I will aggregate and format the data for easy accessibility. I will perform calculations and statistical analysis to identify trends and relationships in smart device usage. This may involve exploring metrics such as daily activity levels, steps taken, heart rate patterns, and sleep duration. By analyzing these trends, I will extract insights and draw conclusions about how consumers are using smart devices.

```r
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
```


<br />

<h2>Share</h2>

- To effectively communicate the findings, I will create supporting visualizations using tools such as ggplot or Tableau. These visualizations will highlight the key trends and insights discovered in the analysis. I will ensure that the visualizations are clear, concise, and visually appealing. I will also prepare a summary of the analysis, including the main findings and their implications for Bellabeat's marketing strategy.
```r
#visualizations 
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
```
<br />

<h2>Act</h2>

- Based on the analysis, I will provide high-level recommendations for Bellabeat's marketing strategy. These recommendations will be rooted in the insights gained from the smart device usage data. I will propose actionable steps that Bellabeat can take to leverage the identified trends and align their marketing efforts with the evolving needs and preferences of consumers. Additionally, I will consider the possibility of incorporating additional data sources to further enhance the analysis and recommendations.

- In conclusion, by following the ask, prepare, process, analyze, share, and act framework, I will conduct a comprehensive analysis of smart device usage data to inform Bellabeat's marketing strategy. The insights and recommendations derived from this analysis will help Bellabeat capitalize on emerging trends and better serve their customers in the wellness technology market.


<br />


<h2>Recommendations for Business Growth</h2>

- <b>Explore partnerships and collaborations with complementary brands or influencers to expand reach and tap into new markets.</b>
- <b>Leverage customer testimonials and success stories to establish trust and credibility among potential customers.</b> 
- <b>Enhance social media presence and user engagement through engaging content, user-generated content campaigns, challenges, and partnerships with influencers.</b>
- <b>Expand marketing efforts towards stress management, creating content and features within the app that specifically address stress reduction and mindfulness.</b> 
<h2>Languages and Utilities Used</h2>

- <b>R</b> 

<h2>Environments Used </h2>

- <b>RStudio, Powerpoint, Tableau</b> 

